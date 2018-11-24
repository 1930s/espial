{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import
import Database.Persist.Sql
import Data.List (nub)
import Data.Function ((&))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Web.FormUrlEncoded as WH

-- View

getAddViewR :: Handler Html
getAddViewR = do
  userId <- requireAuthId

  murl <- lookupGetParam "url"
  mformdb <- runDB (pure . fmap _toBookmarkForm =<< fetchBookmarkByUrl userId murl)
  formurl <- bookmarkFormUrl

  let renderEl = "addForm" :: Text

  popupLayout $ do
    toWidget [whamlet|
      <div id="#{ renderEl }">
    |]
    toWidgetBody [julius|
      app.dat.bmark = #{ toJSON (fromMaybe formurl mformdb) }; 
    |]
    toWidget [julius|
      PS['User'].renderAddForm('##{rawJS renderEl}')(app.dat.bmark)();
    |]

bookmarkFormUrl :: Handler BookmarkForm
bookmarkFormUrl =
  BookmarkForm
    <$> (lookupGetParam "url" >>= pure . fromMaybe "")
    <*> (lookupGetParam "title")
    <*> (lookupGetParam "description" >>= pure . fmap Textarea)
    <*> (lookupGetParam "tags")
    <*> (lookupGetParam "private" >>= pure . fmap parseChk)
    <*> (lookupGetParam "toread" >>= pure . fmap parseChk)
    <*> pure Nothing
    <*> pure Nothing
    <*> pure Nothing
  where
    parseChk s = s == "yes" || s == "on"

-- API

postAddR :: Handler ()
postAddR = do
  bookmarkForm <- requireCheckJsonBody
  _handleFormSuccess bookmarkForm >>= \case
    (Created, bid) -> sendStatusJSON created201 bid
    (Updated, _) -> sendResponseStatus noContent204 ()

_handleFormSuccess :: BookmarkForm -> Handler (UpsertResult, Key Bookmark)
_handleFormSuccess bookmarkForm = do
  cpprint bookmarkForm
  userId <- requireAuthId
  time <- liftIO getCurrentTime
  let bm = _toBookmark userId time bookmarkForm
  if (not ("youtube" `isInfixOf` bookmarkHref bm))
    then void $ async (queueArchiveBookmark bm)
    else pure ()
  runDB (upsertBookmark kbid bm tags)
  where
    kbid = toSqlKey <$> _bid bookmarkForm
    tags = maybe [] (nub . words) (_tags bookmarkForm)

queueArchiveBookmark :: Bookmark -> Handler ()
queueArchiveBookmark (Bookmark {..}) = do
  fetchArchiveSubmitId >>= \case
    Left e -> do
      cpprint e
      pure ()
    Right submitId ->  do
      cpprint submitId
      manager <- liftIO NH.getGlobalManager
      let req = NH.parseRequest_ "POST http://archive.is/submit/" & (\r -> r
              { NH.requestHeaders =
                [ ("User-Agent", "espial")
                , ("Content-Type", "application/x-www-form-urlencoded")
                ]
              , NH.requestBody = NH.RequestBodyLBS $ WH.urlEncodeAsForm ((
                [ ("submitid" , submitId)
                , ("url", unpack bookmarkHref)
                ]) :: [(String, String)])
              , NH.redirectCount = 0
              })
      cpprint req
      res <- liftIO $ NH.httpLbs req manager 
      cpprint res
      pure ()

fetchArchiveSubmitId :: Handler (Either String String)
fetchArchiveSubmitId = do
  manager <- liftIO NH.getGlobalManager
  let req = NH.parseRequest_ "https://archive.is/" & (\r -> r { NH.requestHeaders = [("User-Agent", "espial")] })
  res <- liftIO $ NH.httpLbs req manager 
  pure $ parseSubmitId (LBS.toStrict (responseBody res))

parseSubmitId :: BS.ByteString -> Either String String
parseSubmitId res = do
  (flip AP.parseOnly) res $ do
    skipAnyTill (AP.string "submitid\" value=\"")
    AP.many1 (AP.notChar '"')
  where
    skipAnyTill end = go where go = end *> pure () <|> AP.anyChar *> go
