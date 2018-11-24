{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Handler.Add where

import Import
import Database.Persist.Sql
import Data.List (nub)
import Data.Function ((&))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Types.Status as NH
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
  (userId, user) <- requireAuthPair
  time <- liftIO getCurrentTime
  let bm = _toBookmark userId time bookmarkForm
  when (shouldArchive user bm) $
    void $ async (archiveBookmark bm)
  runDB (upsertBookmark kbid bm tags)
  where
    kbid = toSqlKey <$> _bid bookmarkForm
    tags = maybe [] (nub . words) (_tags bookmarkForm)

shouldArchive :: User -> Bookmark -> Bool
shouldArchive _ bm =
  (bookmarkShared bm) &&
  not (isBlacklisted bm)
  -- && isArchiveEnabled

isBlacklisted :: Bookmark -> Bool 
isBlacklisted (Bookmark {..}) =
  "youtube" `isInfixOf` bookmarkHref

archiveBookmark :: Bookmark -> Handler ()
archiveBookmark bm = do
  fetchArchiveSubmitId >>= \case
    Left e -> do
      cpprint e
      pure ()
    Right submitId ->  do
      cpprint submitId
      forM_ (buildArchiveRequest submitId (unpack (bookmarkHref bm))) $ \req -> do
        res <- liftIO $ NH.httpLbs req =<< NH.getGlobalManager
        cpprint res
        case NH.responseStatus res of
          s | s == NH.status200 ->
            forM_ (lookup "Refresh" (NH.responseHeaders res)) $ \h ->
              forM_ (parseRefreshHeaderUrl h) (updateBookmarkArchiveUrl bm)
          s | s == NH.status302 ->
            forM_ (lookup "Location" (NH.responseHeaders res)) (updateBookmarkArchiveUrl bm)
          _ -> pure ()

parseRefreshHeaderUrl :: ByteString -> Maybe ByteString
parseRefreshHeaderUrl h = do
  let u = BS8.drop 1 $ BS8.dropWhile (/= '=') h
  if (not (null u))
    then Just u
    else Nothing

updateBookmarkArchiveUrl :: Bookmark -> ByteString -> Handler ()
updateBookmarkArchiveUrl bm archiveUrl = do
  cpprint archiveUrl
  pure ()

buildArchiveRequest :: String -> String -> Maybe NH.Request
buildArchiveRequest submitId href =
  NH.parseRequest "POST http://archive.is/submit/" <&> \r ->
    r { NH.requestHeaders =
        [ ("User-Agent", archiveUserAgent)
        , ("Content-Type", "application/x-www-form-urlencoded")
        ]
      , NH.requestBody = NH.RequestBodyLBS $ WH.urlEncodeAsForm ((
        [ ("submitid" , submitId)
        , ("url", href)
        ]) :: [(String, String)])
      , NH.redirectCount = 0
      }

fetchArchiveSubmitId :: Handler (Either String String)
fetchArchiveSubmitId = do
  res <- liftIO $ NH.httpLbs buildSubmitRequest =<< NH.getGlobalManager
  pure $ parseSubmitId (LBS.toStrict (responseBody res))
  where
    buildSubmitRequest =
      NH.parseRequest_ "https://archive.is/" & \r ->
        r {NH.requestHeaders = [("User-Agent", archiveUserAgent)]}

archiveUserAgent :: ByteString
archiveUserAgent = "espial"

parseSubmitId :: BS.ByteString -> Either String String
parseSubmitId res = do
  (flip AP.parseOnly) res $ do
    skipAnyTill (AP.string "submitid\" value=\"")
    AP.many1 (AP.notChar '"')
  where
    skipAnyTill end = go where go = end *> pure () <|> AP.anyChar *> go
