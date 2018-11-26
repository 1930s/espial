module Handler.Add where

import Import
import Handler.Archive
import Database.Persist.Sql
import Data.List (nub)

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
bookmarkFormUrl = do
  Entity _ user <- requireAuth
  BookmarkForm
    <$> (lookupGetParam "url" >>= pure . fromMaybe "")
    <*> (lookupGetParam "title")
    <*> (lookupGetParam "description" >>= pure . fmap Textarea)
    <*> (lookupGetParam "tags")
    <*> ((lookupGetParam "private" >>= pure . fmap parseChk) <&> (<|> userPrivateDefault user))
    <*> (lookupGetParam "toread" >>= pure . fmap parseChk)
    <*> pure Nothing
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
  (res, kbid) <- runDB (upsertBookmark mkbid bm tags)
  whenM (shouldArchiveBookmark user kbid) $
    void $ async (archiveBookmarkUrl kbid (unpack (bookmarkHref bm)))
  pure (res, kbid)
  where
    mkbid = toSqlKey <$> _bid bookmarkForm
    tags = maybe [] (nub . words) (_tags bookmarkForm)
