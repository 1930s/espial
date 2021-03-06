{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Handler.User where

import Import
import Handler.Common (lookupPagingParams)

getUserSettingsR :: UserNameP -> Handler Html
getUserSettingsR uname@(UserNameP name) = do
  (userId, user) <- requireAuthPair
  let userSettingsEl = "userSettings" :: Text
  let userSettings = toUserSettingsForm user
  defaultLayout $ do
    $(widgetFile "user-settings")
    toWidgetBody [julius|
        app.userR = "@{UserR uname}";
        app.dat.userSettings = #{ toJSON userSettings } || []; 
    |]
    toWidget [julius|
      PS['User'].renderUserSettings('##{rawJS userSettingsEl}')(app.dat.userSettings)();
    |]

getUserR :: UserNameP -> Handler Html
getUserR uname@(UserNameP name) = do
  _getUser uname SharedAll FilterAll (TagsP [])

getUserSharedR :: UserNameP -> SharedP -> Handler Html
getUserSharedR uname sharedp =
  _getUser uname sharedp FilterAll (TagsP [])

getUserFilterR :: UserNameP -> FilterP -> Handler Html
getUserFilterR uname filterp =
  _getUser uname SharedAll filterp (TagsP [])

getUserTagsR :: UserNameP -> TagsP -> Handler Html
getUserTagsR uname pathtags =
  _getUser uname SharedAll FilterAll pathtags

_getUser :: UserNameP -> SharedP -> FilterP -> TagsP -> Handler Html
_getUser unamep@(UserNameP uname) sharedp' filterp' (TagsP pathtags) = do
  mauthuname <- maybeAuthUsername
  (limit', page') <- lookupPagingParams
  let limit = maybe 120 fromIntegral limit'
      page  = maybe 1   fromIntegral page'
      isowner = maybe False (== uname) mauthuname
      sharedp = if isowner then sharedp' else SharedPublic
      filterp = case filterp' of
        FilterSingle _ -> filterp'
        _ -> if isowner then filterp' else FilterAll
      isAll = filterp == FilterAll && sharedp == SharedAll && pathtags == []
  (bcount, bmarks, alltags) <-
    runDB $
    do Entity userId _ <- getBy404 (UniqueUserName uname)
       (cnt, bm) <- bookmarksQuery userId sharedp filterp pathtags limit page
       tg <- tagsQuery bm
       pure (cnt, bm, tg)
  mroute <- getCurrentRoute 
  let pager = $(widgetFile "pager")
  let renderEl = "bookmarks" :: Text
  req <- getRequest
  defaultLayout $ do
    $(widgetFile "user")
    toWidgetBody [julius|
        app.dat.bmarks = #{ toJSON $ toBookmarkFormList bmarks alltags } || []; 
        app.dat.isowner = #{ isowner };
        app.userR = "@{UserR unamep}";
    |]
    toWidget [julius|
      PS['User'].renderBookmarks('##{rawJS renderEl}')(app.dat.bmarks)();
    |]
  where
