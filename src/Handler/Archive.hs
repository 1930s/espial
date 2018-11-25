module Handler.Archive where

import Import
import Data.Function ((&))
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Types.Status as NH
import qualified Web.FormUrlEncoded as WH
import qualified Control.Monad.Metrics as MM

shouldArchiveBookmark :: User -> Key Bookmark -> Handler Bool
shouldArchiveBookmark _ kbid = do
  runDB (get kbid) >>= \case
    Nothing -> pure False
    Just bm -> do
      pure $
        (isNothing $ bookmarkArchiveHref bm) &&
        (bookmarkShared bm)
        && not (_isArchiveBlacklisted bm)
     -- && isArchiveEnabled

archiveBookmarkUrl :: Key Bookmark -> String -> Handler ()
archiveBookmarkUrl kbid url = do
  _fetchArchiveSubmitId >>= \case
    Left e -> do
      MM.increment "archive.fetchSubmitId_noparse"
      $(logError) (pack e)
    Right submitId ->  do
        userId <- requireAuthId
        let req = _buildArchiveSubmitRequest submitId url
        MM.increment "archive.submit"  
        res <- liftIO $ NH.httpLbs req  =<< NH.getGlobalManager
        let status = NH.responseStatus res
        MM.increment ("archive.submit_status_" <> (pack.show) (NH.statusCode status)) 
        case status of
          s | s == NH.status200 -> do
            forM_ (lookup "Refresh" (NH.responseHeaders res)) $ \h ->
              forM_ (_parseRefreshHeaderUrl h) (runDB . updateBookmarkArchiveUrl userId kbid . Just)
          s | s == NH.status302 -> do
            forM_
              (lookup "Location" (NH.responseHeaders res))
              (runDB . updateBookmarkArchiveUrl userId kbid . Just . decodeUtf8)
          _ -> do
            $(logError) (pack (show res))

_isArchiveBlacklisted :: Bookmark -> Bool 
_isArchiveBlacklisted (Bookmark {..}) =
  "youtube" `isInfixOf` bookmarkHref

_parseRefreshHeaderUrl :: ByteString -> Maybe Text
_parseRefreshHeaderUrl h = do
  let u = BS8.drop 1 $ BS8.dropWhile (/= '=') h
  if (not (null u))
    then Just $ decodeUtf8 u
    else Nothing

_buildArchiveSubmitRequest :: String -> String -> NH.Request
_buildArchiveSubmitRequest submitId href =
  NH.parseRequest_ "POST http://archive.is/submit/" & \r ->
    r { NH.requestHeaders =
        [ ("User-Agent", _archiveUserAgent)
        , ("Content-Type", "application/x-www-form-urlencoded")
        ]
      , NH.requestBody = NH.RequestBodyLBS $ WH.urlEncodeAsForm ((
        [ ("submitid" , submitId)
        , ("url", href)
        ]) :: [(String, String)])
      , NH.redirectCount = 0
      }

_fetchArchiveSubmitId :: Handler (Either String String)
_fetchArchiveSubmitId = do
  MM.increment "archive.fetchSubmitId"  
  res <- liftIO $ NH.httpLbs buildSubmitRequest =<< NH.getGlobalManager
  MM.increment ("archive.fetchSubmitId_status_" <> (pack.show) (NH.statusCode (NH.responseStatus res))) 
  pure $ _parseSubmitId (LBS.toStrict (responseBody res))
  where
    buildSubmitRequest =
      NH.parseRequest_ "https://archive.is/" & \r ->
        r {NH.requestHeaders = [("User-Agent", _archiveUserAgent)]}

_archiveUserAgent :: ByteString
_archiveUserAgent = "espial"

_parseSubmitId :: BS.ByteString -> Either String String
_parseSubmitId res = do
  (flip AP.parseOnly) res $ do
    skipAnyTill (AP.string "submitid\" value=\"")
    AP.many1 (AP.notChar '"')
  where
    skipAnyTill end = go where go = end *> pure () <|> AP.anyChar *> go
