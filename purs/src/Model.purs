module Model where

import Data.Nullable (Nullable)
import Simple.JSON as J

type BookmarkId = Int
type TagId = Int

type Bookmark =
  { url :: String
  , title :: String
  , description :: String
  , tags :: String
  , private :: Boolean
  , toread :: Boolean
  , bid :: BookmarkId
  , selected :: Boolean
  , time :: String
  , archiveUrl :: Nullable String
  }

newtype Bookmark' = Bookmark' Bookmark
derive newtype instance bookmark_rfI :: J.ReadForeign Bookmark'
derive newtype instance bookmark_wfI :: J.WriteForeign Bookmark'

type NoteId = Int

type Note =
  { id :: NoteId
  , title :: String
  , text :: String
  , length :: Int
  , created :: String
  , updated :: String
  }

newtype Note' = Note' Note
derive newtype instance note_rfI :: J.ReadForeign Note'
derive newtype instance note_wfI :: J.WriteForeign Note'

type UserSettings =
  { archiveDefault :: Boolean
  , privateDefault :: Boolean
  }

newtype UserSettings' = UserSettings' UserSettings
derive newtype instance usersettings_rfI :: J.ReadForeign UserSettings'
derive newtype instance usersettings_wfI :: J.WriteForeign UserSettings'
