{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import Types
import Model
import ModelCrypto

import qualified Database.Persist as P
import qualified Database.Persist.Sqlite as P

import ClassyPrelude
import Lens.Micro

import Options.Generic

data MigrationOpts
  = CreateDB { conn :: Text}
  | CreateUser { conn :: Text
               , userName :: Text
               , userPassword :: Text
               , userApiToken :: Maybe Text
               , privateDefault :: Maybe Bool
               , archiveDefault :: Maybe Bool }
  | DeleteUser { conn :: Text
               , userName :: Text}
  | ImportBookmarks { conn :: Text
                    , userName :: Text
                    , bookmarkFile :: FilePath}
  | ImportNotes { conn :: Text
                , userName :: Text
                , noteDirectory :: FilePath}
  | PrintMigrateDB { conn :: Text}
  deriving (Generic, Show)

instance ParseRecord MigrationOpts

main :: IO ()
main = do
  args <- getRecord "Migrations"
  case args of
    PrintMigrateDB conn ->
      P.runSqlite conn dumpMigration

    CreateDB conn -> do
      let connInfo = P.mkSqliteConnectionInfo conn
                     & set P.fkEnabled False
      P.runSqliteInfo connInfo runMigrations

    CreateUser conn uname upass utoken dprivate darchive ->
      P.runSqlite conn $ do
        hash' <- liftIO (hashPassword upass)
        void $ P.upsertBy
          (UniqueUserName uname)
          (User uname hash' utoken dprivate darchive)
          [ UserPasswordHash P.=. hash'
          , UserApiToken P.=. utoken
          , UserPrivateDefault P.=. dprivate
          , UserArchiveDefault P.=. darchive
          ]
        pure () :: DB ()

    DeleteUser conn uname ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName uname)
        case muser of
          Nothing -> liftIO (print (uname ++ "not found"))
          Just (P.Entity uid _) -> do
            P.deleteCascade uid
            pure () :: DB ()

    ImportBookmarks conn uname file ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName uname)
        case muser of
          Just (P.Entity uid _) -> insertFilePbPosts uid file
          Nothing -> liftIO (print (uname ++ "not found"))

    ImportNotes conn uname dir ->
      P.runSqlite conn $ do
        muser <- P.getBy (UniqueUserName uname)
        case muser of
          Just (P.Entity uid _) -> insertDirPbNotes uid dir
          Nothing -> liftIO (print (uname ++ "not found"))
