{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool, insert)
import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import GHC.Generics (Generic)

import Config (Config, configPool)
import Data.Text (Text, pack)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User json
    name Text
    email Text
    deriving Show Eq
Tariff json
  trade Text
  origin Text
  destination Text
  ohc Text
  dhc Text
  ers Text
  sbf Text
  lss Text
  pss Text
    deriving Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration migrateAll
  return ()

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
