{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Metrics as M
import Text.ParserCombinators.Parsec
import Database.Persist.Postgresql
import Lens.Micro
import Network.Wai.Handler.Warp (run)
import Network.Wai.Metrics
import System.Environment (lookupEnv)
import System.Metrics (newStore)
import System.Remote.Monitoring (forkServer, serverMetricStore)

import Api (app)
import Api.User (generateJavaScript)
import Config (Config(..), Environment(..), makePool, setLogger)
import Logger (defaultLogEnv)
import Models (doMigrations)
import Safe (readMay)
import Seed.Tariff (fillDB, seedTariffs, testTariff, parsing_)
import Lib (csvFile)

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]


-- | Fill a database from csv files
main :: IO ()
main = do
  env <- lookupSetting "ENV" Development
  port <- lookupSetting "PORT" 8081
  logEnv <- defaultLogEnv
  pool <- makePool env logEnv
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r ->
      let
      (container, tariffs) = parsing_ r
      in
      runSqlPool (seedTariffs tariffs) pool
