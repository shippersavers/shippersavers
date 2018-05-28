{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Seed.Tariff
  ( fillDB
  , createTariff
  , testTariff
  , seedTariffs
  , parsing_
  ) where

import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool, insert)
import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Maybe (mapMaybe)
import Lib (csvFile)
import Text.ParserCombinators.Parsec
import Config (AppT(..))
import Data.Text (Text, pack)
import Data.Int (Int64)
import Database.Persist.Postgresql
       (Entity(..), (==.), fromSqlKey, insert, selectFirst, selectList)
import Models
import Control.Monad.Except (MonadIO, liftIO)

seedTariffs :: [Tariff] -> SqlPersistT IO ()
seedTariffs tariffs = do
  runMigration migrateAll
  mapM_ (\t -> insert $ t ) tariffs
  return ()


-- | Fill a database from csv files
fillDB :: IO ()
fillDB = do
  c <- getContents
  case parse csvFile "(stdin)" c of
    Left e -> do
      putStrLn "Error parsing input:"
      print e
    Right r ->
      let
      tariffs = parsing (filterEmptyCells (filterEmptyCells r))
      in
      print tariffs


parsing_ r = parsing (filterEmptyCells (filterEmptyCells r))

testTariff :: Tariff
testTariff = (Tariff "trade" "origin" "destination" "ohc" "dhc" "ers" "sbf" "lss" "pss" )


-- | Creates a user in the database.
createTariff :: MonadIO m => Tariff -> AppT m Int64
createTariff p = do
  -- increment "createTariff"
  -- logDebugNS "web" "creating a Tariff"
  newTariff <-
    runDb
      (insert
         (Tariff
            (tariffTrade p)
            (tariffOrigin p)
            (tariffDestination p)
            (tariffOhc p)
            (tariffDhc p)
            (tariffErs p)
            (tariffSbf p)
            (tariffLss p)
            (tariffPss p)))
  return $ fromSqlKey newTariff

filterEmptyCells :: [[[Char]]] -> [[[Char]]]
filterEmptyCells list = filter (not . null) (map (filter (not . null)) list)

parsing :: [[[Char]]] -> (Container, [Tariff])
parsing (x:_:xs) = (toContainer x, toTariffs xs)
parsing _ = (DRY20, [])

toContainer :: [[Char]] -> Container
toContainer _ =
  DRY20

toTariffs :: [[[Char]]] -> [Tariff]
toTariffs xs =
  mapMaybe toTariff xs

toTariff :: [[Char]] -> Maybe Tariff
toTariff (trade:origin:destination:ohc:dhc:ers:sbf:lss:pss:[]) =
  Just
    (Tariff
       (pack trade)
       (pack origin)
       (pack destination)
       (pack ohc)
       (pack dhc)
       (pack ers)
       (pack sbf)
       (pack lss)
       (pack pss))
toTariff _ = Nothing

data Container =
  DRY20 deriving (Show)
