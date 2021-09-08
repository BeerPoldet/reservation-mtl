{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module DB
  ( ConnectionString
  , Operations (..)
  , ProductionOperations
  , runProduction
  , TestState
  , TestOperations
  , runTest
  , CaravanOperations (..)
  , ServiceAddress
  ) where

import           ApiModel
import           Control.Monad.Except      (ExceptT)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, ask,
                                            runReaderT)
import           Control.Monad.State       (State, runState)
import           Control.Monad.Trans.Class (lift)
import           Data.List                 (find)
import           Data.Map                  (Map)
import           Data.Time                 (FormatTime (..), UTCTime,
                                            ZonedTime (..), defaultTimeLocale,
                                            formatTime)
import           System.Directory          (doesFileExist)
import           System.FilePath           ((</>))

type ConnectionString = String

rawFileNameForDate :: FormatTime t => t -> String
rawFileNameForDate = formatTime defaultTimeLocale "%F"

fileNameForReservation :: Reservation -> FilePath
fileNameForReservation = (++ ".txt") . rawFileNameForDate . date

class Monad m => Operations m where
  readReservations :: ZonedTime -> m [Reservation]
  getReservedSeats :: ZonedTime -> m Int
  saveReservation :: Reservation -> m ()

instance Operations m => Operations (ExceptT e m) where
  readReservations = lift . readReservations
  getReservedSeats = lift . getReservedSeats
  saveReservation = lift . saveReservation

type TestState = Map UTCTime [Reservation]
type TestOperations = State TestState

instance Operations TestOperations where
  readReservations = undefined
  getReservedSeats = undefined
  saveReservation = undefined

runTest :: TestState -> TestOperations a -> (a, TestState)
runTest s t = runState t s

type ProductionConf = (ConnectionString, ServiceAddress)

type ProductionOperations = ReaderT ProductionConf IO

runProduction :: ConnectionString -> ServiceAddress -> ProductionOperations a -> IO a
runProduction connStr svcAddr p = runReaderT p (connStr, svcAddr)

instance Operations ProductionOperations where
  readReservations d = do -- Imagine that this queries a database table instead of reading from a file
    (dir, _) <- ask
    let fileName = dir </> rawFileNameForDate d ++ ".txt"
    exists <- liftIO $ doesFileExist fileName
    if exists
      then liftIO $ read <$> readFile fileName
      else return []

  getReservedSeats d = do
    reservations <- readReservations d
    return (foldr ((+) . quantity) 0 reservations)

  saveReservation r = do --Imagine that this inserts into a database table instead of writing to a file
    reservations <- readReservations (date r)
    -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
    (dir, _) <- ask
    let fileName = dir </> fileNameForReservation r
    length reservations `seq` liftIO $ writeFile fileName $ show (r : reservations)

-- Caravan storage

caravanPool :: [Caravan]
caravanPool = map Caravan [4, 6, 8]

fileNameForCaravan :: ZonedTime -> FilePath
fileNameForCaravan = (++ ".caravan.txt") . rawFileNameForDate

type ServiceAddress = String

class Monad m => CaravanOperations m where
  readReservedCaravans :: ZonedTime -> m [Caravan]
  findCaravan :: Int -> ZonedTime -> m (Maybe Caravan)
  reserveCaravan :: ZonedTime -> Caravan -> m ()

instance CaravanOperations m => CaravanOperations (ExceptT e m) where
  readReservedCaravans = lift . readReservedCaravans
  findCaravan = findCaravan
  reserveCaravan = reserveCaravan

instance CaravanOperations TestOperations where
  readReservedCaravans = undefined
  findCaravan = undefined
  reserveCaravan = undefined

instance CaravanOperations ProductionOperations where
  readReservedCaravans d = do -- Imagine that this queries a web service instead of reading from a file
    (_, dir) <- ask
    let fileName = dir </> fileNameForCaravan d
    exists <- liftIO $ doesFileExist fileName
    if exists
      then liftIO $ read <$> readFile fileName
      else return []

  findCaravan requestedCapacity d = do
    liftIO $ putStrLn "Finding a caravan..."
    reservedCaravans <- readReservedCaravans d
    let availableCaravans = filter (`notElem` reservedCaravans) caravanPool
    return $ find (\c -> requestedCapacity <= caravanCapacity c) availableCaravans

  reserveCaravan d c = do --Imagine that this updates a web service instead of writing to a file
      (_, dir) <- ask
      let fileName = dir </> fileNameForCaravan d
      caravans <- readReservedCaravans d
      -- Use of `seq` as described in http://stackoverflow.com/a/2530948/126014
      length caravans `seq` liftIO $ writeFile fileName $ show (c : caravans)
