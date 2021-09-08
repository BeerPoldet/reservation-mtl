{-# LANGUAGE FlexibleContexts #-}

module App
    (
      postReservation
    ) where

import           ApiModel
import           Control.Monad          (forM_)
import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT, throwError)
import           Control.Monad.Trans    (lift)
import           DB

connStr :: ConnectionString
connStr = "."

svcAddr :: ServiceAddress
svcAddr = "."

hoistEither :: MonadError e m => Either e a -> m a
hoistEither = either throwError return

checkCaravan :: CaravanOperations m => Reservation -> Error -> ExceptT Error m Reservation
checkCaravan reservation err = do
  c <- lift $ findCaravan (quantity reservation) (date reservation)
  newRes <- hoistEither $ checkCaravanCapacityOnError err c reservation
  lift $ forM_ c $ reserveCaravan (date newRes)
  return newRes

postReservation :: (Operations m, CaravanOperations m) => ReservationRendition -> m (HttpResult ())
postReservation candidate = fmap toHttpResult $ runExceptT $ do
  r <- hoistEither $ validateReservation candidate
  i <- getReservedSeats $ date r
  catchError (hoistEither $ checkCapacity 10 i r) (checkCaravan r) 
  hoistEither $ checkCapacity 10 i r
  >>= saveReservation

postReservationIO :: ReservationRendition -> IO (HttpResult ())
postReservationIO candidate = runProduction connStr svcAddr (postReservation candidate)

postReservationTest :: ReservationRendition -> TestState -> (HttpResult (), TestState)
postReservationTest candidate s = runTest s $ postReservation candidate
