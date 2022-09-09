{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module KolkhozAvia.Adapter.InMemory
  ( FlightsState,
    addFlight,
    deleteFlight,
    addPassenger,
    deletePassenger,
    getSeats,
    initialFlightsState,
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), lift, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), asks)
import Data.Has (Has (getter))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import GHC.Conc (TVar (..), atomically, readTVar, writeTVar)
import KolkhozAvia.Domain.Flights
  ( FlightRepoError (..),
    FlightRun,
    Passenger,
    Plane (plainSeats),
    Seat,
  )

data FlightInfo = FlightInfo
  { flightInfoSeats :: Map Seat (Maybe Passenger),
    flightInfoPlane :: Plane
  }
  deriving (Eq, Ord, Show)

data FlightsState = FlightsState
  { flightsStateMap :: Map FlightRun FlightInfo
  }
  deriving (Show)

type InMemory r m = (Has (TVar FlightsState) r, MonadReader r m, MonadIO m)

initialFlightsState :: FlightsState
initialFlightsState = FlightsState Map.empty

addFlight :: InMemory r m => FlightRun -> Plane -> m (Either FlightRepoError ())
addFlight flightRun plane = do
  (tvar :: TVar FlightsState) <- asks getter
  liftIO $
    atomically . runExceptT $ do
      flightState <- lift $ readTVar tvar
      let flights = flightsStateMap flightState
      when (flightRun `Map.member` flights) $ throwError (FlightIsAlreadyExistsError flightRun)
      let freeSeatsMap = Map.fromList [(seat, Nothing) | seat <- Set.toList (plainSeats plane)]
      let flightInfo = FlightInfo {flightInfoSeats = freeSeatsMap, flightInfoPlane = plane}
      let newFlightState = FlightsState $ Map.insert flightRun flightInfo flights
      lift $ writeTVar tvar newFlightState

deleteFlight :: InMemory r m => FlightRun -> m (Either FlightRepoError ())
deleteFlight flightRun = do
  (tvar :: TVar FlightsState) <- asks getter
  liftIO $
    atomically . runExceptT $ do
      flightState <- lift $ readTVar tvar
      let flights = flightsStateMap flightState
      when (not $ flightRun `Map.member` flights) $ throwError (FlightDoesNotExistError flightRun)
      let newFlightState = FlightsState $ Map.delete flightRun flights
      lift $ writeTVar tvar newFlightState

addPassenger :: InMemory r m => FlightRun -> Passenger -> Seat -> m (Either FlightRepoError ())
addPassenger flightRun passenger seat = do
  (tvar :: TVar FlightsState) <- asks getter
  liftIO $
    atomically . runExceptT $ do
      flightState <- lift $ readTVar tvar
      let flights = flightsStateMap flightState
      case flightRun `Map.lookup` flights of
        Nothing -> throwError (AddPassengerFlightDoesNotExistError flightRun)
        Just flightInfo -> do
          let seats = flightInfoSeats flightInfo
          case seat `Map.lookup` seats of
            Nothing -> throwError (AddPassengerNoSuchSeat flightRun seat)
            Just (Just _) -> throwError (AddPassengerSeatIsAlreadyTakenError flightRun seat)
            Just Nothing -> do
              let newFlightInfo = flightInfo {flightInfoSeats = Map.insert seat (Just passenger) seats}
              let newFlightState = FlightsState $ Map.insert flightRun newFlightInfo flights
              lift $ writeTVar tvar newFlightState

deletePassenger :: InMemory r m => FlightRun -> Passenger -> Seat -> m (Either FlightRepoError ())
deletePassenger flightRun passenger seat = do
  (tvar :: TVar FlightsState) <- asks getter
  liftIO $
    atomically . runExceptT $ do
      flightState <- lift $ readTVar tvar
      let flights = flightsStateMap flightState
      case flightRun `Map.lookup` flights of
        Nothing -> throwError (DeletePassengerFlightDoesNotExistError flightRun)
        Just flightInfo -> do
          let seats = flightInfoSeats flightInfo
          case seat `Map.lookup` seats of
            Nothing -> throwError (DeletePassengerSeatNoSuchSeatError flightRun seat)
            Just Nothing -> throwError (DeletePassengerSeatIsFreeError flightRun seat)
            Just (Just passenger') ->
              if passenger == passenger'
                then
                  let newFlightInfo = flightInfo {flightInfoSeats = Map.delete seat seats}
                      newFlightState = FlightsState $ Map.insert flightRun newFlightInfo flights
                   in lift $ writeTVar tvar newFlightState
                else throwError (DeletePassengerWrongPassengerNameError flightRun passenger seat)

getSeats :: InMemory r m => FlightRun -> m (Either FlightRepoError (Map Seat (Maybe Passenger)))
getSeats flightRun = do
  (tvar :: TVar FlightsState) <- asks getter
  liftIO $
    atomically . runExceptT $ do
      flightState <- lift $ readTVar tvar
      let flights = flightsStateMap flightState
      case Map.lookup flightRun flights of
        Just flightInfo -> pure (flightInfoSeats flightInfo)
        Nothing -> throwError (FlightDoesNotExistError flightRun)
