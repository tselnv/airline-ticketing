{-# LANGUAGE ConstraintKinds #-}

module KolkhozAvia.Domain.Flights
  ( FlightRepo (..),
    FlightRepoError (..),
    Seat,
    Passenger,
    FlightRun,
    Plane (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time.Calendar (Day)

type Flight = String

type FlightDate = Day

type FlightRun = (Flight, FlightDate)

type Passenger = String

type Seat = Int

data Plane = Plane
  { plainModel :: String,
    plainSeats :: Set (Seat)
  }
  deriving (Eq, Ord, Show)

data FlightRepoError
  = FlightIsAlreadyExistsError FlightRun
  | FlightDoesNotExistError FlightRun
  | AddPassengerFlightDoesNotExistError FlightRun
  | AddPassengerNoSuchSeat FlightRun Seat
  | AddPassengerSeatIsAlreadyTakenError FlightRun Seat
  | DeletePassengerFlightDoesNotExistError FlightRun
  | DeletePassengerSeatIsFreeError FlightRun Seat
  | DeletePassengerWrongPassengerNameError FlightRun Passenger Seat
  | DeletePassengerSeatNoSuchSeatError FlightRun Seat
  deriving (Eq, Ord, Show)

class MonadIO m => FlightRepo m where
  addFlight :: FlightRun -> Plane -> m (Either FlightRepoError ())
  deleteFlight :: FlightRun -> m (Either FlightRepoError ())
  addPassenger :: FlightRun -> Passenger -> Seat -> m (Either FlightRepoError ())
  deletePassenger :: FlightRun -> Passenger -> Seat -> m (Either FlightRepoError ())
  getSeats :: FlightRun -> m (Either FlightRepoError (Map Seat (Maybe Passenger)))
