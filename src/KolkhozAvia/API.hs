{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module KolkhozAvia.API
  ( App (..),
    TVarFlightState,
    htmlFlightsReportByName,
    htmlFlightsReportBySeat,
    addFlight,
    deleteFlight,
    addPassenger,
    deletePassenger,
    getSeats,
    freeSeats,
    prettyByName,
    prettyBySeat,
    runApp,
  )
where

import Colonnade (ascii, headed)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Fmt.Internal.Core (pretty)
import GHC.Conc (TVar)
import KolkhozAvia.Adapter.InMemory qualified as M
import KolkhozAvia.Domain.Flights (FlightRepo (..), FlightRepoError, FlightRun, Passenger, Seat)
import KolkhozAvia.HtmlGenerator (htmlFlightsReportByName, htmlFlightsReportBySeat)

type TVarFlightState = TVar M.FlightsState

newtype App a = App {unApp :: ReaderT TVarFlightState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader TVarFlightState)

instance FlightRepo App where
  addFlight = M.addFlight
  deleteFlight = M.deleteFlight
  addPassenger = M.addPassenger
  deletePassenger = M.deletePassenger
  getSeats = M.getSeats

freeSeats :: FlightRepo m => FlightRun -> m (Either FlightRepoError (Set Seat))
freeSeats flightRun = do
  eitherSeatsMap <- getSeats flightRun
  pure $ do
    seatsMap <- eitherSeatsMap
    pure $ Map.keysSet $ Map.filter isNothing seatsMap

prettyByName :: FlightRepo m => FlightRun -> m (Either FlightRepoError String)
prettyByName flightRun = prettyFlight flightRun snd

prettyBySeat :: FlightRepo m => FlightRun -> m (Either FlightRepoError String)
prettyBySeat flightRun = prettyFlight flightRun fst

prettyFlight :: (FlightRepo m, Ord b) => FlightRun -> ((Seat, Passenger) -> b) -> m (Either FlightRepoError String)
prettyFlight flightRun@(flName, flDate) sortBy = do
  eitherPassengers <- getSeats flightRun
  pure $ do
    passengers <- eitherPassengers
    let flightTable = sortOn sortBy [(seat, p) | (seat, Just p) <- Map.toList passengers]
    Right $ "Flight: " ++ flName ++ " Date: " ++ (show flDate) ++ "\n" ++ seatReport flightTable
  where
    seatReport :: [(Seat, Passenger)] -> String
    seatReport = ascii colStats
      where
        colStats =
          mconcat
            [ headed "Seat" (pretty . fst),
              headed "Passenger" (pretty . snd)
            ]

runApp :: TVarFlightState -> App a -> IO a
runApp tvar app = runReaderT (unApp app) tvar
