{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module KolkhozAvia.Interact
  ( addFlight,
    deleteFlight,
    addPassenger,
    deletePassenger,
    prettyByName,
    prettyBySeat,
    htmlFlightsReportByName,
    htmlFlightsReportBySeat,
    freeSeats,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as ByteString
import KolkhozAvia.API qualified as API
import KolkhozAvia.Domain.Flights (FlightRepo, FlightRepoError, FlightRun, Passenger, Plane, Seat)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

processAPIAction :: FlightRepo m => m (Either FlightRepoError a) -> m ()
processAPIAction action = do
  res <- action
  case res of
    Left err -> liftIO $ print err
    Right _ -> pure ()

displayAPIAction :: FlightRepo m => m (Either FlightRepoError String) -> m ()
displayAPIAction action = do
  res <- action
  case res of
    Left err -> liftIO $ print err
    Right str -> liftIO $ putStrLn str

addFlight :: FlightRepo m => FlightRun -> Plane -> m ()
addFlight flightRun plane = processAPIAction $ API.addFlight flightRun plane

deleteFlight :: FlightRepo m => FlightRun -> m ()
deleteFlight = processAPIAction . API.deleteFlight

addPassenger :: FlightRepo m => FlightRun -> Passenger -> Seat -> m ()
addPassenger flightRun passenger seat = processAPIAction $ API.addPassenger flightRun passenger seat

deletePassenger :: FlightRepo m => FlightRun -> Passenger -> Seat -> m ()
deletePassenger flightRun passenger seat = processAPIAction $ API.deletePassenger flightRun passenger seat

freeSeats :: FlightRepo m => FlightRun -> m ()
freeSeats flightRun = do
  res <- API.freeSeats flightRun
  case res of
    Left err -> liftIO $ print err
    Right seats -> liftIO $ putStrLn ("Free seats " ++ show flightRun ++ ":") >> print seats

prettyByName :: FlightRepo m => FlightRun -> m ()
prettyByName = displayAPIAction . API.prettyByName

prettyBySeat :: FlightRepo m => FlightRun -> m ()
prettyBySeat = displayAPIAction . API.prettyBySeat

htmlFlightsReportByName :: FlightRepo m => [FlightRun] -> FilePath -> m ()
htmlFlightsReportByName flights path = do
  html <- API.htmlFlightsReportByName flights
  liftIO $ ByteString.writeFile path (renderHtml html)

htmlFlightsReportBySeat :: FlightRepo m => [FlightRun] -> FilePath -> m ()
htmlFlightsReportBySeat flights path = do
  html <- API.htmlFlightsReportBySeat flights
  liftIO $ ByteString.writeFile path (renderHtml html)
