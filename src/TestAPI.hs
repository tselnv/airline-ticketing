{-# LANGUAGE ImportQualifiedPost #-}

module TestAPI (runAction) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Set qualified as Set
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Conc (newTVarIO)
import KolkhozAvia.API qualified as API
import KolkhozAvia.Adapter.InMemory qualified as M
import KolkhozAvia.Domain.Flights (Plane (..))
import KolkhozAvia.Interact

day :: Int -> Int -> Integer -> Day
day d m y = fromGregorian y m d

action :: API.App ()
action = do
  let fl117 = ("FL#117", day 1 2 1982)
  let planeFL117 = Plane "Broiler 747" (Set.fromList [1 .. 100])
  addFlight fl117 planeFL117

  addPassenger fl117 "Karlos Jobim Venibagovich" 21
  addPassenger fl117 "Zorg Petr Ivanovich" 21
  addPassenger fl117 "Bonch Bruevich" 4
  addPassenger fl117 "Zorg Petr Ivanovich" 16
  prettyByName fl117
  prettyBySeat fl117

  deletePassenger fl117 "Sviborg Bruevich" 4
  deletePassenger fl117 "Bonch Bruevich" 4
  prettyBySeat fl117
  deleteFlight fl117
  prettyByName fl117

  let fl108 = ("FL#108", day 14 7 2005)
  let planeFL108 = Plane "U-2(Po-2) 464" (Set.fromList [1 .. 200])
  addFlight fl108 planeFL108
  addPassenger fl108 "Zukerman Dazdraperma Ludvigovna" 60
  addPassenger fl108 "Ivanov Ivan Ivanovich" 37
  addPassenger fl108 "Petrov Petr Petrovich" 24
  prettyByName fl108

  freeSeats fl108

  htmlFlightsReportBySeat [fl117, fl108] ".report.html"

  liftIO $ putStrLn "\nFrom KolkhozAvia with love!"

runAction :: IO ()
runAction = do
  tvar <- newTVarIO M.initialFlightsState
  API.runApp tvar action
