{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module KolkhozAvia.HtmlGenerator
  ( htmlFlightsReportByName,
    htmlFlightsReportBySeat,
  )
where

import Colonnade (Colonnade, Headed, headed)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Either (lefts, rights)
import Data.List (sortOn)
import Data.Map qualified as Map
import Fmt (Buildable)
import Fmt.Internal.Core (pretty)
import KolkhozAvia.Domain.Flights (FlightRepo (..), FlightRepoError, FlightRun, Passenger, Seat)
import Text.Blaze.Colonnade (encodeHtmlTable)
import Text.Blaze.Html (Html, string, text)
import Text.Blaze.Html4.FrameSet (docTypeHtml, h1, h2, h3, h4, style, title)
import Text.Blaze.Html4.FrameSet qualified as H

htmlFlightsReportByName :: FlightRepo m => [FlightRun] -> m Html
htmlFlightsReportByName flights = htmlFlightReport snd flights

htmlFlightsReportBySeat :: FlightRepo m => [FlightRun] -> m Html
htmlFlightsReportBySeat flights = htmlFlightReport fst flights

htmlFlightReport :: (FlightRepo m, Ord b) => ((Seat, Passenger) -> b) -> [FlightRun] -> m Html
htmlFlightReport sortBy flights = do
  tables <- traverse (htmlFlightTable sortBy) flights
  let reportHead = H.head $ do
        title $ string "Flights Report"
        style tableStyle
  let reportBody = H.body $ do
        foldr (\(fr, html) acc -> h4 (string $ show fr) >> html >> acc) (pure ()) (rights tables)
        when (not . null . lefts $ tables) $ foldr (\err acc -> acc >> h4 (string $ show err)) (h3 $ string "Flights Report Errors:") (lefts tables)
  let reportHtml = docTypeHtml $ do
        reportHead
        reportBody
  pure reportHtml
  where
    tableStyle :: Html
    tableStyle =
      "table { \
      \ border-collapse: collapse; \
      \ font-family: Tahoma, Geneva, sans-serif; \
      \ } \
      \ table td, th { \
      \ padding: 15px; \
      \ } \
      \ table thead th { \
      \ background-color: #54585d; \
      \ color: #ffffff; \
      \ font-weight: bold; \
      \ font-size: 13px; \
      \ border: 1px solid #54585d; \
      \ } \
      \ table tbody td { \
      \ color: #636363; \
      \ border: 1px solid #dddfe1; \
      \ } \
      \ table tbody tr { \
      \ background-color: #f9fafb; \
      \ } \
      \ table tbody tr:nth-child(odd) { \
      \ background-color: #ffffff; \
      \ } "

-- tableStyle :: Html
-- tableStyle =
--   "table {border-collapse: collapse}"
--     <> "td, th {border: 1px solid black; padding: 5px; background: #fff; color: #212529; font-family: 'Lato'}"

htmlFlightTable :: (FlightRepo m, Ord b) => ((Seat, Passenger) -> b) -> FlightRun -> m (Either FlightRepoError (FlightRun, Html))
htmlFlightTable sortBy flightRun = do
  eitherPassengers <- getSeats flightRun
  pure $ do
    passengers <- eitherPassengers
    let flightTable = sortOn sortBy [(seat, p) | (seat, Just p) <- Map.toList passengers]
    Right $ (flightRun, htmlTable flightTable)

htmlTable :: Foldable f => f (Seat, Passenger) -> Html
htmlTable flightTable = encodeHtmlTable mempty colFlight flightTable

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty

colFlight :: Colonnade Headed (Seat, Passenger) Html
colFlight =
  mconcat
    [ headed "Seat" (viaFmt . fst),
      headed "Passenger" (viaFmt . snd)
    ]
