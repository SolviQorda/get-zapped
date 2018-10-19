{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.BookingReceived where

import Import
import Database.Persist.Sql

getBookingReceivedR :: ZapBookingId -> Handler Html
getBookingReceivedR zapBookingId = do
  zapBooking <- runDB $ get404 zapBookingId
  defaultLayout $ do
    $(widgetFile "zaps/booking-received")
