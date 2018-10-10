{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.BookingReceived where

import Import

getBookingReceivedR :: ZapBookingId -> Handler Html
getBookingReceivedR zapBookingId = do
  booking <- runDB $ get404 zapBookingId
  defaultLayout $ do
    $(widgetFile "zaps/booking-received")
