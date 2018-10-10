{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.BookZap where

import Data.Time.Calendar
import Data.Time.LocalTime
import Import
import Yesod.Form
import Yesod.Form.Bootstrap3

-- data ZapBooking = ZapBooking
--     { userName           :: Text
--     , userEmail          :: Text
--     , bookingDate        :: Day
--     , bookingTimeStart   :: TimeOfDay
--     , bookingTimeEnd    :: TimeOfDay
--     } deriving (Eq, Show)

zapRequestForm :: AForm Handler ZapBooking
zapRequestForm = ZapBooking
              <$> areq textField "User Name"  Nothing
              <*> areq textField "User Email" Nothing
              <*> areq dayField  "Date"       Nothing
              <*> areq timeField "Start Time" Nothing
              <*> areq timeField "End Time"   Nothing

getBookZapR :: Handler Html
getBookZapR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm zapRequestForm
  defaultLayout $ do
    $(widgetFile "zaps/new-zap")

postBookZapR :: Handler Html
postBookZapR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm zapRequestForm
  case res of
    FormSuccess _ -> error "todo"
    _ -> defaultLayout $(widgetFile "zaps/new-zap")
      -- zapBookingId <- runDB $ insert booking
      --todo - confirmation page
