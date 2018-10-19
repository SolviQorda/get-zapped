{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AddZapAppointment where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3

addAppointmentForm :: AForm Handler TherapistAppointment
addAppointmentForm = TherapistAppointment
                  <$> areq textField "Therapist Name" Nothing
                  <*> areq dayField "Date" Nothing
                  <*> areq timeField "Start time" Nothing
                  <*> areq timeField "End time" Nothing
                  <*> aopt textField "Booked by" Nothing

getAddAppointmentR :: Handler Html
getAddAppointmentR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm addAppointmentForm
  defaultLayout $ do
    $(widgetFile "zaps/therapist/add-appointment")

postAddAppointmentR :: Handler Html
postAddAppointmentR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addAppointmentForm
  case res of
    FormSuccess therapistAppointment -> do
      therapistAppointmentId <- runDB $ insert therapistAppointment
      redirect $ AppointmentAddedR therapistAppointmentId
    _                                -> defaultLayout $(widgetFile "zaps/therapist/add-appointment")
