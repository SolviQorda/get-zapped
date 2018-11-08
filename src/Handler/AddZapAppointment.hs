{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AddZapAppointment where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3

addAppointmentForm :: User -> AForm Handler TherapistAppointment
addAppointmentForm user = TherapistAppointment
                  <$> areq dayField  "Date" Nothing
                  <*> areq timeField "Start time" Nothing
                  <*> areq timeField "End time" Nothing
                  <*> pure name
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> pure Nothing
                    where name = fromMaybe "no username set"$  userName user

getAddAppointmentR :: UserId -> Handler Html
getAddAppointmentR userId = do
  therapist <- runDB $ get404 $ userId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ addAppointmentForm therapist
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/new/add-appointment")

postAddAppointmentR :: UserId -> Handler Html
postAddAppointmentR userId = do
  therapist <- runDB $ get404 $ userId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ addAppointmentForm therapist
  case res of
    FormSuccess therapistAppointment -> do
      therapistAppointmentId <- runDB $ insert therapistAppointment
      redirect $ AppointmentAddedR userId therapistAppointmentId
    _                                -> defaultLayout $(widgetFile "/therapist/dashboard/new/add-appointment")
