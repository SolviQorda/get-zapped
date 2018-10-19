{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.AppointmentAdded where

import Import

getAppointmentAddedR :: TherapistAppointmentId -> Handler Html
getAppointmentAddedR therapistAppointmentId = do
  therapistAppointment <- runDB $ get404 therapistAppointmentId
  defaultLayout $ do
    $(widgetFile "zaps/therapist/appointment-added")
