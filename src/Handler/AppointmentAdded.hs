{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.AppointmentAdded where

import Import

getAppointmentAddedR :: UserId -> TherapistAppointmentId -> Handler Html
getAppointmentAddedR userId therapistAppointmentId = do
  therapistAppointment <- runDB $ get404 therapistAppointmentId
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/new/appointment-added")
