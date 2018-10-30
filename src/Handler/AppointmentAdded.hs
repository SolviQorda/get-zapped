{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.AppointmentAdded where

import Import

getAppointmentAddedR :: TherapistChoiceId -> TherapistAppointmentId -> Handler Html
getAppointmentAddedR therapistChoiceId therapistAppointmentId = do
  therapistAppointment <- runDB $ get404 therapistAppointmentId
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/new/appointment-added")
