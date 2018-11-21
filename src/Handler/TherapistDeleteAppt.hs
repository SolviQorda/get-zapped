{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.TherapistDeleteAppt where

import Import

getTherapistDeleteApptR :: UserId -> TherapistAppointmentId -> Handler Html
getTherapistDeleteApptR userId apptId = do
  _ <- runDB $ delete apptId
  redirect $ ViewApptsR userId

postTherapistDeleteApptR :: UserId -> TherapistAppointmentId -> Handler Html
postTherapistDeleteApptR userId apptId = error "Not yet implemented: postTherapistDeleteApptR"
