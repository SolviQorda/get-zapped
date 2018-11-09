{-# LANGUAGE OverloadedStrings#-}
{-# LANGUAGE TemplateHaskell#-}

module Handler.AppointmentAdded where

import Import

getAppointmentAddedR :: UserId -> Handler Html
getAppointmentAddedR userId = do
  user <- runDB $ get404 userId
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/new/appointment-added")
