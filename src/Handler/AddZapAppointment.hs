{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AddZapAppointment where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3

addAppointmentForm :: TherapistChoice -> AForm Handler TherapistAppointment
addAppointmentForm therapist = TherapistAppointment
                  <$> areq dayField  "Date" Nothing
                  <*> areq timeField "Start time" Nothing
                  <*> areq timeField "End time" Nothing
                  <*> pure name
                  <*> pure Nothing
                  <*> pure Nothing
                    where name = therapistChoiceTherapist therapist

getAddAppointmentR :: TherapistChoiceId -> Handler Html
getAddAppointmentR therapistChoiceId = do
  therapist <- runDB $ get404 $ therapistChoiceId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ addAppointmentForm therapist
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/new/add-appointment")

postAddAppointmentR :: TherapistChoiceId -> Handler Html
postAddAppointmentR therapistChoiceId = do
  therapist <- runDB $ get404 $ therapistChoiceId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ addAppointmentForm therapist
  case res of
    FormSuccess therapistAppointment -> do
      therapistAppointmentId <- runDB $ insert therapistAppointment
      redirect $ AppointmentAddedR therapistChoiceId therapistAppointmentId
    _                                -> defaultLayout $(widgetFile "zaps/therapist/dashboard/new/add-appointment")
