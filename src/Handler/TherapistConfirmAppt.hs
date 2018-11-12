{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.TherapistConfirmAppt where

import Import
import Yesod.Form.Bootstrap3

getTherapistConfirmApptR :: UserId -> TherapistAppointmentId -> Handler Html
getTherapistConfirmApptR userId apptId = do
  appt <- runDB $ get404 apptId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ confirmOrRejectForm apptId appt
  defaultLayout $ do
    $(widgetFile "therapist/dashboard/view/confirm/confirm-reject")

confirmOrRejectForm :: TherapistAppointmentId -> TherapistAppointment -> AForm Handler SubmitConfirmation
confirmOrRejectForm apptId appt = SubmitConfirmation
                <$> areq boolField "To confirm, choose yes. To cancel/reject, choose no  " (therapistAppointmentConfirmed appt)
                <*> aopt textField "(Optional) Message for user  " Nothing
                <*> pure apptId

postTherapistConfirmApptR :: UserId -> TherapistAppointmentId -> Handler Html
postTherapistConfirmApptR userId apptId = do
  appt <- runDB $ get404 apptId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ confirmOrRejectForm apptId appt
  case res of
    FormSuccess submitConf -> do
      _ <- runDB $ insert submitConf
      _ <- runDB $
        update apptId
        [ TherapistAppointmentConfirmed =. Just (submitConfirmationConfirmOrReject submitConf) ]
      redirect $ ViewApptsR userId
    _ -> error "form failure - please check and try again"
