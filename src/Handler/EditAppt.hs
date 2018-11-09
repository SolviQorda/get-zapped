{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.EditAppt where

import Import
import Yesod.Form.Bootstrap3

getEditApptR :: UserId -> TherapistAppointmentId -> Handler Html
getEditApptR userId therapistAppointmentId = do
  user <- runDB $ get404 userId
  therapistAppointment <- runDB $ get404 therapistAppointmentId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm editApptForm
  defaultLayout $ do
    $(widgetFile "therapist/dashboard/view/edit/edit-appt")

editApptForm :: AForm Handler EditAppt
editApptForm = EditAppt
                <$> areq timeField "Start Time" Nothing
                <*> areq timeField "End Time" Nothing
                <*> areq dayField "Date" Nothing

postEditApptR :: UserId -> TherapistAppointmentId -> Handler Html
postEditApptR userId therapistAppointmentId = do
  user <- runDB $ get404 userId
  therapistAppointment <- runDB $ get404 therapistAppointmentId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm editApptForm
  case res of
    FormSuccess editAppt -> do
      _ <- runDB $
        update therapistAppointmentId
        [ TherapistAppointmentTimeStart =. (editApptStart editAppt)
        , TherapistAppointmentTimeEnd   =. (editApptEnd editAppt)
        , TherapistAppointmentDate      =. (editApptDate editAppt)]
      redirect $ ViewApptsR userId
    _ -> redirect $ MainDashboardR userId
