{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AddZapAppointment where

import Import
import Yesod.Form.Bootstrap3
import Data.Time.Calendar

addAppointmentForm :: User -> AForm Handler TherapistAppointment
addAppointmentForm user = TherapistAppointment
                  <$> areq dayField  "Date" Nothing
                  <*> areq timeField "Start Time" Nothing
                  <*> areq timeField "End Time" Nothing
                  <*> pure name
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> pure Nothing
                  <*> areq (selectFieldList apptOpts) "Appointment type" Nothing
                  <*> areq checkBoxField "Repeat Weekly?" Nothing
                  <*> areq intField "For How Many Weeks?" (Just 0)
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
      appts <- runDB $ insertMany $ handleRepeats therapistAppointment
      redirect $ AppointmentAddedR userId
    _                                -> defaultLayout $(widgetFile "/therapist/dashboard/new/add-appointment")

-- hardcoded strings for appointment options.
-- Consultations are shorter so its useful for therapists to distinguish
apptOpts :: [(Text, Text)]
apptOpts =
  [ ("Full Session", "Full Session")
  , ("Consultation", "Consultation")
  ]

--return a list of appointments based on whether they repeat and how often
handleRepeats :: TherapistAppointment -> [TherapistAppointment]
handleRepeats appt
  | (therapistAppointmentRepeatWeekly appt) == False = [appt]
  | otherwise =
    Prelude.map (getRepeatAppointments appt) intervals
      where intervals = getIntervals $ therapistAppointmentForXWeeks appt

--marshal the repeats into Integer for addDays
getIntervals :: Int -> [Integer]
getIntervals i = Prelude.map Prelude.fromIntegral [1..i]

getRepeatAppointments :: TherapistAppointment -> Integer -> TherapistAppointment
getRepeatAppointments appt interval = appt
    { therapistAppointmentDate = addDays (interval * 7) (therapistAppointmentDate appt)
    , therapistAppointmentRepeatWeekly = False
    , therapistAppointmentForXWeeks = 0
    }
