{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, GADTs, QuasiQuotes, OverloadedStrings, FlexibleContexts #-}

module Handler.BookZap where

import Import
import Yesod.Form.Bootstrap3
import qualified Data.Text as T



zapRequestForm :: User -> Text -> AForm Handler ZapBooking
zapRequestForm user therapist = ZapBooking
              <$> pure name
              <*> pure email
              <*> areq (selectField $ appointments therapist) "Choose appointment " Nothing
              <*> aopt textField "Your Pronouns (optional) " Nothing
              <*> areq (selectField $ payOps therapist) "Select your price tier " Nothing
                where name = fromMaybe (userEmail user) $ userName user
                      email = userEmail user

getBookZapR :: Text -> Handler Html
getBookZapR therapist = do
    (authId, user) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ zapRequestForm user therapist
    defaultLayout $ do
      $(widgetFile "/new/book/new-zap")

appointments :: Text -> HandlerFor App (OptionList (Key TherapistAppointment))
appointments therapist = do
  rows <- runDB $ selectList [TherapistAppointmentBookedBy ==. Nothing, TherapistAppointmentTherapistName ==. therapist] [Asc TherapistAppointmentDate]
  optionsPairs $ Prelude.map (\r -> ((parseAppt $ entityVal $ r), entityKey r )) rows

--parse the appointment so that it's easier to read.
parseAppt :: TherapistAppointment -> Text
parseAppt app = T.concat
      [ T.pack $ show $ therapistAppointmentDate app
      , T.pack " from "
      , T.pack $ show $ therapistAppointmentTimeStart app
      , T.pack " to "
      , T.pack $ show $ therapistAppointmentTimeEnd app
      ]

postBookZapR :: Text -> Handler Html
postBookZapR therapist = do
  (authId, user) <- requireAuthPair
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ zapRequestForm user therapist
  case res of
    FormSuccess zapBooking -> do
      zapBookingId <- runDB $ insert zapBooking
      maybeAppointment <- runDB $ get (zapBookingAppointment zapBooking)
      case maybeAppointment of
        Nothing -> error "no appointment with that id"
        Just _ -> runDB $ update (zapBookingAppointment zapBooking)
            [ TherapistAppointmentBookedBy =. (Just $ zapBookingUserName zapBooking)
            , TherapistAppointmentBookedByEmail =. (Just $ zapBookingUserEmail zapBooking)
            , TherapistAppointmentBookedByPronouns =. (zapBookingUserPronouns zapBooking)]
      redirect $ BookingReceivedR zapBookingId
    _ -> defaultLayout $(widgetFile "/new/book/new-zap")

--get payment options
payOps :: Text -> HandlerFor App (OptionList Tier)
payOps therapist = do
  rows <-  runDB $ selectList [TherapistAppointmentBookedBy ==. Nothing, TherapistAppointmentTherapistName ==. therapist] [Asc TherapistAppointmentDate]
  prefs <- runDB $ selectList [TherapistPrefsTherapist ==. (getId rows)] [Desc TherapistPrefsTherapist]
  optionsPairs
      $ Prelude.map
        (\r -> ((parseTier $ r), r)) (parsePrefs prefs)

getId :: [Entity TherapistAppointment] -> UserId
getId appts = therapistAppointmentTherapistId $ entityVal appt
  where appt = Prelude.head appts

--parse the tier as legible text
parseTier :: Tier -> Text
parseTier tier = T.concat
        [ tierDescription tier
        , pack $ " - £"
        , pack $ show $ tierPricePerHour tier
        , pack $ "/h"
        ]

--no duplicates should exist as the update is always on UUID
--TODO: Handle the possibility of no prefs set.
parsePrefs :: [Entity TherapistPrefs] -> [Tier]
parsePrefs prefs
  | Prelude.null prefs = dummyTier
  | otherwise          = therapistPrefsTiers (entityVal $ pref)
  where pref = Prelude.head prefs
        dummyTier = [Tier 60 (pack "Solidarity and high wage")]
