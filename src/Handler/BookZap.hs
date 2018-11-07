{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, GADTs, QuasiQuotes, OverloadedStrings, FlexibleContexts #-}

module Handler.BookZap where

import Import
import Yesod.Form.Bootstrap3
import qualified Data.Text as T

zapRequestForm :: Text -> AForm Handler ZapBooking
zapRequestForm therapist = ZapBooking
              <$> areq textField "Your Name " Nothing
              <*> areq textField "Your Email " Nothing
              <*> areq (selectField $ appointments therapist) "Choose appointment " Nothing
              <*> aopt textField "Your Pronouns (optional) " Nothing

getBookZapR :: Text -> Handler Html
getBookZapR therapist = do
    (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ zapRequestForm therapist
    defaultLayout $ do
      $(widgetFile "zaps/new/book/new-zap")

appointments :: Text -> HandlerFor App (OptionList (Key TherapistAppointment))
appointments therapist = do
  rows <- runDB $ selectList [TherapistAppointmentBookedBy ==. Nothing, TherapistAppointmentTherapistName ==. therapist] [Asc TherapistAppointmentDate]
  optionsPairs $ Prelude.map (\r -> ((parseAppt $ entityVal $ r), entityKey r )) rows

parseAppt :: TherapistAppointment -> Text
parseAppt app = T.concat
      [ T.pack $ show $ therapistAppointmentDate app
      , T.pack " from "
      , T.pack $ show $ therapistAppointmentTimeStart app
      , T.pack " to "
      , T.pack $ show $ therapistAppointmentTimeEnd app
      -- , T.pack " with "
      -- , T.pack $ show $ therapistAppointmentTherapistName app
      ]

postBookZapR :: Text -> Handler Html
postBookZapR therapist = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ zapRequestForm therapist
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
    _ -> defaultLayout $(widgetFile "zaps/new/book/new-zap")
