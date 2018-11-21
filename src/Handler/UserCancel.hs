{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.UserCancel where

import Import
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Yaml
import Network.Mail.Mime
import Network.Mail.Mime.SES
import qualified System.Exit as SE
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)
import Yesod.Form.Bootstrap3


getUserCancelR :: UserId -> TherapistAppointmentId -> Handler Html
getUserCancelR userId therapistAppointmentId = do
  appt <- runDB $ get404 therapistAppointmentId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm cancelForm
  defaultLayout $ do
    $(widgetFile "/user/cancel-are-you-sure")

cancelForm :: AForm Handler UserCancel
cancelForm = UserCancel
          <$> areq checkBoxField "Select yes to cancel" Nothing
          <*> areq textField "Reason for cancelling" Nothing

postUserCancelR :: UserId -> TherapistAppointmentId -> Handler Html
postUserCancelR userId therapistAppointmentId = do
  appt <- runDB $ get404 therapistAppointmentId
  user <- runDB $ get404 userId
  therapist <- runDB $ get404 (therapistAppointmentTherapistId appt)
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm cancelForm
  case res of
    FormSuccess userCancel -> do
      _ <- triggerTherapistEmail therapist appt userCancel
      runDB $ updateWhere [TherapistAppointmentId ==. therapistAppointmentId]
            [ TherapistAppointmentBookedBy =. Nothing
            , TherapistAppointmentBookedByEmail =. Nothing
            , TherapistAppointmentBookedByPronouns =. Nothing ]
      redirect UserDashR
    _           -> defaultLayout $ do
      $(widgetFile "/user/cancel-are-you-sure")

triggerTherapistEmail :: User -> TherapistAppointment -> UserCancel -> Handler ()
triggerTherapistEmail therapist appt userCancel = do
    h <- getYesod
    sesCreds <- liftIO $ getSESCredentials
    liftIO $
      renderSendMailSES (getHttpManager h) sesCreds
        (emptyMail $ Address Nothing "getzapped@protonmail.com")
      { mailTo = [Address Nothing (userEmail therapist)]
      , mailHeaders =
        [ ("Subject", "A user has cancelled a booking with you.")
        ]
      , mailParts = [[textPart, htmlPart]]
      }
    where
      getSESCredentials :: IO SES
      getSESCredentials = do
        key <- getSesAccessKey
        return SES
          { sesFrom = "getzapped@protonmail.com"
          , sesTo = [(TE.encodeUtf8 (userEmail therapist))]
          , sesAccessKey = TE.encodeUtf8 $ awsAccessKey key
          , sesSecretKey = TE.encodeUtf8 $ awsSecretKey key
          , sesSessionToken = Nothing
          , sesRegion = usEast1
          }
      getSesAccessKey :: IO SesKeys
      getSesAccessKey = do
        ymlConfig <- C8.readFile "config/secrets.yaml"

        case decode ymlConfig of
          Nothing -> do C8.putStrLn "Error while parsing secrets.yaml"; SE.exitWith (SE.ExitFailure 1)
          Just c -> return c

      textPart = Part
        { partType = "text/plain; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = LTE.encodeUtf8 $
              [stext|
                Hey #{therapistName},

                Unfortunately #{user} has cancelled the following booking with you:

                #{timeStart} on #{apptDate}

                Message from user

                #{msg}

                The appointment has been returned to your available bookings.
                To see all your booking details head to:

                http://localhost:3000/auth/login

                Ta!
              |]
        , partHeaders = []
        }
      htmlPart = Part
        { partType = "text/html; charset=utf-8"
        , partEncoding = None
        , partFilename = Nothing
        , partContent = renderHtml
          [shamlet|
            <p>Hey #{therapistName},
            <p>
            <p>Unfortunately #{user} has cancelled the following booking with you:
            <p>
            <p>#{timeStart} on #{apptDate}
            <p>Message from user
            <p>
            <p>#{msg}
            <p>
            <p>The appointment has been returned to your available bookings.
            <p>To see all your booking details head to:
            <p>
            <a href="http://localhost:3000/auth/login">
              http://localhost:3000/auth/login
            <p>
            <p>Ta!
          |]
        , partHeaders = []
        }
      loginUrl :: String
      loginUrl = "http://localhost:3000/auth/login"

      user :: Text
      user = fromMaybe " user unknown " $ therapistAppointmentBookedBy appt

      therapistName :: Text
      therapistName = fromMaybe "!" $ userName therapist

      timeStart :: Text
      timeStart = pack $ show $ therapistAppointmentTimeStart appt

      apptDate :: Text
      apptDate = pack $ show $ therapistAppointmentDate appt

      msg :: Text
      msg = userCancelMsg userCancel
