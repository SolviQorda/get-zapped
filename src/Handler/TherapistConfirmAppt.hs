{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.TherapistConfirmAppt where

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
        triggerEmailToUser appt submitConf
        redirect $ ViewApptsR userId
      _ -> error "form failure - please check and try again"

triggerEmailToUser :: TherapistAppointment -> SubmitConfirmation -> Handler ()
triggerEmailToUser appt submitConf = do
  h <- getYesod
  sesCreds <- liftIO $ getSESCredentials
  liftIO $
    renderSendMailSES (getHttpManager h) sesCreds
      (emptyMail $ Address Nothing "getzapped@protonmail.com")
    { mailTo = [Address Nothing (fromMaybe "error:no email" $ therapistAppointmentBookedByEmail appt)]
    , mailHeaders =
      [ ("Subject", handleSubject submitConf)
      ]
    , mailParts = [[textPart, htmlPart]]
    }
  where
    getSESCredentials :: IO SES
    getSESCredentials = do
      key <- getSesAccessKey
      return SES
        { sesFrom = "getzapped@protonmail.com"
        , sesTo = [(TE.encodeUtf8 (fromMaybe "error:no email" $ therapistAppointmentBookedByEmail appt))]
        , sesAccessKey = TE.encodeUtf8 $ awsAccessKey
        , sesSecretKey = TE.encodeUtf8 $ awsSecretKey
        , sesSessionToken = Nothing
        , sesRegion = usEast1
        }
    getSesAccessKey :: IO SesKeys
    getSesAccessKey = do
      ymlConfig <- C8.readFile "config/secrets.yaml"

      case decode ymlConfig of
        Nothing -> do C8.putStrLn "Error while parsing secrets.yaml"; SE.exitWith (SE ExitFailure 1)
        Just c -> return c

    textPart = Part
      { partType = "text/plain; charset=utf-8"
      , partEncoding = None
      , partFilename = Nothing
      , partContent = LTE.encodeUtf8 $
          [stext|
            Hey #{name},

            #{body}

            Thank you!

            Solvi @ Get Zapped
          |]
      , partHeaders = []
      }
    htmlPart = Part
      { partType = "text/html; charset=utf-8"
      , }
      name :: Text
      name = fromMaybe "!" $ therapistAppointmentBookedBy appt

      body :: Text
      body
        | submitConfirmationConfirmOrReject submitConf =
          T.concat
            [ T.pack "Your therapist has confirmed your appointment! "
            , (if (submitConfirmationMsg == Nothing = T.pack " ")
               else T.concat ["\nNote from Therapist: \n", fromMaybe (T.pack " ") $ submitConfirmationMsg)
        | otherwise                                    =
          T.concat
            [ T.pack "Your therapist has rejected your booking. "
            , (if (submitConfirmationMsg == Nothing = T.pack " ")
               else T.concat ["\nNote from Therapist: \n", fromMaybe (T.pack " ") $ submitConfirmationMsg)

handleConfirmationText :: Bool -> Text
handleConfirmationText confirm
  | confirm =

handleSubject :: SubmitConfirmation -> Text
handleSubject conf
  | submitConfirmationConfirmOrReject = "Your appointment has been confirmed!"
  | otherwise                         = "Sorry, your booking has been rejected."

confirmParts :: SubmitConfirmation -> []

rejectParts :: SubmitConfirmation ->
