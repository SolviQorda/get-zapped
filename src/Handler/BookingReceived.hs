{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handler.BookingReceived where

import Import
import Database.Persist.Sql
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Yaml
import Network.Mail.Mime
import Network.Mail.Mime.SES
import qualified System.Exit as SE
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Shakespeare.Text (stext)

getBookingReceivedR :: ZapBookingId -> Handler Html
getBookingReceivedR zapBookingId = do
  zapBooking <- runDB $ get404 zapBookingId
  appt       <- runDB $ get404 $ zapBookingAppointment zapBooking
  therapist  <- runDB $ get404 $ therapistAppointmentTherapistId appt
  triggerTherapistEmail therapist
  defaultLayout $ do
    $(widgetFile "/new/book/result/booking-received")


triggerTherapistEmail :: User -> Handler ()
triggerTherapistEmail therapist = do
    h <- getYesod
    sesCreds <- liftIO $ getSESCredentials
    liftIO $
      renderSendMailSES (getHttpManager h) sesCreds
        (emptyMail $ Address Nothing "getzapped@protonmail.com")
      { mailTo = [Address Nothing (userEmail therapist)]
      , mailHeaders =
        [ ("Subject", "You have a new booking on Get Zapped!")
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
                You've received a new booking! Click on the link below to log in and see the details.

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
            <p>You've received a new booking! Click on the link below to log in and see the details.
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
