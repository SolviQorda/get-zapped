{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}


module Handler.TriggerEmailToTherapist where

import Import

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Yaml
import Network.Mail.Mime
import Network.Mail.Mime.SES
import qualified System.Exit as SE
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.Shakespeare.Text (stext)



--need booking
triggerTherapistEmail :: MonadIO m => User -> m ()
triggerTherapistEmail therapist = do
    h <- getYesod
    sesCreds <- liftIO $ getSESCredentials
    -- sendMailSes :: MonadIO m	 => Manager	-> SES -> ByteString -> m ()
    -- sendMailSes (getHttpManager h) sesCreds _ = do
    liftIO $
      -- renderSendMailSES :: MonadIO m => Manager -> SES -> Mail -> m ()
      renderSendMailSES (getHttpManager h) sesCreds
        (emptyMail $ Address Nothing "getzapped@protonmail.com")
      { mailTo = [Address Nothing (userEmail therapist)]}
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

                  #{HomeR}

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
                <a href=#{HomeR}>#{HomeR}
              <p>Ta!
            |]
          , partHeaders = []
          }

getTriggerEmailToTherapistR :: UserId -> Handler Html
getTriggerEmailToTherapistR userId = do
  therapist <- runDB $ get404 userId
  triggerTherapistEmail therapist
  defaultLayout $ do
    $(widgetFile "/about")


postTriggerEmailToTherapistR :: UserId -> Handler Html
postTriggerEmailToTherapistR = error "Not yet implemented: postTriggerEmailToTherapistR"
