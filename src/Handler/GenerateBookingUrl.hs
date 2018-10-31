{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.GenerateBookingUrl where

import Import
import Yesod.Form.Bootstrap3

getGenerateBookingUrlR :: TherapistChoiceId -> Handler Html
getGenerateBookingUrlR therapistChoiceId = do
  therapistChoice <- runDB $ get404 therapistChoiceId
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/gen/generate-my-url")
