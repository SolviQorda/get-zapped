{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.GenerateBookingUrl where

import Import
import Yesod.Form.Bootstrap3

getGenerateBookingUrlR :: UserId -> Handler Html
getGenerateBookingUrlR userId = do
  user <- runDB $ get404 userId
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/gen/generate-my-url")
