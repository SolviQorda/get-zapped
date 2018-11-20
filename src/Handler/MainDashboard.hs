{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.MainDashboard where

import Import
import Yesod.Form.Bootstrap3

getMainDashboardR :: UserId -> Handler Html
getMainDashboardR userId = do
  user <- runDB $ get404 userId
  paymentSettings <- runDB $ selectList [TherapistPrefsTherapist ==. userId] [Desc TherapistPrefsTherapist]
  defaultLayout $ do
    $(widgetFile "/therapist/my-dash")

postMainDashboardR :: UserId -> Handler Html
postMainDashboardR userId = error "Not yet implemented: postMainDashboardR"

paymentSettingsWarning :: [Entity TherapistPrefs] -> Text
paymentSettingsWarning prefList
  | Prelude.null prefList = pack "Warning: No Payment Settings Saved"
  | otherwise     = pack " "
