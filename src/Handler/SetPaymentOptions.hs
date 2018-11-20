{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SetPaymentOptions where

import Import
import qualified Database.Esqueleto as E
import qualified Data.Text as T
import Yesod.Form.Bootstrap3

getSetPaymentOptionsR :: UserId -> Handler Html
getSetPaymentOptionsR userId = do
  user <- runDB $ get404 userId
  paymentSettings <- runDB $ selectList [TherapistPrefsTherapist ==. userId] [Desc TherapistPrefsTherapist]
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ paymentForm userId
  defaultLayout $ do
    $(widgetFile"/therapist/dashboard/payment/payment-options")

paymentForm :: UserId -> AForm Handler TherapistPrefs
paymentForm userId = TherapistPrefs
  --TODO: find a way for this to autofill
  <$> pure userId
  <*> areq (multiSelectFieldList paymentOptions) "" Nothing
  <*> areq (multiSelectFieldList basicTiers) "" Nothing

postSetPaymentOptionsR :: UserId -> Handler Html
postSetPaymentOptionsR userId = do
  user <- runDB $ get404 userId
  paymentSettings <- runDB $ selectList [TherapistPrefsTherapist ==. userId] [Desc TherapistPrefsTherapist]
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ paymentForm userId
  case res of
    FormSuccess therapistPrefs -> do
      _ <- runDB $
        updateWhere
          [ TherapistPrefsTherapist ==. userId]
          [ TherapistPrefsPaymentOps =. (therapistPrefsPaymentOps therapistPrefs)
          , TherapistPrefsTiers =. (therapistPrefsTiers therapistPrefs)]
      redirect $ MainDashboardR userId
    _           -> defaultLayout $ do
      $(widgetFile "/therapist/dashboard/payment/payment-options")

paymentSettingsParse :: ([Entity TherapistPrefs], UserId) -> TherapistPrefs
paymentSettingsParse (prefs, userId)
  | Prelude.null prefs = blankPrefs userId
  | otherwise          = entityVal $ Prelude.head prefs

blankPrefs :: UserId -> TherapistPrefs
blankPrefs userId = TherapistPrefs userId [] [blankTier]
                      where blankTier = Tier 0 (T.pack " ")

-- renderPaymentsOps :: Text -> Text
-- renderPaymentsOps op = T.concat $ Prelude.map (\x -> T.concat [x, T.pack " "]) ops

renderTiers :: Tier -> Text
renderTiers tier = T.concat [T.pack $ show $ tierPricePerHour tier, T.pack "-", tierDescription tier, T.pack " "]

--TODO: deprecate this once we've got a tier entry system.
basicTiers :: [(Text, Tier)]
basicTiers =
  [ ("low income - £15/h", Tier 15 "low income")
  , ("average income - £30/h", Tier 30 "average income")
  , ("solidarity - £60/h", Tier 60 "solidarity")
  ]

--payment options
paymentOptions :: [(Text, Text)]
paymentOptions =
  [ ("PayPal", "PayPal")
  , ("Cash"  , "Cash")
  ]
