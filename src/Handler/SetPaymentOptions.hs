{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SetPaymentOptions where

import Import
import qualified Database.Esqueleto as E
import Yesod.Form.Bootstrap3

getSetPaymentOptionsR :: UserId -> Handler Html
getSetPaymentOptionsR userId = do
  user <- runDB $ get404 userId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ paymentForm userId
  defaultLayout $ do
    $(widgetFile"/therapist/dashboard/payment/payment-options")

paymentForm :: UserId -> AForm Handler TherapistPrefs
paymentForm userId = TherapistPrefs
  --TODO: find a way for this to autofill
  <$> pure userId
  <*> areq (multiSelectFieldList paymentOptions) "Your payment options " Nothing
  <*> areq (multiSelectFieldList basicTiers) "Your tiers " Nothing

postSetPaymentOptionsR :: UserId -> Handler Html
postSetPaymentOptionsR userId = do
  user <- runDB $ get404 userId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ paymentForm userId
  case res of
    FormSuccess therapistPrefs ->do
      _ <- runDB $ insert therapistPrefs
      redirect $ MainDashboardR userId
    _           -> defaultLayout $ do
      $(widgetFile "/therapist/dashboard/payment/payment-options")

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
