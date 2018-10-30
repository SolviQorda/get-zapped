{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SetPaymentOptions where

import Import
import qualified Database.Esqueleto as E
import Yesod.Form.Bootstrap3

getSetPaymentOptionsR :: TherapistChoiceId -> Handler Html
getSetPaymentOptionsR therapistChoiceId = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm paymentForm
  defaultLayout $ do
    $(widgetFile"zaps/therapist/dashboard/payment/payment-options")

paymentForm :: AForm Handler TherapistPrefs
paymentForm = TherapistPrefs
  --TODO: find a way for this to autofill
  <$> areq (selectField therapists) "Your name " Nothing
  <*> areq (multiSelectFieldList paymentOptions) "Your payment options " Nothing
  <*> areq (multiSelectFieldList basicTiers) "Your tiers " Nothing

postSetPaymentOptionsR :: TherapistChoiceId -> Handler Html
postSetPaymentOptionsR therapistChoiceId = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm paymentForm
  case res of
    FormSuccess therapistPrefs ->do
      _ <- runDB $ insert therapistPrefs
      redirect $ MainDashboardR therapistChoiceId
    _           -> defaultLayout $ do
      $(widgetFile "zaps/therapist/dashboard/payment/payment-options")

--TODO: remove in favour of autofilling based on id.
therapists = do
 rows <- runDB getTherapists
 optionsPairs $ Prelude.map (\r->((therapistChoiceTherapist $ entityVal r), therapistChoiceTherapist $ entityVal r)) rows

getTherapists :: (MonadIO m, MonadLogger m)
              => E.SqlReadT m [Entity TherapistChoice]
getTherapists =
 E.select $
 E.from $ \t ->
 E.distinctOn [E.don (t E.^. TherapistChoiceTherapist)] $ do
 return t

--deprecate this once you've got a tier entry system.
basicTiers :: [(Text, Tier)]
basicTiers =
  [ ("low income - £15/h", Tier 15 "low income")
  , ("average income - £30/h", Tier 30 "average income")
  , ("solidarity - £60/h", Tier 60 "solidarity")
  ]

paymentOptions :: [(Text, Text)]
paymentOptions =
  [ ("PayPal", "PayPal")
  , ("Cash"  , "Cash")
  ]
