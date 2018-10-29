{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.SetPaymentOptions where

import Import

getSetPaymentOptionsR :: TherapistChoiceId -> Handler Html
getSetPaymentOptionsR therapistChoiceId = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 Bootstrap3 paymentForm

paymentForm :: AForm Handler TherapistPrefs
paymentForm = TherapistPrefs
  --TODO: find a way for this to autofill
  <$> areq (selectField therapists) "Your name " Nothing
  <*> areq (multiSelectFieldList basicTiers) "Your tiers " Nothing
  <*> areq (multiSelectFieldList paymentOptions) "Your payment options " isNothing



postSetPaymentOptionsR :: TherapistChoiceId -> Handler Html
postSetPaymentOptionsR therapistChoiceId = (do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 Bootstrap3 paymentForm


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
basicTiers :: [(Text, Int)]
basicTiers =
  [ ("low income", 15)
  , ("average income", 30)
  , ("solidarity", 60)
]

paymentOptions :: [Text]
paymentOptions =
  [ "PayPal"
  , "Cash"
  ]
