{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.MainDashboard where

import Import

getMainDashboardR :: TherapistChoiceId -> Handler Html
getMainDashboardR therapistChoiceId = do
  newUrl     <- MainDashboardR     therapistChoiceId
  viewUrl    <- ViewApptsR         therapistChoiceId
  paymentUrl <- SetPaymentOptionsR therapistChoiceId
  defaultLayout $ do
    $(widgetFile "zaps/therapist/my-dash")

postMainDashboardR :: Handler Html
postMainDashboardR = error "Not yet implemented: postMainDashboardR"
