{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.MainDashboard where

import Import

getMainDashboardR :: TherapistChoiceId -> Handler Html
getMainDashboardR therapistChoiceId = do
  defaultLayout $ do
    $(widgetFile "zaps/therapist/my-dash")

postMainDashboardR :: TherapistChoiceId -> Handler Html
postMainDashboardR therapistChoiceId = error "Not yet implemented: postMainDashboardR"
