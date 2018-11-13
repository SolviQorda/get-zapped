{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.MainDashboard where

import Import

getMainDashboardR :: UserId -> Handler Html
getMainDashboardR userId = do
  defaultLayout $ do
    $(widgetFile "/therapist/my-dash")

--Not in use yet, therapist dash structure direct to separate pages with routes.
postMainDashboardR :: UserId -> Handler Html
postMainDashboardR userId = error "Not yet implemented: postMainDashboardR"
