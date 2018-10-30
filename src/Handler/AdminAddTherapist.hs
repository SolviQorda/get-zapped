{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.AdminAddTherapist where

import Import
import Yesod.Form.Bootstrap3

getAdminAddTherapistR :: Handler Html
getAdminAddTherapistR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm addTherapistForm
  defaultLayout $ do
    $(widgetFile "admin/add-therapist")

addTherapistForm :: AForm Handler TherapistChoice
addTherapistForm = TherapistChoice
                <$> areq textField "" Nothing

postAdminAddTherapistR :: Handler Html
postAdminAddTherapistR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addTherapistForm
  case res of
    FormSuccess therapistChoice -> do
      _ <- runDB $ insert therapistChoice
      redirect HomeR
    _ -> defaultLayout $ do
      $(widgetFile "admin/add-therapist")
