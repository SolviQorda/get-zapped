{-# LANGUAGE FlexibleContexts  #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell   #-}

module Handler.AuthenticateTherapist where

import Import
import Yesod.Form.Bootstrap3

getAuthenticateTherapistR :: Handler Html
getAuthenticateTherapistR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ selectUserForm
  therapists <- runDB $ selectList [UserIsTherapist ==. True] [Desc UserEmail]
  defaultLayout $ do
    $(widgetFile "/admin/authenticate-therapist")

selectUserForm :: AForm Handler UserChoice
selectUserForm = UserChoice
        <$> areq (selectField $ userEmails) "" Nothing

-- userEmails :: HandlerFor App (OptionList (Key User))
userEmails = do
  users <- runDB $ selectList [UserIsTherapist ==. False] [Desc UserEmail]
  optionsPairs $ Prelude.map (\r -> ((userEmail $ entityVal r), userEmail $ entityVal r)) users

postAuthenticateTherapistR :: Handler Html
postAuthenticateTherapistR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ selectUserForm
  case res of
    FormSuccess userChoice -> do
      _ <- runDB $ updateWhere [UserEmail ==. userChoiceEmail userChoice] [UserIsTherapist =. True]
      redirect AdminDashR
    _ -> redirect HomeR
