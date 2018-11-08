{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.ChangeUserName where

import Yesod.Form.Bootstrap3
import Import

getChangeUserNameR :: UserId -> Handler Html
getChangeUserNameR userId = do
  user <- runDB $ get404 userId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ userNameForm user
  defaultLayout $ do
    $(widgetFile "/change-my-username")

userNameForm :: User -> AForm Handler UsernameChange
userNameForm user = UsernameChange
                <$> areq textField "" Nothing

postChangeUserNameR :: UserId -> Handler Html
postChangeUserNameR userId = do
  user <- runDB $ get404 userId
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ userNameForm user
  case res of
    FormSuccess usernameChange -> do
      _ <- runDB $ updateWhere [UserId ==. userId] [UserName =. (Just $ usernameChangeNew usernameChange)]
      redirect $ UserDashR
    _ -> defaultLayout $ do
      $(widgetFile "/change-my-username")
