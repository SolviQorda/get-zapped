{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell   #-}

module Handler.SeeAllUsers where

import Import

getSeeAllUsersR :: Handler Html
getSeeAllUsersR = do
  users <- runDB $ selectList [] [Desc UserEmail]
  defaultLayout $ do
    $(widgetFile "/admin/all-users") 

postSeeAllUsersR :: Handler Html
postSeeAllUsersR = error "Not yet implemented: postSeeAllUsersR"
