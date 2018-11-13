{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell   #-}

module Handler.SeeAllUsers where

import Import

getSeeAllUsersR :: Handler Html
getSeeAllUsersR = do
  users <- runDB $ selectList [] [Desc UserEmail]
  defaultLayout $ do
    $(widgetFile "/admin/all-users")

--Not yet in use - will be necessary for any filtering
postSeeAllUsersR :: Handler Html
postSeeAllUsersR = error "Not yet implemented: postSeeAllUsersR"
