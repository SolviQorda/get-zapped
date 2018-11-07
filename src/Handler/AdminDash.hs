{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}

module Handler.AdminDash where

import Import
import Yesod.Form.Bootstrap3

getAdminDashR :: Handler Html
getAdminDashR = do
  -- (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $ selectList [] [Desc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "admin/admin-dashboard")

postAdminDashR :: Handler Html
postAdminDashR = error "Not yet implemented: postAdminDashR"
