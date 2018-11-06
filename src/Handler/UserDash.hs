{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserDash where

import Import

getUserDashR :: Handler Html
getUserDashR = do
  (_, user) <- requireAuthPair
  appts <- runDB $ selectList [TherapistAppointmentBookedByEmail ==. (Just $ userEmail user)] [Desc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "my-dashboard")

postUserDashR :: Handler Html
postUserDashR = error "Not yet implemented: postUserDashR"
