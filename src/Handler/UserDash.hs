{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.UserDash where

import Import

getUserDashR :: Handler Html
getUserDashR = do
  (authId, user) <- requireAuthPair
  appts <- runDB $ selectList [TherapistAppointmentBookedByEmail ==. (Just $ userEmail user)] [Desc TherapistAppointmentDate]
  case (userIsTherapist user) of
    True -> do
      -- error (show $ userId user)
      redirect $ MainDashboardR $ authId
    False -> do
      defaultLayout $ do
        $(widgetFile "my-dashboard")

postUserDashR :: Handler Html
postUserDashR = error "Not yet implemented: postUserDashR"
