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
  appts <- runDB $ selectList [TherapistAppointmentBookedByEmail ==. (Just $ userEmail user)] [Asc TherapistAppointmentDate]
  case (userIsTherapist user) of
    True -> do
      -- error (show $ userId user)
      redirect $ MainDashboardR $ authId
    False -> do
      defaultLayout $ do
        $(widgetFile "my-dashboard")

--Currently user dashboard directs to services via routes, this may change if UI shifts to one progressive page
postUserDashR :: Handler Html
postUserDashR = error "Not yet implemented: postUserDashR"
