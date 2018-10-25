{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Handler.QueryTherapistDashboard where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Data.Time.Calendar

getQueryTherapistDashboardR :: Handler Html
getQueryTherapistDashboardR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $ selectList [] [Desc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/view-appointments")

postQueryTherapistDashboardR :: Handler Html
postQueryTherapistDashboardR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      queryName <- runDB $ get (therapistAppointmentTherapistName $ filterChoiceTherapist filterChoice)
  --     maybeDate <- runDB $ get (filterChoiceDate filterChoice)
  --     queryName <- case maybeName of
  --       Nothing -> error "can't handle that"
  --       Just name -> name
  --     queryDate <- case maybeDate of
  --       Nothing -> error "no date"
  --       Just date -> date
      appts <- runDB $ selectList [TherapistAppointmentTherapistName ==. queryName] [Desc TherapistAppointmentTimeStart]
      defaultLayout $ do
        $(widgetFile "zaps/therapist/dashboard/view-appointments")


filterByForm :: AForm Handler FilterChoice
filterByForm = FilterChoice
   <$> areq (selectField therapists) "Filter by therapist" Nothing
   -- <*> aopt (selectField dates) "Filter by available date" Nothing

therapists :: HandlerFor App (OptionList (Key TherapistAppointment))
therapists = do
  rows <- runDB getTherapists
  optionsPairs $ Prelude.map (\r->((therapistAppointmentTherapistName $ entityVal r), entityKey r)) rows

getTherapists :: (MonadIO m, MonadLogger m)
              => E.SqlReadT m [Entity TherapistAppointment]
getTherapists =
  E.select $
  E.from $ \t ->
  E.distinctOn [E.don (t E.^. TherapistAppointmentTherapistName)] $ do
  return t

dates :: HandlerFor App (OptionList (Key TherapistAppointment))
dates = do
  rows <- runDB getDates
  optionsPairs $ Prelude.map (\r->((fDate $ entityVal r), entityKey r)) rows

--TODO - put this back into dates
fDate :: TherapistAppointment -> Text
fDate app = pack $ show $ therapistAppointmentDate app

getDates :: (MonadIO m, MonadLogger m)
        => E.SqlReadT m [Entity TherapistAppointment]
getDates =
  E.select $
  E.from $ \t ->
  E.distinctOn [E.don (t E.^. TherapistAppointmentDate)] $ do
  return t
