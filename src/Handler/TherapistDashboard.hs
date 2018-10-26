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


module Handler.TherapistDashboard where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Data.Time.Calendar

getTherapistDashboardR :: Handler Html
getTherapistDashboardR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $ selectList [] [Desc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/view-appointments")

postTherapistDashboardR :: Handler Html
postTherapistDashboardR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      redirect $ QueryTherapistDashboardR $ filterChoiceTherapist filterChoice
      -- query <- runDB $ get404 ( filterChoiceTherapist filterChoice)
      -- appts <- runDB $ selectList [TherapistAppointmentTherapistName ==. (therapistAppointmentTherapistName query)] [Desc TherapistAppointmentTimeStart]
      -- defaultLayout $ do
      --   $(widgetFile "zaps/therapist/dashboard/view-appointments")
  --     maybeDate <- runDB $ get (filterChoiceDate filterChoice)
  --     queryName <- case maybeName of
  --       Nothing -> error "can't handle that"
  --       Just name -> name
  --     queryDate <- case maybeDate of
  --       Nothing -> error "no date"
  --       Just date -> date


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
