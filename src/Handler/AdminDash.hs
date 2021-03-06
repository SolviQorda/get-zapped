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

module Handler.AdminDash where

import Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Data.Time.LocalTime

getAdminDashR :: Handler Html
getAdminDashR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  appts <- runDB $ selectList [TherapistAppointmentDate >=. today] [Asc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "admin/admin-dashboard")

postAdminDashR :: Handler Html
postAdminDashR = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  appts <- runDB $ selectList [TherapistAppointmentDate >=. today] [Asc TherapistAppointmentDate]
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      redirect $ AdminFilterApptsR (filterChoice xs)
    _ ->   defaultLayout $(widgetFile "/admin/admin-dashboard")

filterByForm :: AForm Handler ([Text] -> FilterChoice)
filterByForm = FilterChoice
   <$> aopt (selectField therapists) "Filter by therapist" Nothing
   <*> aopt (selectField dates) "Filter by available date" Nothing

--empty list placeholder necessary for multiPath route
xs :: [Text]
xs = []

therapists :: HandlerFor App (OptionList Text)
therapists = do
 rows <- runDB getTherapists
 optionsPairs $ Prelude.map (\r->((therapistAppointmentTherapistName $ entityVal r), therapistAppointmentTherapistName $ entityVal r)) rows

--get appts with distinct therapist names from the db.
getTherapists :: (MonadIO m, MonadLogger m)
             => E.SqlReadT m [Entity TherapistAppointment]
getTherapists =
 E.select $
 E.from $ \t ->
 E.distinctOn [E.don (t E.^. TherapistAppointmentTherapistName)] $ do
 return t

dates :: HandlerFor App (OptionList Text)
dates = do
 rows <- runDB getDates
 optionsPairs $ Prelude.map (\r->((fDate $ entityVal r), fDate $ entityVal r)) rows

--get the date as Text
fDate :: TherapistAppointment -> Text
fDate app = pack $ show $ therapistAppointmentDate app

--all distinct dates
getDates :: (MonadIO m, MonadLogger m)
       => E.SqlReadT m [Entity TherapistAppointment]
getDates =
 E.select $
 E.from $ \t ->
 E.distinctOn [E.don (t E.^. TherapistAppointmentDate)] $ do
 return t
