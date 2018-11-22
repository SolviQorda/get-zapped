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

module Handler.ViewHistoric where

import Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Data.Time.LocalTime

getViewHistoricR :: UserId -> Handler Html
getViewHistoricR userId = do
  therapist <- runDB $ get404 userId
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $
            selectList [ TherapistAppointmentDate <. today
                       , TherapistAppointmentTherapistName ==. (fromMaybe (userEmail therapist) $ userName therapist)]
            [Asc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/view/historic/view-historic")

postViewHistoricR :: UserId -> Handler Html
postViewHistoricR userId = do
  therapist <- runDB $ get404 userId
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $
            selectList [ TherapistAppointmentDate <. today
                       , TherapistAppointmentTherapistName ==. (fromMaybe (userEmail therapist) $ userName therapist)]
            [Asc TherapistAppointmentDate]
  case res of
    FormSuccess filterChoice -> do
      redirect $ FilterApptsR userId (filterChoice xs)
    _ ->   defaultLayout $(widgetFile "/therapist/dashboard/view/historic/view-historic")

--empty list placeholder necessary for multiPath route
xs :: [Text]
xs = []

filterByForm :: AForm Handler ([Text] -> FilterChoice)
filterByForm = FilterChoice
   <$> aopt (selectField therapists) "Filter by therapist" Nothing
   <*> aopt (selectField dates) "Filter by available date" Nothing

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
  rows <- runDB $ getDates
  optionsPairs $ Prelude.map (\r->((fDate $ entityVal r), fDate $ entityVal r)) rows

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

-- getDates' :: (MonadIO m, MonadLogger m)
--     => Day
--     -> E.SqlReadT m [Entity TherapistAppointment]
-- getDates' today =
--   E.select $
--   E.from $ \t ->
--   E.distinctOn [E.don (t E.^. TherapistAppointmentDate)] $ do
--   E.where_ (t E.^. TherapistAppointmentDate E.<. E.val today)
--   return t
