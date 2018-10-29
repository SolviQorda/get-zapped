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

getFilterApptsR :: FilterChoice -> Handler Html
getQueryTherapistDashboardR filterChoice = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- handleQueries filterChoice
  defaultLayout $ do
    $(widgetFile "zaps/therapist/dashboard/filter/filter-therapist")

handleQueries :: (PersistQueryRead (YesodPersistBackend site)
                 , YesodPersist site
                 , BaseBackend (YesodPersistBackend site) ~ SqlBackend)
                 => FilterChoice
                 -> HandlerFor site [Entity TherapistAppointment]
handleQueries filterChoice
  | queryTherapist == pht && queryDate == phd = runDB $ selectList [] [Desc TherapistAppointmentTimeStart]
  | queryTherapist == pht && queryDate /= phd = runDB $ selectList [TherapistAppointmentDate ==. (getDate $ Just queryDate)] [Desc TherapistAppointmentTimeStart]
  | queryTherapist /= pht && queryDate == phd = runDB $ selectList [TherapistAppointmentTherapistName ==. queryTherapist] [Desc TherapistAppointmentTimeStart]
  | otherwise                                 = runDB $ selectList [TherapistAppointmentTherapistName ==. queryTherapist, TherapistAppointmentDate ==. (getDate $ Just queryDate)] [Desc TherapistAppointmentTimeStart]
    where queryTherapist = fromMaybe (pack "error") $ therapist filterChoice
          queryDate      = fromMaybe (pack "error") $ date filterChoice
          pht            = pack "alltherapists"
          phd            = pack "alldates"

getDate :: (Maybe Text) -> Day
getDate t = fromMaybe (toEnum 47993) $ parseTimeM True defaultTimeLocale "%Y-%-m-%-d" day
  where day = unpack $ fromMaybe (pack "error") t

postFilterApptsR :: FilterChoice -> Handler Html
postQueryTherapistDashboardR filterChoice = do
  appts <- handleQueries filterChoice
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      redirect $ FilterApptsR (filterChoice xs)
    _ -> defaultLayout $ do
      $(widgetFile "zaps/therapist/dashboard/filter/filter-therapist")

--empty list necessary for multi part route.
xs :: [Text]
xs = []

filterByForm :: AForm Handler ([Text] -> FilterChoice)
filterByForm = FilterChoice
   <$> aopt (selectField therapists) "Filter by therapist" Nothing
   <*> aopt (selectField dates) "Filter by available date" Nothing

therapists :: HandlerFor App (OptionList Text)
therapists = do
  rows <- runDB getTherapists
  optionsPairs
    $ Prelude.map
      (\r->((therapistAppointmentTherapistName $ entityVal r)
      , therapistAppointmentTherapistName $ entityVal r)) rows

--give a list of distinct therapists in the db (esqueleto)
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
  optionsPairs $ Prelude.map (\r->(fDate $ entityVal r, fDate $ entityVal r)) rows

--parse the date for rendering
--TODO - put this back into dates
fDate :: TherapistAppointment -> Text
fDate app = pack $ show $ therapistAppointmentDate app

--give a list of distinct dates in the db (esqueleto)
getDates :: (MonadIO m, MonadLogger m)
        => E.SqlReadT m [Entity TherapistAppointment]
getDates =
  E.select $
  E.from $ \t ->
  E.distinctOn [E.don (t E.^. TherapistAppointmentDate)] $ do
  return t
