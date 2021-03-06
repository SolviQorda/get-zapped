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

module Handler.FilterAppts where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E
import Data.Time.LocalTime

getFilterApptsR :: UserId -> FilterChoice -> Handler Html
getFilterApptsR userId filterChoice = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  appts <- runDB $ selectList [TherapistAppointmentDate >=. today] [Asc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/view/filter/filter-therapist")

handleQueries :: (PersistQueryRead (YesodPersistBackend site)
                 , YesodPersist site
                 , BaseBackend (YesodPersistBackend site) ~ SqlBackend)
                 => FilterChoice
                 -> UserId
                 -> HandlerFor site [Entity TherapistAppointment]
handleQueries filterChoice userId
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

postFilterApptsR :: UserId -> FilterChoice -> Handler Html
postFilterApptsR userId filterChoice = do
  now <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let zoneNow = utcToLocalTime timezone now
  let today = localDay zoneNow
  appts <- runDB $ selectList [TherapistAppointmentDate >=. today] [Asc TherapistAppointmentDate]
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      redirect $ FilterApptsR userId (filterChoice xs)
    _ -> defaultLayout $ do
      $(widgetFile "/therapist/dashboard/view/filter/filter-therapist")

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

--change button text dependent on status
confirmBtnText :: Maybe Bool -> Maybe Text -> Text
confirmBtnText confirm booked
  | confirm == Nothing && (booked /= Nothing) = "Confirm"
  | confirm == Nothing = "      "
  | confirm == Just True = "Cancel"
  | otherwise = "      "

-- want to change CSS dependent on whether the user has booked the appt
confirmBtnAppearance :: Maybe Text -> Text
confirmBtnAppearance booked
  | booked == Nothing = "btn btn-unbooked"
  | otherwise         = "btn btn-default"
