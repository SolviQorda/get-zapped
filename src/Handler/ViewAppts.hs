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

module Handler.ViewAppts where

import Import
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E

getViewApptsR :: UserId -> Handler Html
--TODO:default to chosen therapist with filter
getViewApptsR userId = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  appts <- runDB $ selectList [] [Desc TherapistAppointmentDate]
  defaultLayout $ do
    $(widgetFile "/therapist/dashboard/view/view-appointments")

postViewApptsR ::  UserId -> Handler Html
--TODO:default to chosen therapist with filter
postViewApptsR userId = do
  appts <- runDB $ selectList [] [Desc TherapistAppointmentDate]
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm filterByForm
  case res of
    FormSuccess filterChoice -> do
      redirect $ FilterApptsR userId (filterChoice xs)
    _ ->   defaultLayout $(widgetFile "/therapist/dashboard/view/view-appointments")

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
  rows <- runDB getDates
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

--button text dependent on whether booking has been confirmed
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
