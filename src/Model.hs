{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Yesod.Form
import Data.Time.Calendar
import Data.Time.LocalTime

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

data FilterChoice =
  FilterChoice
    { therapist :: (Maybe Text)
    , date      :: (Maybe Text)
    , q         :: [Text]
    } deriving (Eq, Show, Read)

instance PathMultiPiece FilterChoice where
   toPathMultiPiece (FilterChoice Nothing (Just date) q) = alltherapists : date : q
   toPathMultiPiece (FilterChoice (Just therapist) Nothing q) = therapist : alldates : q
   toPathMultiPiece (FilterChoice (Just therapist) (Just date) q) = therapist : date : q
   toPathMultiPiece (FilterChoice Nothing Nothing q) = alltherapists : alldates : q
   fromPathMultiPiece (therapist : date: q) = Just $ FilterChoice (Just therapist) (Just date) q
   fromPathMultiPiece _ = Nothing
     --my birthday-using as a means of triggering an 'all-dates' query response

--helper methods
alldates :: Text
alldates = pack $ "alldates"
alltherapists = pack "alltherapists"
