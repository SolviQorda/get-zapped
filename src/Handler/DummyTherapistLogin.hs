{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.DummyTherapistLogin where

import Import
import qualified Database.Esqueleto as E
import Yesod.Form.Bootstrap3

getDummyTherapistLoginR :: Handler Html
getDummyTherapistLoginR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm therapistForm
  defaultLayout $ do
    $(widgetFile "zaps/therapist/login")

therapistForm :: AForm Handler TherapistChoice
therapistForm = TherapistChoice
  <$> areq (selectField therapists) "" Nothing

postDummyTherapistLoginR :: Handler Html
postDummyTherapistLoginR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm therapistForm
  case res of
    FormSuccess therapistChoice -> do
      therapistChoiceId <- runDB $ insert therapistChoice
      redirect $ MainDashboardR $ therapistChoiceId
    _ -> defaultLayout $ do
      $(widgetFile "zaps/therapist/login")

therapists = do
 rows <- runDB getTherapists
 optionsPairs $ Prelude.map (\r->((therapistChoiceTherapist $ entityVal r), therapistChoiceTherapist $ entityVal r)) rows

getTherapists :: (MonadIO m, MonadLogger m)
              => E.SqlReadT m [Entity TherapistChoice]
getTherapists =
 E.select $
 E.from $ \t ->
 E.distinctOn [E.don (t E.^. TherapistChoiceTherapist)] $ do
 return t
