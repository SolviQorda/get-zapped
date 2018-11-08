{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.ChooseTherapist where

import Import
import Yesod.Form
import Yesod.Form.Bootstrap3
import qualified Database.Esqueleto as E

getChooseTherapistR :: Handler Html
getChooseTherapistR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm therapistForm
  defaultLayout $ do
    $(widgetFile "/new/choose-therapist")

therapistForm :: AForm Handler TherapistChoice
therapistForm = TherapistChoice
  <$> areq (selectField therapists) "" Nothing

postChooseTherapistR :: Handler Html
postChooseTherapistR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm therapistForm
  case res of
    FormSuccess therapistChoice -> do
      therapistChoiceId <- runDB $ insert therapistChoice
      redirect $ BookZapR $ therapistChoiceTherapist therapistChoice
    _ -> redirect HomeR

therapists = do
 rows <- runDB $ selectList [UserIsTherapist ==. True] [Desc UserId]
 optionsPairs $ Prelude.map (\r->((fromMaybe "no username set" $ userName $ entityVal r), fromMaybe "no username set" $ userName $ entityVal r)) rows

-- getTherapists :: (MonadIO m, MonadLogger m)
--               => E.SqlReadT m [Entity TherapistChoice]
-- getTherapists =
--  E.select $
--  E.from $ \t ->
--  E.distinctOn [E.don (t E.^. TherapistChoiceTherapist)] $ do
--  return t
