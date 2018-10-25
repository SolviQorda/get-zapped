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
    $(widgetFile "zaps/new/choose-therapist")

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
        -- defaultLayout $(widgetFile "zaps/new/choose-therapist")

        -- postAdminAddTherapistR :: Handler Html
        -- postAdminAddTherapistR = do
        --   ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm addTherapistForm
        --   case res of
        --     FormSuccess therapistChoice -> do
        --       therapistChoiceId <- runDB $ insert therapistChoice
        --       redirect HomeR
        --     _ -> defaultLayout $ do
        --       $(widgetFile "admin/add-therapist")


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
