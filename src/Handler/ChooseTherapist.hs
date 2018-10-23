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
    $(widgetFile "zaps/choose-therapist")

therapistForm :: AForm Handler TherapistChoice
therapistForm = TherapistChoice
  <$> areq (selectField therapists) "" Nothing

postChooseTherapistR :: Handler Html
postChooseTherapistR = do
  ((res, widget), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm therapistForm
  case res of
    FormSuccess choice -> do
        defaultLayout $(widgetFile "zaps/choose-therapist")
      -- redirect Prelude.. BookZapR $ therapist choice
    _ -> defaultLayout $(widgetFile "zaps/choose-therapist")

--TODO refactor as a query of available therapists
-- therapists :: [(Text, Text)]
-- therapists = [ ("Siobhan Reilly - Electrolysis by Siobhan","Siobhan")
--              , ("Sam Turner - Tortoise Beats Hair", "Sam")
--              ]

therapists :: HandlerFor App (OptionList (Key TherapistChoice))
therapists = do
 rows <- runDB getTherapists
 optionsPairs $ Prelude.map (\r->((therapistChoiceTherapist $ entityVal r), entityKey r)) rows

getTherapists :: (MonadIO m, MonadLogger m)
              => E.SqlReadT m [Entity TherapistChoice]
getTherapists =
 E.select $
 E.from $ \t ->
 E.distinctOn [E.don (t E.^. TherapistChoiceTherapist)] $ do
 return t
