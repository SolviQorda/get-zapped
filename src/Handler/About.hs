{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.About where

import Import

--simple about page
getAboutR :: Handler Html
getAboutR = do
  defaultLayout $ do
      $(widgetFile "about")
