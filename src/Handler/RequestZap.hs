module Handler.RequestZap where

import Import
import Yesod.Form.Bootstrap3
import Text.Markdown (Markdown)
import Yesod.Text.Markdown

getRequestZapR :: Handler Html
getRequestZapR = do
  defaultLayout $ do
    $(widgetFile "zaps/index")

postRequestZapR :: Handler Html
postRequestZapR = do
  defaultLayout $ do
    $(widgetFile "zaps/index")
