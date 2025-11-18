module Main where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.React (HTML, renderComponent)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import ReactDOM as ReactDOM
import UI (initialModel, runUI)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.Window (document) as DOM

hydrateWidgetInDom :: forall a. String -> Widget HTML a -> Effect Unit
hydrateWidgetInDom elemId w = do
  doc <- DOM.document =<< DOM.window
  mroot <- DOM.getElementById elemId $ DOM.toNonElementParentNode doc
  case mroot of
    Nothing -> pure unit
    Just root -> void $ ReactDOM.hydrate (renderComponent w) root

main :: Effect Unit
main = hydrateWidgetInDom "root" $ runUI initialModel
