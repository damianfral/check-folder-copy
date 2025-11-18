module PickFiles (pickDirectoryWidget) where

import Prelude hiding (div)

import Concur.Core.Types (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, p, text)
import Concur.React.Props (className, onClick, onDragEnd, onDragLeave, onDragOver, onDrop, onDropCapture, onMouseOut)
import Control.Alternative ((<|>))
import Control.Promise (toAff, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FFI.JSInterface (FSDirectoryHandle, getAsFileSystemHandle, getDataTransfer, showDirectoryPicker)
import React.SyntheticEvent (SyntheticMouseEvent)
import React.SyntheticEvent as SyntheticEvent
import Web.HTML.Event.DataTransfer (DropEffect(..), items, setDropEffect) as DT
import Web.HTML.Event.DataTransfer.DataTransferItem (dataTransferItem) as DT

data FileAction
  = OpenFilePicker
  | DragEnter SyntheticMouseEvent
  | DragLeave
  | Drop SyntheticMouseEvent

-- | A widget that allows the user to pick a directory
pickDirectoryWidget :: Boolean -> Widget HTML FSDirectoryHandle
pickDirectoryWidget isDragging = do
  action <- div
    [ className cName
    , OpenFilePicker <$ onClick
    , Drop <$> onDrop
    , Drop <$> onDropCapture
    , DragEnter <$> onDragOver
    , DragLeave <$ onDragLeave
    , DragLeave <$ onDragEnd
    , DragLeave <$ onMouseOut

    ]
    [ p [ className "text-center" ] [ text msg ] ]
  case action of
    DragEnter ev -> do
      liftEffect $ do
        SyntheticEvent.preventDefault ev
        SyntheticEvent.stopPropagation ev
        let dt = getDataTransfer ev
        DT.setDropEffect DT.Copy dt
      pickDirectoryWidget true
    DragLeave -> pickDirectoryWidget false
    Drop ev -> do
      liftEffect $ SyntheticEvent.preventDefault ev
      let dt = getDataTransfer ev
      liftEffect $ DT.setDropEffect DT.Copy dt
      let mItem = DT.dataTransferItem 0 $ DT.items dt
      case mItem of
        Nothing -> pickDirectoryWidget false
        Just item -> liftAff $ toAff $ getAsFileSystemHandle item
    OpenFilePicker -> do
      result <-
        handleDirectoryPicker <|>
          div [ className cName ] [ p [ className "text-center" ] [ text msg ] ]
      case result of
        Left e -> liftEffect (log $ show e) *> pickDirectoryWidget false
        Right dirHandle -> pure dirHandle
  where
  msg =
    """
    Drop a directory here or click to pick
    """
  cName =
    """
    p-4 px-6 bg-indigo-100 border-2 border-dashed w-full h-full
    text-stone-900 rounded-lg cursor-pointer flex justify-center items-center
    """ <> if isDragging then " border-orange-500" else " border-stone-900"
  handleDirectoryPicker = liftAff $ attempt $ toAffE showDirectoryPicker

