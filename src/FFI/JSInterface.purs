module FFI.JSInterface where

import Control.Promise (Promise)
import Effect (Effect)
import React.SyntheticEvent (SyntheticMouseEvent)
import Web.File.File (File)
import Web.HTML.Event.DataTransfer (DataTransfer)
import Web.HTML.Event.DataTransfer.DataTransferItem (DataTransferItem)

data FSDirectoryHandle
data FSFileHandle

foreign import showDirectoryPicker :: Effect (Promise FSDirectoryHandle)
-- foreign import getDataTransfer :: SyntheticMouseEvent -> DataTransfer

-- foreign import showModal :: Ref NativeNode -> Effect Unit
-- foreign import closeModal :: Ref NativeNode -> Effect Unit

foreign import getDirectoryHandleName :: FSDirectoryHandle -> String
foreign import fullPath :: FSDirectoryHandle -> String

foreign import getDirectoryHandle
  :: FSDirectoryHandle -> String -> Promise FSDirectoryHandle

foreign import getDirectoryContents
  :: FSDirectoryHandle
  -> Effect
       ( Promise
           { directories :: Array FSDirectoryHandle
           , files :: Array FSFileHandle
           }
       )

foreign import getFileHandleName :: FSFileHandle -> String
foreign import getFileHandle
  :: FSDirectoryHandle -> String -> Promise FSFileHandle

foreign import getFileFromHandle :: FSFileHandle -> Effect (Promise File)

foreign import getDataTransfer :: SyntheticMouseEvent -> DataTransfer
foreign import getAsFileSystemHandle :: DataTransferItem -> Promise FSDirectoryHandle

