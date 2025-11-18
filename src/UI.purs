module UI where

import Prelude hiding (div)

import Concur.Core (Widget)
import Concur.Core.Props (Props(..))
import Concur.React (HTML)
import Concur.React.DOM (button, div, h1, h2, input, label, p, pre, text)
import Concur.React.Props (_type, checked, className, disabled, onChange, onClick)
import Concur.React.Props as A
import Control.Alternative ((<|>))
import Control.Promise (toAffE)
import Data.Array (filter, foldMap, intercalate, null)
import Data.BigInt as BI
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Int (round)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive)
import Data.Newtype (modify, unwrap, wrap)
import Data.String (take)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import FFI.JSInterface (FSDirectoryHandle, getDirectoryContents, getDirectoryHandleName, getFileFromHandle, getFileHandleName)
import PickFiles (pickDirectoryWidget)
import React.DOM.Props as R
import Web.File.File as F

-- | The model for the UI
data Model
  = Start ModelStart
  | Processing ModelProcessing
  | Finished ModelFinished

initialModel :: Model
initialModel =
  Start { leftFolder: Nothing, rightFolder: Nothing, skipHidden: true }

type ModelStart =
  { leftFolder :: Maybe FSDirectoryHandle
  , rightFolder :: Maybe FSDirectoryHandle
  , skipHidden :: Boolean
  }

type ModelProcessing =
  { leftFolder :: FSDirectoryHandle
  , rightFolder :: FSDirectoryHandle
  , skipHidden :: Boolean
  }

type ModelFinished =
  { leftFolder :: FSDirectoryHandle
  , rightFolder :: FSDirectoryHandle
  , leftTree :: Node
  , rightTree :: Node
  , skipHidden :: Boolean
  }

type FileInfo = { name :: String, path :: String, size :: BI.BigInt }

type DirectoryInfo = { name :: String, path :: String }

data Node
  = Dir DirectoryInfo (Array Node)
  | F FileInfo
  | EN

derive instance Eq Node

type Result = { leftTree :: Node, rightTree :: Node }

type Stats =
  { directories :: Additive BI.BigInt
  , files :: Additive BI.BigInt
  , bytes :: Additive BI.BigInt
  }

-- | Traverse a directory recursively, returning a Node tree
buildTree :: Boolean -> FSDirectoryHandle -> Aff Node
buildTree skipHidden = traverseDir true ""
  where
  isHidden str = take 1 str == "."
  traverseDir isRoot basePath dirHandle = do
    { directories, files } <- toAffE $ getDirectoryContents dirHandle
    let dirName = getDirectoryHandleName dirHandle
    if not isRoot && skipHidden && isHidden dirName then pure EN
    else do
      let basePath' = if basePath == "" then "" else basePath <> "/"
      let currentPath = basePath' <> dirName
      -- Process files
      fileNodes <- for files \fh -> do
        if skipHidden && isHidden (getFileHandleName fh) then pure EN
        else do
          file <- toAffE $ getFileFromHandle fh
          let fName = F.name file
          let filePath = currentPath <> "/" <> fName
          pure $ F
            { name: fName
            , path: filePath
            , size: BI.fromInt $ round $ F.size file
            }
      -- Recurse into directories
      dirNodes <- for directories (traverseDir false currentPath)
      pure $ Dir { name: dirName, path: currentPath } $ dirNodes <> fileNodes

treeStats :: Node -> Stats
treeStats EN = mempty
treeStats (F { size }) =
  { directories: wrap $ BI.fromInt 0
  , files: wrap $ BI.fromInt 1
  , bytes: wrap size
  }
treeStats (Dir _ nodes) =
  { directories: wrap $ BI.fromInt 1
  , files: wrap $ BI.fromInt 0
  , bytes: wrap $ BI.fromInt 0
  } <> foldMap treeStats nodes

-- | Render a Node tree as a string
renderTreeString :: Node -> String
renderTreeString node = go 0 node
  where
  indent depth = fold (replicate depth "  " :: Array String)

  go :: Int -> Node -> String
  go depth n = case n of
    EN -> ""
    F f -> indent depth <> f.name <> " (" <> BI.toString f.size <> " bytes)"
    Dir d children ->
      let
        header = indent depth <> d.name <> "/"
        nonEmptyChildren = filter (_ /= EN) children
        childrenStr = intercalate "\n" $ map (go $ depth + 1) nonEmptyChildren
      in
        if null children then header else header <> "\n" <> childrenStr

setLeftFolder :: Model -> FSDirectoryHandle -> Model
setLeftFolder (Start model) folder = Start model { leftFolder = Just folder }
setLeftFolder (Finished model) folder =
  Start
    { leftFolder: Just folder
    , rightFolder: Just model.rightFolder
    , skipHidden: model.skipHidden
    }
setLeftFolder m _folder = m

setRightFolder :: Model -> FSDirectoryHandle -> Model
setRightFolder (Start model) folder = Start model { rightFolder = Just folder }
setRightFolder (Finished model) folder =
  Start
    { leftFolder: Just model.leftFolder
    , rightFolder: Just folder
    , skipHidden: model.skipHidden
    }
setRightFolder m _folder = m

setSkipHidden :: Model -> Boolean -> Model
setSkipHidden (Start model) skipHidden =
  Start $ model { skipHidden = skipHidden }
setSkipHidden (Processing model) skipHidden =
  Processing $ model { skipHidden = skipHidden }
setSkipHidden (Finished model) skipHidden =
  Finished $ model { skipHidden = skipHidden }

getSkipHidden :: Model -> Boolean
getSkipHidden (Start model) = model.skipHidden
getSkipHidden (Processing model) = model.skipHidden
getSkipHidden (Finished model) = model.skipHidden

getLeftFolder :: Model -> Maybe FSDirectoryHandle
getLeftFolder (Start model) = model.leftFolder
getLeftFolder (Processing model) = Just model.leftFolder
getLeftFolder (Finished model) = Just model.leftFolder

getRightFolder :: Model -> Maybe FSDirectoryHandle
getRightFolder (Start model) = model.rightFolder
getRightFolder (Processing model) = Just model.rightFolder
getRightFolder (Finished model) = Just model.rightFolder

getLeftTree :: Model -> Maybe Node
getLeftTree (Start _) = Nothing
getLeftTree (Processing _) = Nothing
getLeftTree (Finished model) = Just model.leftTree

getRightTree :: Model -> Maybe Node
getRightTree (Start _) = Nothing
getRightTree (Processing _) = Nothing
getRightTree (Finished model) = Just model.rightTree

runUI :: Model -> Widget HTML Model
runUI model = runUI =<< div [ className "w-full grid grid-cols-3 gap-4 p-8" ]
  [ div [ className folderColClass ]
      [ div [ className "h-80 flex flex-col gap-4" ]
          [ setLeftFolder model <$> folderSection "Left Folder" ]
      , renderFolder $ getLeftFolder model
      , maybe mempty renderTree $ getLeftTree model
      ]
  , div
      [ className
          "flex flex-col gap-4 p-4 justify-start items-center text-center"
      ]
      [ div [ className "h-80 flex flex-col gap-4 justify-start text-center" ]
          [ h1 [ className "text-2xl font-bold" ] [ text "Check Folder Copy" ]
          , p []
              [ text
                  """
                  This application checks that both folder structures are
                  identical and all files exist in both folders with matching
                  names and sizes. It does not check the contents of the files."
                  """
              ]
          , compareMenu model
          ]
      , case Tuple <$> getLeftTree model <*> getRightTree model of
          Nothing -> mempty
          Just (Tuple lTree rTree) -> renderDiff lTree rTree
      ]
  , div [ className folderColClass ]
      [ div [ className "h-80 flex flex-col gap-4" ]
          [ setRightFolder model <$> folderSection "Right Folder" ]
      , renderFolder $ getRightFolder model
      , maybe mempty renderTree $ getRightTree model
      ]
  ]

folderSection :: String -> Widget HTML FSDirectoryHandle
folderSection title =
  div [ className "flex flex-col gap-4 h-full" ]
    [ h2 [ className "font-semibold text-center" ] [ text title ]
    , pickDirectoryWidget false
    ]

compareMenu :: Model -> Widget HTML Model
compareMenu (Start model@{ leftFolder, rightFolder, skipHidden }) = do
  let mTuple = Tuple <$> leftFolder <*> rightFolder
  result <- formW mTuple
  pure $ case result of
    Left b -> setSkipHidden (Start model) b
    Right (Tuple l r) ->
      Processing { leftFolder: l, rightFolder: r, skipHidden }
  where
  formW mTuple = do
    div [ className "flex flex-col gap-4 justify-center items-center" ]
      [ div [ className "flex gap-2 items-center" ]
          [ Left <$> input
              [ A.name "skipHidden"
              , _type "checkbox"
              , checked skipHidden
              , onChange $> not skipHidden
              , className "h-5 w-5 rounded text-blue-500 focus:ring-blue-400"
              ]
          , label [ PrimProp <<< R.unsafeMkProps "for" $ "skipHidden" ]
              [ text "Skip hidden files and directories" ]
          ]
      , Right <$> button
          [ className $ maybe cannotCompareClass (const canCompareClass) mTuple
          , case Tuple <$> leftFolder <*> rightFolder of
              Nothing -> disabled true
              Just t -> onClick <#> const t
          ]
          [ text "Check" ]
      ]

compareMenu (Processing { leftFolder, rightFolder, skipHidden }) = do
  formW <|> liftAff do
    Tuple leftTree rightTree <- Tuple
      <$> buildTree skipHidden leftFolder
      <*> buildTree skipHidden rightFolder
    pure $ Finished { leftFolder, rightFolder, leftTree, rightTree, skipHidden }
  where
  formW = div
    [ className "flex flex-col gap-4 justify-center items-center" ]
    [ div [ className "flex gap-2 items-center" ]
        [ input
            [ A.name "skipHidden"
            , _type "checkbox"
            , checked skipHidden
            , disabled true
            , className "h-5 w-5 rounded text-blue-500 focus:ring-blue-400"
            ]
        , label [] [ text "Skip hidden files and directories" ]
        ]
    , button [ className comparingClass, disabled true ] [ text "checking..." ]
    ]

compareMenu (Finished { leftFolder, rightFolder, skipHidden }) =
  compareMenu $
    Start
      { leftFolder: Just leftFolder
      , rightFolder: Just rightFolder
      , skipHidden: skipHidden
      }

folderColClass :: String
folderColClass =
  "flex flex-col rounded border-2 border-stone-800 bg-stone-50 p-4 gap-4"

cannotCompareClass :: String
cannotCompareClass =
  "p-3 py-2 bg-stone-400 text-stone-800 rounded-lg cursor-not-allowed"

canCompareClass :: String
canCompareClass =
  "p-3 py-2 bg-indigo-500 hover:bg-indigo-600 text-white rounded-lg cursor-pointer"

comparingClass :: String
comparingClass = "p-3 py-2 bg-orange-300 text-stone-800 rounded-lg cursor-wait"

renderFolder :: forall a. Maybe FSDirectoryHandle -> Widget HTML a
renderFolder Nothing = h2
  [ className "flex items-center justify-center text-center" ]
  [ text "No directory selected" ]
renderFolder (Just dh) = h2 [ className cn ]
  [ text $ "📂 " <> getDirectoryHandleName dh ]
  where
  cn = "flex items-center justify-center text-xl text-center font-semibold"

renderTree :: Node -> Widget HTML Model
renderTree tree = do
  div [ className "flex flex-col gap-4" ]
    [ renderStats $ treeStats tree
    , div [ className cn ] [ pre [] [ text $ renderTreeString tree ] ]
    ]
  where
  cn = "flex w-full h-full text-xs bg-stone-200 rounded-lg overflow-auto p-1"

renderDiff :: forall a. Node -> Node -> Widget HTML a
renderDiff leftTree rightTree = div [ className "flex flex-col gap-4 w-fit" ]
  [ h2
      [ className "items-center text-xl text-center font-semibold" ]
      [ text $
          "Left and right folders " <> if match then " match" else "don't match"
      ]
  , div [ className $ "rounded " <> color ] [ renderStats diffStats ]
  ]
  where
  match = leftTree == rightTree
  lStats = treeStats leftTree
  rStats = treeStats rightTree
  diffStats =
    { directories: modify negate rStats.directories
    , files: modify negate rStats.files
    , bytes: modify negate rStats.bytes
    } <> lStats
  color = if match then "bg-green-300" else "bg-red-300"

renderStats :: forall a. Stats -> Widget HTML a
renderStats { directories, files, bytes } = div
  [ className "grid grid-cols-2 gap-2 font-mono" ]
  [ p [ className "p-1 text-right" ] [ text $ BI.toString $ unwrap bytes ]
  , p [ className "p-1 text-left" ] [ text " bytes" ]
  , p [ className "p-1 text-right" ] [ text $ BI.toString $ unwrap files ]
  , p [ className "p-1 text-left" ] [ text " files" ]
  , p [ className "p-1 text-right" ] [ text $ BI.toString $ unwrap directories ]
  , p [ className "p-1 text-left" ] [ text " directories" ]
  ]
