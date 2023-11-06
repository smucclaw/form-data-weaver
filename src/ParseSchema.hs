{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module ParseSchema where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_, foldMap)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
-- import Data.Text.IO (hPutStrLn)
import Text.Pretty.Simple (pPrint)


import Data.Aeson
import qualified Data.Aeson.Text as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL

import qualified Data.Tree as Tree
import Data.Tree (Tree)

-- import System.Console.Pretty qualified as Pretty

--- protoyping


preuserPath :: FilePath
-- preuserPath = "formConfigs/private/oct19/preUser.json"
preuserPath = "formConfigs/private/preSimplest.json"

getJSON :: IO BL.ByteString
getJSON = BL.readFile preuserPath

decodePreUser ::  IO (Maybe Value)
-- decodePreUser ::  IO (Maybe (Tree Value))
decodePreUser = fmap decode getJSON

printPreUser = decodePreUser >>= pPrint

-- https://abhinavsarkar.net/posts/json-parsing-from-scratch-in-haskell/

-- https://stackoverflow.com/questions/35669080/resolving-references-while-parsing-a-json-document-with-aeson/35679992#35679992
-- https://stackoverflow.com/questions/25789860/aeson-match-binary-tree
-- https://stackoverflow.com/questions/52218662/parse-json-rose-tree-with-haskell-aeson