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

