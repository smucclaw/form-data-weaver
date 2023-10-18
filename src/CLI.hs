{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}


module CLI
  (
    main
  ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
-- import Data.Text.IO (hPutStrLn)

import Options.Applicative   qualified as Opt
-- import System.Console.Pretty qualified as Pretty

import Iris qualified
import Paths_form_helper qualified as Autogen
-- https://cabal.readthedocs.io/en/stable/cabal-package.html?highlight=Paths_*#accessing-data-files-from-package-code

data Options = Options
    { fileJsfmsDataSchema :: FilePath
    , fileUIInfoYaml      :: FilePath
    }

optionsP :: Opt.Parser Options
optionsP = do
    fileJsfmsDataSchema <- Opt.strOption $ mconcat
        [ Opt.long "schema"
        , Opt.short 's'
        , Opt.metavar "PATH_SCHEMA"
        , Opt.help "Path to the Json Forms schema (typically named `schema.json` or `preUser.json`)"
        ]

    fileUIInfoYaml <- Opt.strOption $ mconcat
        [ Opt.long "uiinfo"
        , Opt.short 'u'
        , Opt.metavar "PATH_UI_INFO_YAML"
        , Opt.help "Path to the UI Info .yaml"
        ]

    pure Options{..}


-- placeholder for now; this will change soon
type EnvType = ()
newtype App a = App { unApp :: Iris.CliApp Options EnvType a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv Options EnvType)
        )

appSettings :: Iris.CliEnvSettings Options EnvType
appSettings = Iris.defaultCliEnvSettings
    { -- CLI parser for Options
      Iris.cliEnvSettingsCmdParser = optionsP
      
      -- short description
    , Iris.cliEnvSettingsHeaderDesc = "Helper tool for managing and orchestrating web form ui text"

      -- Long app description to appear in --help
    , Iris.cliEnvSettingsProgDesc = "Helper tool for managing and orchestrating web form ui text (and potentially more)"

      -- a function to display the tool version
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = ("Form UI Text Helper" <>)
            }
    }

app :: App ()
app = do
    --  Get parsed 'Options' from the environment
    Options{..} <- Iris.asksCliEnv Iris.cliEnvCmd

    liftIO $ putStrLn "placeholder TODO"

main :: IO ()
main = Iris.runCliApp appSettings $ unApp app