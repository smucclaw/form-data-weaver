{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module CLI ( main ) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_, foldMap)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
-- import Data.Text.IO (hPutStrLn)

import Options.Applicative qualified as Opt
-- import System.Console.Pretty qualified as Pretty

import Iris qualified
import Paths_form_weaver qualified as Autogen
-- https://cabal.readthedocs.io/en/stable/cabal-package.html?highlight=Paths_*#accessing-data-files-from-package-code


{- Optsparse
This application is simple enough that `optparse-generic` can probably do the job
But doing it more manually makes for good practice
-}

-- data Options = Options
--     { optCommand :: !Cmd
--     , globalOpt :: ...
--     }

data Cmd
  = CheckUIYaml CheckUIYamlOptions
  | CheckWithConfig FilePath
  | GetFormFieldsFromLE FilePath
    deriving stock (Show)

data CheckUIYamlOptions = 
  CheckUIYamlOptions { fileJsfmsDataSchema :: FilePath
                     , fileUIInfoYaml      :: FilePath }
    deriving stock (Show)

cmdP :: Opt.Parser Cmd
cmdP =  
  Opt.subparser . foldMap command' $  
    [ ("get-form-fields", "(**Not Yet Implemented**) Given an LE encoding, extracts the names of the json form fields that the LE encoding uses", 
        fieldsFromLeP)
    -- , ("check-ui-info", "(**Not Yet Implemented**) Check if UI Info yaml is consistent with json schema", 
    --     chkUIinfoP)
    -- , ("check-with-config", "(**Not Yet Implemented**) Use supplied config to check (i) if files conform to hash and (ii) if UI Info yaml is consistent with json schema", 
    --     chkWithCfgP) 
    ]
  where
    fieldsFromLeP = GetFormFieldsFromLE <$> lengArg

    chkUIinfoP = CheckUIYaml <$> (CheckUIYamlOptions <$> 
                    jsonSchemaArg <*> uiYamlArg)
    
    chkWithCfgP = CheckWithConfig <$> cfgArg

    lengArg = Opt.strArgument $ mconcat
            [ Opt.help "The LE file (typically named `program.le`)"
            , Opt.metavar "PATH_LE_ARG" ]

    jsonSchemaArg = Opt.strArgument $ mconcat
            [ Opt.help "Path to the Json Forms schema (typically named `schema.json` or `preUser.json`)"
            , Opt.metavar "PATH_SCHEMA" ]

    uiYamlArg = Opt.strArgument $ mconcat
            [ Opt.help "Path to the UI Info .yaml"
            , Opt.metavar "PATH_UI_INFO_YAML" ]

    cfgArg = Opt.strArgument $ mconcat
            [ Opt.help "Use supplied config to check (i) if files conform to hash and (ii) the .yaml"
            , Opt.metavar "PATH_CFG_ARG" ]



command' :: (String, String, Opt.Parser a) -> Opt.Mod Opt.CommandFields a
command' (cmdName, desc, parser) = Opt.command cmdName (info' parser desc)

info' :: Opt.Parser a -> String -> Opt.ParserInfo a
info' p desc = Opt.info 
    (Opt.helper <*> p) 
    (Opt.fullDesc <> Opt.progDesc desc)


------------------ Iris app and settings -------------------------------

-- placeholder for now; this will change soon
type EnvType = ()
newtype App a = App { unApp :: Iris.CliApp Cmd EnvType a }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader (Iris.CliEnv Cmd EnvType)
        )

appSettings :: Iris.CliEnvSettings Cmd EnvType
appSettings = Iris.defaultCliEnvSettings
    { -- CLI parser for Options
      Iris.cliEnvSettingsCmdParser = cmdP
      
      -- short description
    , Iris.cliEnvSettingsHeaderDesc = "Helper tool for web form usecase"

      -- Long app description to appear in --help
    , Iris.cliEnvSettingsProgDesc = "Helper tool for web form usecase"

      -- a function to display the tool version
    , Iris.cliEnvSettingsVersionSettings =
        Just (Iris.defaultVersionSettings Autogen.version)
            { Iris.versionSettingsMkDesc = ("Form UI Text Helper" <>)
            }
    }

-- handleCmd :: Cmd -> 
handleCmd :: Cmd -> FilePath
handleCmd = \case
  GetFormFieldsFromLE pathLE -> pathLE
  _ -> error "Not yet implemented"


app :: App ()
app = do
    --  Get parsed 'Cmd' from the environment
    parsedCmd <- Iris.asksCliEnv Iris.cliEnvCmd

    liftIO . putStrLn $ "[TEMP PLACEHOLDER] cmd is:\n"<> show parsedCmd




main :: IO ()
main = Iris.runCliApp appSettings $ unApp app


{-
Useful resources
----------------

On opts parse:
* https://www.fpcomplete.com/haskell/library/optparse-applicative/
* https://github.com/danidiaz/vdpq/blob/master/executable/Main.hs
* https://danidiaz.medium.com/subcommands-with-optparse-applicative-1234549b21c6

On configuration:
* https://cs-syd.eu/posts/2016-06-26-a-configuration-loading-scheme-for-tools-in-haskell.html

-}