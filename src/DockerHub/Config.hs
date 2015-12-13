{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module DockerHub.Config
       ( load
       , Config(..)
       ) where

{-
  @Issue(
    "Check if we could limit what is imported from Text.Internal, Yaml and Generics"
    type="improvement"
    priority="low"
  )
-}
import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Maybe (fromJust)
import Data.Text.Internal
import Data.Yaml
import DockerHub.Data (Repository)
import GHC.Generics

-- Public API/types.

-- Data type that holds information about a DockerHub account.
data Config = Config { repositories :: [Repository] } deriving (Show, Generic)

-- Load a configuration file and convert it to a Config object.
{-
  @Issue(
    "Add support for other file types too"
    type="improvement"
    priority="low"
  )
-}
load :: FilePath -> IO (Config)
load filepath = do
    ymlData <- BS.readFile filepath
    let config = Data.Yaml.decode ymlData :: Maybe Config
    return $ fromJust config

-- Functions/types for internal use.

-- Conversion from and to JSON.
instance FromJSON Config
instance ToJSON Config
