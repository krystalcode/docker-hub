{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module DockerHub.Data
       ( Repository(..)
       ) where

{-
  @Issue(
    "Check if we could limit what is imported from Aeson and Generics"
    type="improvement"
    priority="low"
  )
-}
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import GHC.Generics

-- Public API/types.

-- Data type that holds information about a DockerHub repository.
data Repository = Repository { name :: String
                             , triggerToken :: String } deriving (Show, Generic)

-- Functions/types for internal use.

-- Conversion from and to JSON.
instance FromJSON Repository where
    parseJSON (Object v) = Repository <$>
                           v .: "name" <*>
                           v .: "trigger_token"
    parseJSON invalid    = typeMismatch "Repository" invalid

instance ToJSON Repository
