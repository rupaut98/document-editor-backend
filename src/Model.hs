{-# LANGUAGE DeriveGeneric #-}
module Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Document data type
data Document = Document
    { docId :: Int
    , docTitle :: String
    , docContent :: String
    } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document
