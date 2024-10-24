{-# LANGUAGE DeriveGeneric #-}
module Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Database.Persist.TH
import Data.Text (Text)

-- Document data type
data Document = Document
    { docId :: Int
    , docTitle :: String
    , docContent :: String
    } deriving (Show, Generic)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    password Text -- This will store the hashed password
    deriving Show Generic
|]

instance FromJSON Document
instance ToJSON Document
