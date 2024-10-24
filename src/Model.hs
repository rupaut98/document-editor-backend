-- src/Model.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

-- Define the User and Document models
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    username Text
    password Text
    UniqueUsername username
    deriving Show Generic

Document
    title Text
    content Text
    ownerId UserId
    deriving Show Generic
|]

-- JSON instances for Document and User
instance FromJSON Document
instance ToJSON Document

instance FromJSON User
instance ToJSON User
