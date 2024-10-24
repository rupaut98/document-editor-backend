-- src/Model.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Persist.TH
import GHC.Generics (Generic)

-- Define the User model with a unique constraint on username
share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    username Text
    password Text -- This will store the hashed password
    UniqueUsername username
    deriving Show Generic

Document
    title Text
    content Text
    ownerId UserId
    deriving Show Generic
|]

instance FromJSON Document

instance ToJSON Document

instance FromJSON User

instance ToJSON User
