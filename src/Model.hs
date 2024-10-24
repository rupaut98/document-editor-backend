-- src/Model.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

module Model where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Connection
import Data.Functor.Identity (Identity)
import Beam.Postgres.Aeson (deriveBeamAeson)

-- Define the User table
data UserT f = User
    { userId       :: Columnar f Int
    , userUsername :: Columnar f Text
    , userPassword :: Columnar f Text
    } deriving (Generic, Beamable)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User

-- Derive JSON instances using beam-aeson
deriveBeamAeson defaultOptions ''UserT

-- Define the PrimaryKey for UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = UserId . userId

-- Derive JSON instances for PrimaryKey
deriving instance ToJSON (PrimaryKey UserT Identity)
deriving instance FromJSON (PrimaryKey UserT Identity)

-- Define the Document table
data DocumentT f = Document
    { documentId      :: Columnar f Int
    , documentTitle   :: Columnar f Text
    , documentContent :: Columnar f Text
    , documentOwnerId :: PrimaryKey UserT f  -- Foreign key reference to User
    } deriving (Generic, Beamable)

type Document = DocumentT Identity
deriving instance Show Document
deriving instance Eq Document

-- Derive JSON instances using beam-aeson
deriveBeamAeson defaultOptions ''DocumentT

-- Define the PrimaryKey for DocumentT
instance Table DocumentT where
    data PrimaryKey DocumentT f = DocumentId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = DocumentId . documentId

-- Derive JSON instances for PrimaryKey
deriving instance ToJSON (PrimaryKey DocumentT Identity)
deriving instance FromJSON (PrimaryKey DocumentT Identity)

-- Define the Database
data DocumentDb f = DocumentDb
    { _users     :: f (TableEntity UserT)
    , _documents :: f (TableEntity DocumentT)
    } deriving (Generic, Database be)

-- Beam database settings
documentDb :: DatabaseSettings be DocumentDb
documentDb = defaultDbSettings
