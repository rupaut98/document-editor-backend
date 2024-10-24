-- src/Model.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Database.Beam
import Database.Beam.Postgres
import Data.Functor.Identity (Identity)

-- Define the User table
data UserT f = User
    { userId       :: Columnar f Int
    , userUsername :: Columnar f Text
    , userPassword :: Columnar f Text
    } deriving stock (Generic)
      deriving anyclass (Beamable)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User
instance FromJSON User
instance ToJSON User

-- Define the PrimaryKey for UserT
instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving stock (Generic)
                                   deriving anyclass (Beamable)
    primaryKey = UserId . userId

-- Manually derive ToJSON and FromJSON for PrimaryKey UserT Identity
deriving anyclass instance ToJSON (PrimaryKey UserT Identity)
deriving anyclass instance FromJSON (PrimaryKey UserT Identity)

-- Define the Document table
data DocumentT f = Document
    { documentId      :: Columnar f Int
    , documentTitle   :: Columnar f Text
    , documentContent :: Columnar f Text
    , documentOwnerId :: PrimaryKey UserT f  -- Foreign key reference to User
    } deriving stock (Generic)
      deriving anyclass (Beamable)

type Document = DocumentT Identity
deriving instance Show Document
deriving instance Eq Document
instance FromJSON Document
instance ToJSON Document

-- Define the PrimaryKey for DocumentT
instance Table DocumentT where
    data PrimaryKey DocumentT f = DocumentId (Columnar f Int) deriving stock (Generic)
                                       deriving anyclass (Beamable)
    primaryKey = DocumentId . documentId

-- Manually derive ToJSON and FromJSON for PrimaryKey DocumentT Identity
deriving anyclass instance ToJSON (PrimaryKey DocumentT Identity)
deriving anyclass instance FromJSON (PrimaryKey DocumentT Identity)

-- Define the Database
data DocumentDb f = DocumentDb
    { _users     :: f (TableEntity UserT)
    , _documents :: f (TableEntity DocumentT)
    } deriving (Generic, Database be)

-- Beam database settings
documentDb :: DatabaseSettings be DocumentDb
documentDb = defaultDbSettings
