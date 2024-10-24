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
import Data.Functor.Identity (Identity)

-- Define the User table
data UserT f = User
    { userId       :: Columnar f Int
    , userUsername :: Columnar f Text
    , userPassword :: Columnar f Text
    } deriving (Generic, Beamable)

type User = UserT Identity
deriving instance Show User
deriving instance Eq User
instance FromJSON User
instance ToJSON User

-- Define the PrimaryKey for UserT
deriving instance Show (PrimaryKey UserT Identity)
deriving instance Eq (PrimaryKey UserT Identity)

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = UserId . userId

-- Define the Document table
data DocumentT f = Document
    { documentId      :: Columnar f Int
    , documentTitle   :: Columnar f Text
    , documentContent :: Columnar f Text
    , documentOwnerId :: PrimaryKey UserT f -- Foreign key reference to User
    } deriving (Generic, Beamable)

type Document = DocumentT Identity
deriving instance Show Document
deriving instance Eq Document
instance FromJSON Document
instance ToJSON Document

-- Define the PrimaryKey for DocumentT
deriving instance Show (PrimaryKey DocumentT Identity)
deriving instance Eq (PrimaryKey DocumentT Identity)

instance Table DocumentT where
    data PrimaryKey DocumentT f = DocumentId (Columnar f Int) deriving (Generic, Beamable)
    primaryKey = DocumentId . documentId

-- Define the Database
data DocumentDb f = DocumentDb
    { _users     :: f (TableEntity UserT)
    , _documents :: f (TableEntity DocumentT)
    } deriving (Generic, Database be)

-- Beam database settings
documentDb :: DatabaseSettings be DocumentDb
documentDb = defaultDbSettings
