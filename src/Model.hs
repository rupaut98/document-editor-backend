-- src/Model.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Database.Persist.TH
import Data.Text (Text)

-- Define the User model with a unique constraint on username
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

-- Additional instances for User if needed
instance FromJSON User
instance ToJSON User
