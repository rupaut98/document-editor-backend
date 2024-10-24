{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.PostgreSQL.ORM.Model (Model(..), DBKey, DBRef, underscoreModelInfo)

-- Define the User model
data User = User
    { userId       :: !DBKey
    , userUsername :: !Text
    , userPassword :: !Text  -- This will store the hashed password
    } deriving (Show, Generic)

-- Define the Document model
data Document = Document
    { documentId      :: !DBKey
    , documentTitle   :: !Text
    , documentContent :: !Text
    , documentOwnerId :: !(DBRef User) -- Foreign key reference to User
    } deriving (Show, Generic)

-- Associate User with a database table using postgresql-orm
instance Model User where
    modelInfo = underscoreModelInfo "user"

-- Associate Document with a database table using postgresql-orm
instance Model Document where
    modelInfo = underscoreModelInfo "document"

-- JSON instances for User and Document
instance FromJSON User
instance ToJSON User

instance FromJSON Document
instance ToJSON Document
