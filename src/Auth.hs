-- src/Auth.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Servant
import Servant.Auth.Server
import Data.Text (Text)
import GHC.Generics (Generic)
import Model (User(..))
import Data.Aeson (ToJSON, FromJSON)
import Database.Persist (Key)


-- Define the User type used in JWT
data AuthenticatedUser = AuthenticatedUser
    { authUserId :: Key User
    , authUsername :: Text
    } deriving (Generic, Show)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser

