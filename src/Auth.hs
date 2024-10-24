-- src/Auth.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Servant
import Servant.Auth.Server (Auth, JWT, AuthResult(Authenticated))
import Data.Text (Text)
import GHC.Generics (Generic)
import Model (User(..))
import Data.Aeson (ToJSON, FromJSON)

-- Define the User type used in JWT
data AuthenticatedUser = AuthenticatedUser
    { authUserId   :: Int
    , authUsername :: Text
    } deriving (Generic, Show)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
