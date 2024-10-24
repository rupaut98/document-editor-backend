-- src/Auth.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Auth where

import Servant
import Servant.Auth.Server (Auth, JWT, AuthResult(Authenticated), ToJWT, FromJWT)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Functor.Identity (Identity)      -- Import Identity
import Model (PrimaryKey, UserT)             -- Import PrimaryKey and UserT separately
import Data.Aeson (ToJSON, FromJSON)

-- Define the User type used in JWT
data AuthenticatedUser = AuthenticatedUser
    { authUserId    :: PrimaryKey UserT Identity
    , authUsername  :: Text
    } deriving (Generic, Show, ToJSON, FromJSON, ToJWT, FromJWT)
