-- src/Api/AuthAPI.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Api.AuthAPI where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Request type for registration and login
data AuthRequest = AuthRequest
    { uname :: Text
    , pwd   :: Text
    } deriving stock (Generic)
      deriving anyclass (FromJSON, ToJSON, Show, Eq)

-- | Response type for authentication (e.g., JWT token)
data AuthResponse = AuthResponse
    { token :: Text
    } deriving stock (Generic)
      deriving anyclass (FromJSON, ToJSON, Show, Eq)

-- | Define the Auth API with two endpoints: register and login
type AuthAPI =
         "register" :> ReqBody '[JSON] AuthRequest :> Verb 'POST 204 '[JSON] NoContent
    :<|> "login"    :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResponse
