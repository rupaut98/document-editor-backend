-- src/Api/AuthAPI.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.AuthAPI where

import Servant
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Model (User(..))

-- Request body for registration and login
data AuthRequest = AuthRequest
    { username :: Text
    , password :: Text
    } deriving (Generic, Show)

instance ToJSON AuthRequest
instance FromJSON AuthRequest

-- Response after successful login
data AuthResponse = AuthResponse
    { token :: Text
    } deriving (Generic, Show)

instance ToJSON AuthResponse
instance FromJSON AuthResponse

-- Define the Auth API
type AuthAPI = "register" :> ReqBody '[JSON] AuthRequest :> PostNoContent '[JSON] NoContent
        :<|> "login" :> ReqBody '[JSON] AuthRequest :> Post '[JSON] AuthResponse
