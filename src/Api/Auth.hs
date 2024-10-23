{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth where

import Servant
import Servant.Auth.Server
import Data.Text (Text)

type ProtectedAPI = Auth '[JWT] User :> DocumentAPI

data User = User { userId :: Int, userName :: Text }
  deriving (Eq, Show)

instance FromJWT User
instance ToJWT User

-- Simulate user login (you can later integrate with real login)
login :: Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
login = return NoContent  -- Simplified for now
