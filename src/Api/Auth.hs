{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth where

import Servant
import Servant.Auth.Server
import Model (User)

type AuthAPI = Auth '[JWT] User :> ProtectedAPI

-- Login logic, generating JWT tokens
login :: User -> Handler (Headers '[Header "Set-Cookie" SetCookie] NoContent)
login user = -- implement JWT token generation
