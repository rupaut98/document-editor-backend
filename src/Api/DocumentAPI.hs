-- src/Api/DocumentAPI.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.DocumentAPI where

import Servant
import Servant.Auth.Server (Auth, JWT)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Model (Document(..))
import Auth (AuthenticatedUser)

-- Define the Document type for API responses
data DocumentResponse = DocumentResponse
    { docId    :: Int
    , title    :: Text
    , content  :: Text
    } deriving (Generic, Show)

instance ToJSON DocumentResponse
instance FromJSON DocumentResponse

-- Define request bodies
data DocumentCreate = DocumentCreate
    { createTitle   :: Text
    , createContent :: Text
    } deriving (Generic, Show)

instance ToJSON DocumentCreate
instance FromJSON DocumentCreate

data DocumentUpdate = DocumentUpdate
    { updateTitle   :: Maybe Text
    , updateContent :: Maybe Text
    } deriving (Generic, Show)

instance ToJSON DocumentUpdate
instance FromJSON DocumentUpdate

-- Define the API endpoints for documents
type DocumentAPI =
         "documents" :> ReqBody '[JSON] DocumentCreate :> Auth '[JWT] AuthenticatedUser :> Post '[JSON] DocumentResponse
    :<|> "documents" :> Get '[JSON] [DocumentResponse]
    :<|> "documents" :> Capture "docId" Int :> ReqBody '[JSON] DocumentUpdate :> Auth '[JWT] AuthenticatedUser :> Put '[JSON] NoContent
    :<|> "documents" :> Capture "docId" Int :> Auth '[JWT] AuthenticatedUser :> DeleteNoContent '[JSON] NoContent
