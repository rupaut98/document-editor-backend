{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Document
  ( DocumentAPI,       -- Export the API type
    documentAPI,       -- Export the API Proxy
    documentServer     -- Export the server logic
  ) where

import Servant
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Define a Document data type
data Document = Document
    { docId :: Int
    , title :: String
    , content :: String
    } deriving (Eq, Show, Generic)

instance FromJSON Document
instance ToJSON Document

-- Define the API type
type DocumentAPI =
       "documents" :> Get '[JSON] [Document]         -- Get all documents
  :<|> "documents" :> ReqBody '[JSON] Document :> Post '[JSON] Document  -- Create a new document

-- Handlers for the API
documents :: [Document]
documents = [Document 1 "Sample Document" "This is a sample document"]

getDocuments :: Handler [Document]
getDocuments = return documents

createDocument :: Document -> Handler Document
createDocument doc = return doc

-- Combine the handlers into a server
documentServer :: Server DocumentAPI
documentServer = getDocuments :<|> createDocument

-- API Proxy
documentAPI :: Proxy DocumentAPI
documentAPI = Proxy
