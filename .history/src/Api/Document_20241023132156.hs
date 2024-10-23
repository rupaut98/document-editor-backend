{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Document where

import Servant
import Model (Document)
import Database.DocumentDB (getDocuments, createDocument)

type DocumentAPI = 
       "documents" :> Get '[JSON] [Document]  -- Get all documents
  :<|> "documents" :> ReqBody '[JSON] Document :> Post '[JSON] Document  -- Create new document

documentAPI :: Proxy DocumentAPI
documentAPI = Proxy

-- Handlers for the API
documentServer :: Server DocumentAPI
documentServer = getDocuments :<|> createDocument
