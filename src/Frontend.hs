{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import Reflex.Dom
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)

-- Define a simple Document type that mirrors the backend
data Document = Document
  { docId :: Int
  , title :: Text
  , content :: Text
  } deriving (Show, Generic)

instance FromJSON Document
instance ToJSON Document

-- Function to render a document
renderDocument :: MonadWidget t m => Document -> m ()
renderDocument doc = el "div" $ do
  el "h3" $ text (title doc)
  el "p" $ text (content doc)

-- Fetch documents from the backend
getDocuments :: MonadWidget t m => m ()
getDocuments = do
  el "h1" $ text "Documents"
  -- Perform an HTTP GET request
  ev <- performRequestAsync $ const (xhrRequest "GET" "/documents" def) <$> (button "Load Documents")
  -- Decode the response and render it
  let docs = fmap decodeXhrResponse ev :: Event t (Maybe [Document])
  dynText =<< holdDyn "Loading..." (fmap (T.pack . show) docs)

-- Main frontend application
frontend :: MonadWidget t m => m ()
frontend = do
  el "h1" $ text "Document Collaboration"
  getDocuments
