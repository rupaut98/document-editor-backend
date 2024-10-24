-- src/Api/AuthServer.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.AuthServer where

import Servant
import Servant.Auth.Server
import Model
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Auth
import Crypto.BCrypt
import Data.ByteString.Char8 (pack, unpack)

-- Handler for user registration
registerHandler :: MonadIO m => AuthRequest -> ReaderT SqlBackend m NoContent
registerHandler (AuthRequest uname pwd) = do
    hashedPwd <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack $ T.unpack pwd)
    case hashedPwd of
        Nothing -> liftIO $ putStrLn "Password hashing failed."
        Just hpwd -> do
            _ <- insert $ User uname (T.pack $ unpack hpwd)
            liftIO $ putStrLn "User registered successfully."
    return NoContent

-- Handler for user login
loginHandler :: MonadIO m => AuthRequest -> ReaderT SqlBackend m AuthResponse
loginHandler (AuthRequest uname pwd) = do
    mUser <- getBy $ UniqueUsername uname
    case mUser of
        Nothing -> liftIO $ putStrLn "User not found." >> return (AuthResponse "")
        Just (Entity uid user) -> do
            let valid = validatePassword (pack $ T.unpack $ userPassword user) (pack $ T.unpack pwd)
            if valid
                then do
                    let authUser = AuthenticatedUser uid uname
                    jwt <- liftIO $ makeJWT authUser jwtCfg Nothing
                    case jwt of
                        Left err -> liftIO $ putStrLn ("JWT Error: " ++ show err) >> return (AuthResponse "")
                        Right token -> return (AuthResponse $ T.pack $ show token)
                else liftIO $ putStrLn "Invalid password." >> return (AuthResponse "")

-- Server for AuthAPI
authServer :: ConnectionPool -> CookieSettings -> JWTSettings -> Server AuthAPI
authServer pool cookieCfg jwtCfg = hoistServer apiProxy (convertApp pool) (registerHandler :<|> loginHandler)
  where
    apiProxy :: Proxy AuthAPI
    apiProxy = Proxy

    convertApp :: ConnectionPool -> ReaderT SqlBackend Handler a -> Handler a
    convertApp p x = liftIO (runSqlPersistMPool (runReaderT x) p)

-- Update your Model to include UniqueUsername
-- src/Model.hs
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    username Text
    password Text -- This will store the hashed password
    UniqueUsername username
    deriving Show Generic
|]
