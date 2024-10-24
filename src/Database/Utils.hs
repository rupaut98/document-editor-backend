-- src/Database/Utils.hs
{-# LANGUAGE FlexibleContexts #-}

module Database.Utils where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Connection (PgConnection)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Data.Pool (Pool, withResource)

runBeamPostgresPool :: Pool Postgres -> Pg a -> IO a
runBeamPostgresPool pool action = withResource pool (`runBeamPostgres` action)
