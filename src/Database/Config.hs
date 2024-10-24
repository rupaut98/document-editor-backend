-- src/Database/Config.hs
{-# LANGUAGE OverloadedStrings #-}

module Database.Config where

import Database.Beam.Postgres
import Database.Beam.Postgres.Connection

data PostgresConfig = PostgresConfig
    { pgConnStr :: PostgresConnectionString
    }

