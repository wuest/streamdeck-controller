{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Data.Sqlite ( connect, run, initialize )
    where

import Prelude

import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Reader ( ReaderT )
import Control.Monad.Reader       ( asks )
import Control.Monad.Logger       ( runStdoutLoggingT )
import Database.Persist.Sqlite    ( SqlBackend, ConnectionPool
                                  , createSqlitePool, runSqlPool
                                  , runMigration )

import Data.Model

import qualified Opts as O

newtype DBPool = DBPool { getPool :: ConnectionPool }
type DBPoolM = ReaderT DBPool IO

type Query = ReaderT SqlBackend IO ()

connect :: IO DBPool
connect = do
    let n = "streamdeck.db"
    pool <- runStdoutLoggingT $ createSqlitePool n 10
    return $ DBPool pool

run :: Query -> DBPoolM ()
run query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

initialize :: DBPoolM ()
initialize = run $ runMigration migrateAll
