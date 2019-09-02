{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.Model where

import Prelude

import Data.Text                   ( Text )
import GHC.Generics                ( Generic )
import Data.Aeson                  ( FromJSON, ToJSON )
import Data.ByteString             ( ByteString )

import Database.Persist.TH

data ActionSpec = Hotkey Text
                | OpenURL Text
                | RunApp Text
  deriving (Show, Read, Eq)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Streamdeck
    serial   Text
    nickname Text
    enabled  Bool
    deriving Show Generic

Button
    streamdeck Streamdeck
    xPos       Int
    yPos       Int
    image      Text
    actionType Int
    actionText Text
    deriving Show Generic
|]

instance FromJSON Streamdeck
instance ToJSON Streamdeck

instance FromJSON Button
instance ToJSON Button
