{-# LANGUAGE TemplateHaskell #-}

module Configurator.Const where

import Data.ByteString.Lazy as LBS
import Data.FileEmbed ( embedFile )
import Prelude ()

mainApp :: LBS.ByteString
mainApp = LBS.fromStrict $(embedFile "static/index.html")

mainJS :: LBS.ByteString
mainJS = LBS.fromStrict $(embedFile "static/main.js")

mainCSS :: LBS.ByteString
mainCSS = LBS.fromStrict $(embedFile "static/main.css")
