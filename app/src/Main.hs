{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom
import Reflex

import StandOffApp.App
import StandOffApp.Config


config :: AppConfig
config = defaultConfig & cfg_baseUri .~ "http://127.0.0.1:3000"

main :: IO ()
main = mainWidget (appWidget config appView appEventsHook)
