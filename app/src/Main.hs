{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import StandOffApp.Bibliography.Widget

import Reflex.Dom


main :: IO ()
main = mainWidget bibInputWizard

