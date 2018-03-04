{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Main where

import Reflex.Dom
import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid((<>))
import           Data.Maybe (fromJust)
import Control.Monad

import GHCJS.DOM.Types (JSM)

import StandOffApp.Bibliography

main :: JSM ()
main = mainWidget bibInputWizard

