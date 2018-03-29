{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Xhr
  where

import Reflex.Dom
import Data.Text
import Data.Monoid ((<>))


-- * Parsing the response

-- | Show the whole response body.
showRsp :: Either XhrException XhrResponse -> Maybe Text
showRsp rsp =
  either
  (Just . pack . show)
  (Just . ("Response: " <>) . pack . show . _xhrResponse_responseText)
  rsp
