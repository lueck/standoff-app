{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.Xhr
  where

import Reflex.Dom
import Data.Text
import Data.Monoid ((<>))


-- * Request configuration

-- | make a request passing the first argument and dropping a second.
requestCfgIgn1 :: XhrRequestConfig () -- ^ the request with credentials
                 -> a -- ^ a value that is ignored
                 -> XhrRequestConfig ()
requestCfgIgn1 rq _ = rq


-- * Parsing the response

-- | Show the whole response body.
showRsp :: Either XhrException XhrResponse -> Maybe Text
showRsp rsp =
  either
  (Just . pack . show)
  (Just . ("Response: " <>) . pack . show . _xhrResponse_responseText)
  rsp
