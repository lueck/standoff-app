{-# LANGUAGE OverloadedStrings #-}
module StandOffApp.PostgRest
  where

import Reflex.Dom
import Data.Text
import Data.Monoid ((<>))
import qualified Data.Map as Map
import qualified Data.Aeson as J
import Control.Lens
import Data.Aeson.Lens
import Control.Monad


-- | Parse error message returned by PostgREST.
parseError :: Either XhrException XhrResponse -> Maybe Text
parseError rsp =
  either
  (Just . pack . show) -- when the request fails
  (\r -> fmap ((<> status r) -- add status information
               . pack . show . (^._String)) $ -- convert J.Value String to Text
    join $  -- Just Nothing -> Nothing etc.
    fmap
    (Map.lookup "message") -- get the error message from postgrest
    -- parse the response to a map
    ((decodeXhrResponse r) :: Maybe (Map.Map String J.Value)))
  rsp
  where
    status r =
      " ("
      <> (pack $ show $ _xhrResponse_status r)
      <> " "
      <> (_xhrResponse_statusText r)
      <> ")"
