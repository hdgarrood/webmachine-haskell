{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Routing where

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import Data.Text (Text)

import Decision (handle)
import Internal (unServerMonad)
import Resource
import Types

-- maps URI patterns to Resources
makeApp :: (Resource a) => [(BS.ByteString, a)] -> W.Application
makeApp [] req = unServerMonad (toResponse H.notFound404) req
makeApp (r:rs) req =
    if patternMatches resInfo reqInfo
        then handle (snd r) req
        else makeApp rs req
    where reqInfo = W.pathInfo req
          resInfo = H.decodePathSegments $ fst r

patternMatches :: [Text] -> [Text] -> Bool
patternMatches _ _ = True
