module Routing where

import Control.Monad.Reader

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H

import Resource
import Types
import Decision (handle)

makeApplication :: (Resource a) => [a] -> W.Application
makeApplication [] req = runReaderT (toResponse H.notFound404) req
makeApplication (r:rs) req
    | r `matches` (W.rawPathInfo req) = runReaderT (handle r) req
    | otherwise                       = makeApplication rs req

makeRoutes :: [(SBS.ByteString, 
