module Monads (ServerMonad) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource

import qualified Network.Wai as W

-- combines ability to read the request with doing IO actions
type ServerMonad = ReaderT W.Request (ResourceT IO)
