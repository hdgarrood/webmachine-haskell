module Internal where

import Control.Monad.Reader (runReaderT)
import Data.Conduit (ResourceT)

import qualified Network.Wai as W

import Types (ServerMonad)

-- converts a ServerMonad Response into the form that Wai will recognise it
-- in.
unServerMonad :: ServerMonad W.Response -> W.Request -> ResourceT IO W.Response
unServerMonad = runReaderT
