{-# LANGUAGE OverloadedStrings #-}

module Decision (handle) where

import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H

import Monads
import Resource

-- the main entry point. Takes a resource and returns a response
handle :: (Resource a) => a -> ServerMonad W.Response
handle res = do
    available <- serviceAvailable res
    if available
        then do
            knownMeths <- knownMethods res
            rqMeth <- asks W.requestMethod
            if rqMeth `elem` knownMeths
                then do
                    allowedMeths <- allowedMethods res
                    if rqMeth `elem` allowedMeths
                        then do
                            text <- render res "text/plain"
                            return $ W.responseLBS H.ok200 [] text
                        -- TODO: list of allowed methods in Accept header
                        else methodNotAllowed
                else notImplemented
        else serviceUnavailable

methodNotAllowed :: ServerMonad W.Response
methodNotAllowed = return $ W.responseLBS H.status405 [] ""

notImplemented :: ServerMonad W.Response
notImplemented = return $ W.responseLBS H.status501 [] ""

serviceUnavailable :: ServerMonad W.Response
serviceUnavailable = return $ W.responseLBS H.status503 [] ""

