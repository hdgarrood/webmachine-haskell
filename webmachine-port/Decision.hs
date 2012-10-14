{-# LANGUAGE OverloadedStrings #-}

module Decision (handle) where

import Control.Monad.Reader
import qualified Network.Wai as W
import qualified Network.HTTP.Types as H

import Types
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
                        else toResponse H.methodNotAllowed405
                else toResponse H.notImplemented501
        else toResponse H.serviceUnavailable503

