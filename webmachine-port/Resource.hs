{-# LANGUAGE OverloadedStrings #-}

module Resource ( Resource
                , serviceAvailable
                , knownMethods
                , allowedMethods
                , toHtml
                , toText
                ) where

import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Types as H
import Monads (ServerMonad)

class Resource a where
    serviceAvailable :: a -> ServerMonad Bool
    serviceAvailable = const $ return True

    knownMethods :: a -> ServerMonad [H.Method]
    knownMethods = const $ return
        [ "GET"
        , "HEAD"
        , "POST"
        , "PUT"
        , "DELETE"
        , "OPTIONS"
        , "TRACE"
        ]

    allowedMethods :: a -> ServerMonad [H.Method]
    allowedMethods = knownMethods

    toHtml :: a -> ServerMonad BS.ByteString
    toText :: a -> ServerMonad BS.ByteString
