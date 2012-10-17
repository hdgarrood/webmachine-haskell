{-# LANGUAGE OverloadedStrings #-}

module Resource ( Resource
                , serviceAvailable
                , knownMethods
                , allowedMethods
                , render
                ) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import qualified Network.HTTP.Types as H

import Types ( ServerMonad
             , ToResponse
             , toResponse
             )

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
        , "PATCH"
        ]

    allowedMethods :: a -> ServerMonad [H.Method]
    allowedMethods = const $ return
        [ "GET"
        , "HEAD"
        ]

    render :: a -> BS.ByteString -> ServerMonad BS.ByteString
    render _ _ = return ""
