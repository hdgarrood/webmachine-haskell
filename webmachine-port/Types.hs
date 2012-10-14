{-# LANGUAGE OverloadedStrings #-}

module Types ( ServerMonad
             , ToResponse
             , toResponse
             ) where

import Control.Monad.Reader
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import qualified Network.HTTP.Types as H
import qualified Network.Wai as W

import Utility (fromStrict)

-- combines ability to read the request with doing IO actions
type ServerMonad = ReaderT W.Request (ResourceT IO)

class Renderable a where
    render :: a -> BS.ByteString -> ServerMonad T.Text
    render a "application/json" = do
        rendered <- render a "text/plain"
        let start = "{\"message\":\""
            end = "\"}"
        return $ start `T.append` rendered `T.append` end

    render a "application/xml" = do
        rendered <- render a "text/plain"
        let start = "<message>"
            end = "</message>"
        return $ start `T.append` rendered `T.append` end

instance Renderable H.Status where
    render s "text/plain" =
        return $
            (T.pack $ show $ H.statusCode s)
            `T.append` (decodeUtf8 $ H.statusMessage s)
            `T.append` "\n"

class ToResponse a where
    toResponse :: a -> ServerMonad W.Response

instance ToResponse H.Status where
    toResponse s = do
        rendered <- render s "text/plain"
        let responseBody = fromStrict $ encodeUtf8 rendered
        return $ W.responseLBS
            s
            [("Content-Type", "text/plain")]
            responseBody
