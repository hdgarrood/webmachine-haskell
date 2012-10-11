{-# LANGUAGE OverloadedStrings #-}

module Lib.RequestHelpers
    --(head, get, post, put, delete)
    where

import Data.Maybe
import Data.ByteString.Internal
import Data.Conduit
import Language.Haskell.TH

import Test.HUnit
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as L

baseUrlString :: String
baseUrlString = "http://localhost:3000"

sendRequest req =
    withManager $ \manager -> do
        Response status _ headers body <- http req manager
        return (status, headers, body)


methodWrapper :: Method -> String -> String
                -> m (Status, ResponseHeaders, Body)
methodWrapper meth base url =
    let req = fromJust $ parseUrl (base ++ url)
        req' = req { method = meth
                   , checkStatus = \_ _ -> Nothing
                   }
    in sendRequest req'

--get = methodWrapper "GET" baseUrlString
--post = methodWrapper "POST" baseUrlString
--head = methodWrapper "HEAD" baseUrlString
--put = methodWrapper "PUT" baseUrlString
--delete = methodWrapper "DELETE" baseUrlString
