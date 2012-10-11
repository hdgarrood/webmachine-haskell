{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Lib.RequestHelpers
    --(head, get, post, put, delete)
    where

import Data.Maybe
import Data.ByteString
import Data.Conduit
import Language.Haskell.TH

import qualified Control.Monad.IO.Class as M
import qualified Control.Monad.Trans.Control as M

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


methodWrapper :: (M.MonadIO m,
                M.MonadBaseControl IO m,
                MonadUnsafeIO m,
                MonadThrow m) =>
                Method -> String -> String
                -> m (Status,
                        ResponseHeaders, 
                        ResumableSource (ResourceT m) ByteString)
methodWrapper meth base url =
    let req = fromJust $ parseUrl (base ++ url)
        req' = req { method = meth
                   , checkStatus = \_ _ -> Nothing
                   }
    in sendRequest req'

--get = methodWrapper methodGet baseUrlString
--post = methodWrapper methodPost baseUrlString
--head = methodWrapper methodHead baseUrlString
--put = methodWrapper methodPut baseUrlString
--delete = methodWrapper methodDelete baseUrlString
