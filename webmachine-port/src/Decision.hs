{-# LANGUAGE OverloadedStrings, Rank2Types #-}

module Decision where

import Control.Monad.Reader
import Control.Monad.Writer

import qualified Network.Wai as W
import qualified Network.HTTP.Types as H

import Types
import Resource

data Junction = B13
              | B12
              | B11
              deriving (Show, Eq)

-- for tracing the path made through the HTTP chart
type Trace = WriterT [Junction] ServerMonad

-- Shortcut
type Decision = (Resource a) => a -> Trace W.Response

decide :: Decision
decide = b13

b13 :: Decision
b13 res = do
    tell [B13]
    available <- lift $ serviceAvailable res
    if available
        then b12 res
        else lift $ toResponse H.serviceUnavailable503

b12 :: Decision
b12 res = do
    tell [B12]
    knownMeths <- lift $ knownMethods res
    rqMeth <- lift $ asks W.requestMethod
    if rqMeth `elem` knownMeths
        then b11 res
        else lift $ toResponse H.notImplemented501

-- TODO: list of allowed methods in Accept header
b11 :: Decision
b11 res = do
    tell [B11]
    rqMeth <- lift $ asks W.requestMethod
    allowedMeths <- lift $ allowedMethods res
    if rqMeth `elem` allowedMeths
        then do
            text <- lift $ render res "text/plain"
            return $ W.responseLBS H.ok200 [] text
        else lift $ toResponse H.methodNotAllowed405

-- the main entry point. Takes a resource and returns a response
-- unwraps the Trace as well
handle :: (Resource a) => a -> ServerMonad W.Response
handle res = do
    (resp, js) <- runWriterT (decide res)
    liftIO $ printJuncList js
    return resp

printJuncList :: [Junction] -> IO ()
printJuncList js =
    do putStr "trace: "
       putStr . show . head $ js
       mapM_ putTracePart (tail js)
       putStrLn ""
    where putTracePart junc = putStr (" -> " ++ (show junc))
