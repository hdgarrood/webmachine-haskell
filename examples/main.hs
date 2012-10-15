{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

--import qualified Data.ByteString as SBS
--import qualified Data.ByteString.Lazy as BS
--import qualified Data.ByteString.Lazy.Char8 as BS8

import qualified Network.Wai as W
import Network.Wai.Handler.Warp (run)

import Types (ServerMonad)
import Resource (Resource, render)
import Routing (makeApp)

data Posts = Posts

instance Resource Posts where
    render _ "text/plain" = return "PostsCollection: Here are all the posts.\n"

app :: W.Application
app = makeApp [
    ("/posts/:id", Posts)
    ]

main :: IO ()
main = run 3000 app
