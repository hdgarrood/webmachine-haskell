{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp (run)

import Types (ServerMonad)
import Resource
import Decision (handle)

data PostsCollection = PostsCollection

newtype PostId = PostId Int deriving (Show, Read, Eq, Ord)
data Post = Post PostId

instance Resource PostsCollection where
    render _ "text/plain" = return "PostsCollection: Here are all the posts.\n"

instance Resource Post where
    render (Post postId) "text/plain" = return $
        "This is the page for post "
        `BS.append` (BS8.pack $ show $ postId)
        `BS.append` ".\n"

app :: W.Application
app req = case W.rawPathInfo req of
              "/posts"    -> runReaderT (handle PostsCollection) req
              "/posts/1"  -> runReaderT (handle $ Post $ PostId 1) req
              _           -> return $ W.responseLBS H.status404 [] ""

main :: IO ()
main = run 3000 app
