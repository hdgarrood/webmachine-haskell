{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

import Prelude hiding (id, (.))
import Control.Category (Category(id, (.)))
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8

import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp (run)
import Safe (readMay)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes.Boomerang (Router, (<>), (</>), int, parse1)
import Text.Boomerang.HStack ((:-))

import Types (ServerMonad)
import Resource (Resource, render)
import qualified Decision as D

data PostsCollection = PostsCollection
data Post = Post Int

instance Resource PostsCollection where
    render _ "text/plain" = return "PostsCollection: Here are all the posts.\n"

instance Resource Post where
    render (Post x) "text/plain" = return $
        "This is the page for post "
        `BS.append` (BS8.pack $ show $ x)
        `BS.append` ".\n"

data Sitemap = RPostsCollection
             | RPost Int
             deriving (Show, Eq)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap (Sitemap :- Sitemap)
sitemap = "posts" . (rRPostsCollection <> rRPost </> int)

handle :: Sitemap -> ServerMonad W.Response
handle r = case r of
               RPostsCollection -> D.handle PostsCollection
               RPost x          -> D.handle (Post x)

--main :: IO ()
--main = run 3000 app
