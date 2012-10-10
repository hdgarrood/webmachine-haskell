{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Data.Maybe

import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- shortcut function for the default layout
getParam :: M.Map String H.Html -> String -> H.Html
getParam params key = fromMaybe (H.toHtml ("" :: String)) $ M.lookup key params

-- default layout. Takes a map of HTML snippets and returns HTML
defaultLayout :: M.Map String H.Html -> H.Html
defaultLayout params = 
    H.docTypeHtml $ do
        H.html $ do
            H.head $ do
                H.title "School Shop"
                H.meta ! A.charset "utf-8"
                H.meta !
                    A.httpEquiv "X-UA-Compatible" !
                    A.content "IE=edge,Chrome=1"
                getParam params "stylesheet_link_tag"
                getParam params "javascript_link_tag"
                getParam params "csrf_meta_tags"
            H.body $ do
                H.div ! A.class_ "container" $ do
                    H.header $ do
                        H.h1 "School Shop"
                        getParam params "navigation"
                    H.section ! A.id "main" $ do
                        getParam params "show_flash"
                        getParam params "body"

helloBlaze :: ServerPart Response
helloBlaze =
    ok $ toResponse $ defaultLayout $ M.fromList [
        ("stylesheet_link_tag",
        H.link ! A.rel "stylesheet" !
            A.type_ "text/css" !
            A.href "http://localhost:3000/assets/application.css"),
        ("body", H.p "Hello")
    ]

main :: IO ()
main = simpleHTTP nullConf $ helloBlaze
