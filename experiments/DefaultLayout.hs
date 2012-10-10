{-# LANGUAGE OverloadedStrings #-}

module DefaultLayout (defaultLayout) where

import qualified Data.Map as M
import Data.Maybe
import Data.Text

import Happstack.Server
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- shortcut function for the default layout
getParam :: M.Map Text H.Html -> Text -> H.Html
getParam params key = fromMaybe (H.toHtml ("" :: Text)) $ M.lookup key params

-- default layout. Takes a map of HTML snippets and returns HTML
defaultLayout :: M.Map Text H.Html -> H.Html
defaultLayout params =
    let param = getParam params
    in H.docTypeHtml $ do
        H.html $ do
            H.head $ do
                H.title "School Shop"
                H.meta ! A.charset "utf-8"
                H.meta !
                    A.httpEquiv "X-UA-Compatible" !
                    A.content "IE=edge,Chrome=1"
                param "stylesheet_link"
                param "javascript_link"
                param "csrf_meta"
            H.body $ do
                H.div ! A.class_ "container" $ do
                    H.header $ do
                        H.h1 "School Shop"
                        param "navigation"
                    H.section ! A.id "main" $ do
                        param "show_flash"
                        param "body"

helloBlaze :: ServerPart Response
helloBlaze =
    ok $ toResponse $ defaultLayout $ M.fromList [
        ("stylesheet_link",
        H.link ! A.rel "stylesheet" !
            A.type_ "text/css" !
            A.href "http://localhost:3000/assets/application.css"),
        ("body", H.p "Hello")
    ]

--main :: IO ()
--main = simpleHTTP nullConf $ helloBlaze
