{-# LANGUAGE OverloadedStrings #-}

module App.Routes where

import Data.Text
import Control.Monad
import Happstack.Server

routes :: ServerPart Response
routes = ok $ toResponse ("hello world" :: Text)

--routes = do
--    methodM GET $ ok $ toResponse ("hello world!\n" :: String)

--routes = msum $
--    [ do methodM GET
--        ok $ toResponse $ ("You did a GET request.\n" :: Text)
--    , do methodM POST
--        ok "You did a POST request.\n"
--    , dir "foo" do
--        ok "You did a request on /foo.\n"
--    ]
