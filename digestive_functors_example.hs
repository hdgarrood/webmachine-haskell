{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (isJust)

import Data.Text (Text)
import Text.Blaze ((!))
import qualified Data.Text as T
import qualified Happstack.Server as Happ
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Happstack
import Text.Digestive.Util

import DefaultLayout (defaultLayout)
import qualified Data.Map as M

data GarmentType = GarmentType
    { name :: Text
    } deriving (Show)

garmentTypeForm :: Monad m => Form Text m GarmentType
garmentTypeForm = GarmentType
    <$> "name" .: check "Already taken" checkGarmentType (text Nothing)

checkGarmentType :: Text -> Bool
checkGarmentType name =
    let takenTypes = ["Trousers", "Shirt"]
    in not $ name `elem` takenTypes

garmentTypeView :: View H.Html -> H.Html
garmentTypeView view = do
    label "name" view "Name"
    inputText "name" view

paramShortcut :: H.Html -> M.Map Text H.Html
paramShortcut html = M.fromList [("body", html)]

site :: Happ.ServerPart Happ.Response
site = do
    Happ.decodeBody $ Happ.defaultBodyPolicy "/tmp" 4096 4096 4096
    r <- runForm "test" garmentTypeForm
    case r of
        (view, Nothing) -> do
            let view' = fmap H.toHtml view
                params = paramShortcut $
                    form view' "/" do
                        garmentTypeView view'
                        H.br
                        inputSubmit "Submit")
            Happ.ok $ Happ.toResponse $
                defaultLayout params
        (_, Just garmentType) -> Happ.ok $ Happ.toResponse $ 
            defaultLayout M.fromList [
                ("body", do
                    H.h1 "Garment type received"
                    H.p $ H.toHtml $ show garmentType)
            ]

main :: IO ()
main = Happ.simpleHTTP Happ.nullConf site
