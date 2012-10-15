{-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}
 
module Main where

import Prelude              hiding (id, (.))
import Control.Category     (Category(id, (.)))
import Control.Monad.Trans  (MonadIO(liftIO))
import Text.Boomerang.TH    (derivePrinterParsers)
import Text.Boomerang.HStack ((:-))
import Web.Routes           (Site(..), RouteT(..), decodePathInfo, encodePathInfo, runSite, showURL)
import Web.Routes.Boomerang (Router, (<>), (</>), int, parse1, boomerangSiteRouteT, anyText)
import Data.Text.Internal

-- | the routes
data Sitemap
    = Home
    | UserOverview
    | UserDetail Int
    deriving (Eq, Show)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router Sitemap (Sitemap :- Sitemap)
sitemap =
    (  rHome
    <> "users" . users
    )
  where
      users  =  rUserOverview
             <> rUserDetail </> int

handle :: Sitemap -> RouteT Sitemap IO ()
handle url =
     case url of
          _ -> do liftIO $ print url
               s <- showURL url
               liftIO $ putStrLn s
