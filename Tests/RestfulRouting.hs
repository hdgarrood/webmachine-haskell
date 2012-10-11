module Tests.RestfulRouting where

import Data.Maybe

import Test.HUnit
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as L

url = fromJust $ parseUrl "http://localhost:3000/"

test1 = TestCase (do
            (s,b) <- withManager $ \manager -> do
                Response status _ _ body <- http url manager
                return (status, body)
            assertEqual "status should be 200" status200 s)
