module App.OtherRoutes where

import Control.Monad (msum)
import Happstack.Server (Method(GET, POST), dir, methodM, nullConf, ok, simpleHTTP)

routes = msum 
       [ do methodM GET
            ok $ "You did a GET request.\n"
       , do methodM POST
            ok $ "You did a POST request.\n"
       , dir "foo" $ do methodM GET
                        ok $ "You did a GET request on /foo\n"
       ]

