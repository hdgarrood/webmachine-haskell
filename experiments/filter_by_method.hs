
import Control.Monad (msum)
import Happstack.Server (Method(GET, POST), dir, methodM, nullConf, ok, simpleHTTP)

main = simpleHTTP nullConf $ msum 
       [ do methodM GET
            ok $ "You did a GET request.\n"
       , do methodM POST
            ok $ "You did a POST request.\n"
       , dir "foo" $ do methodM GET
                        ok $ "You did a GET request on /foo\n"
       ]
