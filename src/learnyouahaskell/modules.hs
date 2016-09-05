-- Import only the nub- and sort-functions:
-- import Data.List (nub, sort)
-- Import all but the nub- and sort-functions:
-- import Data.List hiding (nub, sort)

-- import qualified Data.Map as M
-- -> M.filter ...


import Control.Monad   
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main