module Data.CSV.Table.Utils where

import            Text.CSV
import            Data.CSV.Table.Types
import            Control.Monad
import            Data.List  (elemIndex)
import qualified  Data.Map.Strict as M
import            Control.Applicative ((<$>))

-------------------------------------------------------------------
-- | Map a function over all rows of a table
-------------------------------------------------------------------

mapRows   :: ([(Field, Field)] -> a) -> Table -> [a]
mapRows f = map f . rowCols   

rowCols   :: Table -> [[(Field, Field)]]
rowCols t = [zip cs r | r <- rs]
  where
    cs    = getCols t
    rs    = getRows t

-------------------------------------------------------------------
-- | Sort table by a particular column
-------------------------------------------------------------------

sortBy :: Col -> Table -> Table
sortBy = undefined

-- TODO

average   :: Col -> Table -> Int
average   = undefined

-- addCol    :: Table -> Formula -> Table
-- addCol    = undefined


