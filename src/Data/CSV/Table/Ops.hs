
module Data.CSV.Table.Ops (

  -- * Core Operators
    join
  , index
  , joinBy
  , indexBy
  , sortBy
  , project
  , project1
  , moveColL

  ) where

import           Text.CSV
import           System.FilePath
import           Control.Applicative ((<$>))
import           Data.Maybe
import           Data.List (sort, elemIndex)
import qualified Data.Map.Strict as M
import           Data.CSV.Table.Types

-------------------------------------------------------------------
-- | Swap two columns of a table
-------------------------------------------------------------------
moveColL      :: Table -> Col -> Table
moveColL t@(T n cs b) c 
  | i == 0    = t
  | otherwise = T n (pluck i cs) [R $ pluck i r | R r <- b]
  where
     i        = columnIndex t c

pluck n xs = go [] n xs
  where 
    go ls 0 (r:rs) = r : (ls +++ rs)
    go ls n (r:rs) = go (r:ls) (n-1) rs

[]     +++ ys = ys
(x:xs) +++ ys = xs +++ (x:ys)

-------------------------------------------------------------------
-- | Join two tables by first column (which should be unique) 
-------------------------------------------------------------------

join       :: Table -> Table -> Table
join t1 t2 = T n' cs b' 
  where
    m1     = index t1
    m2     = index t2 
    m'     = M.intersectionWith (\r1 r2 -> r1 ++ tail r2) m1 m2
    b'     = R <$> M.elems m'
    n'     = dim t1 + dim t2 - 1
    cs     = (cols t1) ++ (tail $ cols t2)

index   :: Table -> M.Map Field [Field]
index t = M.fromList [(head r, r) | r <- getRows t]


-------------------------------------------------------------------
-- | Join two tables by any unique column 
-------------------------------------------------------------------

joinBy         :: Col -> Table -> Table -> Table
joinBy c t1 t2 = join t1' t2' 
  where 
    t1'        = moveColL t1 c 
    t2'        = moveColL t2 c

------------------------------------------------------------------
--- | Index table by any column 
-------------------------------------------------------------------

indexBy       :: Table -> Col -> M.Map Field [Field]
indexBy t c    
  | ok        = M.fromList [ (r !! i, r) | r <- getRows t]
  | otherwise = error $ "indexBy: " ++ show c ++ " is not a unique column!"
  where
    i         = columnIndex t c 
    ok        = isUnique    t c

-------------------------------------------------------------------
-- | Is a given column unique
-------------------------------------------------------------------

isUnique      :: Table -> Col -> Bool
isUnique t c  = not $ isDup $ project1 t c

isDup xs      = (length xs) /= (length xs') where xs' = nubOrd xs

nubOrd        = go . sort
  where
    go []     = []
    go (x:xs) = x : go ys where (_,ys) = span (x ==) xs

-------------------------------------------------------------------
-- | Project to a particular column
-------------------------------------------------------------------

project1     :: Table -> Col -> [Field]
project1 t c = [ r !! i | r <- getRows t]
  where 
    i        = columnIndex t c

project      :: Table -> [Col] -> Table
project t cs = T n cs body'
  where
    body'    = projRow is' <$> body t
    is       = (t `columnIndex`) <$> cs
    n        = length is
    is'      = sort is
    
projRow is (R xs) = R [ xs !! i | i <- is ] 


-------------------------------------------------------------------
-- | Sort table by a particular column
-------------------------------------------------------------------

sortBy     :: Table -> Col -> Table
sortBy t c = t { body = [ R fs | (_, fs) <- M.toList $ indexBy t c] }

-------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------

columnIndex     :: Table -> Col -> Int
columnIndex t c = fromJust $ elemIndex c $ cols t 
