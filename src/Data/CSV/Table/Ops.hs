
module Data.CSV.Table.Ops (

  -- * Core Operators
    join, joinBy

  -- * Join using default row for missing keys
  , padJoin, padJoinBy

  -- * Difference of two tables
  , diff, diffBy

  -- * Index a table by a key
  , index
  , indexBy
  , uniqueBy

  -- * Order by the values of a particular column
  , sortBy

  -- * Restrict table to a subset of columns
  , project
  , project1
  , hide

  -- * Move a column to first (leftmost) position
  , moveColL

  -- * Map a function over all rows
  , mapRows
  , newColumn

  ) where

import           Text.CSV
import           System.FilePath
import           Control.Applicative ((<$>))
import           Data.Maybe
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.CSV.Table.Types


--------------------------------------------------------------------------------
-- | Sort by a given column, and ordering
--------------------------------------------------------------------------------
sortBy :: Col ->  TField -> Order -> Table -> Table
sortBy c t o tab = tab { body = L.sortBy (cmpRow i t o) (body tab)}
  where
    i            = columnIndex tab c

cmpRow :: Int -> TField -> Order -> Row -> Row -> Ordering
cmpRow i t o (R fs) (R fs') = orient o $ compareAs t (fs !! i) (fs' !! i)

compareAs :: TField -> Field -> Field -> Ordering
compareAs FStr x y = compare       x                  y
compareAs FInt x y = compare (read x :: Int)    (read y :: Int)
compareAs FDbl x y = compare (read x :: Double) (read y :: Double)

orient :: Order -> Ordering -> Ordering
orient Dsc LT = GT
orient Dsc GT = LT
orient _   x  = x

-------------------------------------------------------------------
-- | Swap two columns of a table
-------------------------------------------------------------------
moveColL      :: Table -> Col -> Table
moveColL t@(T n cs b) c
  | i == 0    = t
  | otherwise = T n (pluck i cs) [R $ pluck i r | R r <- b]
  where
     i        = columnIndex t c

pluck :: Int -> [a] -> [a]
pluck              = go []
  where
    go ls 0 (r:rs) = r : (ls +++ rs)
    go ls n (r:rs) = go (r:ls) (n-1) rs

(+++) :: [a] -> [a] -> [a]
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
    m'     = M.intersectionWith (\r1 r2 -> r1 ++ myTail r2) m1 m2
    b'     = R <$> M.elems m'
    n'     = dim t1 + dim t2 - 1
    cs     = cols t1 ++ myTail (cols t2)

index   :: Table -> M.Map Field [Field]
index t = M.fromList [(myHead r, r) | r <- getRows t]


myTail :: [a] -> [a]
myTail (_:xs) = xs
myTail _      = error "myTail!"

myHead :: [a] -> a
myHead (x:_) = x
myHead _     = error "myHead!"


-------------------------------------------------------------------
-- | Join two tables by any unique column
-------------------------------------------------------------------

joinBy         :: Col -> Table -> Table -> Table
joinBy c t1 t2 = join t1' t2'
  where
    t1'        = moveColL t1 c
    t2'        = moveColL t2 c

--------------------------------------------------------------------
-- | Differences of two tables by first column
-------------------------------------------------------------------

diff :: Table -> Table -> Table
diff t1@(T n1 c1 b1) t2 = T n1 c1 b1'
  where
    m1     = index t1
    m2     = index t2
    m1'    = M.difference m1 m2
    b1'    = R <$> M.elems m1'

--------------------------------------------------------------------
-- | Differences of two tables by any column
-------------------------------------------------------------------

diffBy :: Col -> Table -> Table -> Table
diffBy c t1 t2 = diff t1' t2'
  where
    t1'        = moveColL t1 c
    t2'        = moveColL t2 c


--------------------------------------------------------------------
-- | Join two tables by first column, using default row for missing keys
-------------------------------------------------------------------

padJoin :: Row -> Table -> Table -> Table
padJoin (R xs) t1 t2 = T n' cs b'
  where
    m1     = index t1
    m2     = index t2
    m1'    = M.difference m1 m2
    m2'    = M.mapWithKey (\k _ -> k:xs) m1
    m'     = M.intersectionWith (\r1 r2 -> r1 ++ myTail r2) m1 (M.union m2 m2')
    b'     = R <$> M.elems m'
    n'     = dim t1 + dim t2 - 1
    cs     = cols t1 ++ myTail (cols t2)

--------------------------------------------------------------------
-- | Join two tables by any unique column, using default row for missing keys
-------------------------------------------------------------------

padJoinBy c r t1 t2 = padJoin r t1' t2'
  where
    t1'             = moveColL t1 c
    t2'             = moveColL t2 c

--------------------------------------------------------------------------------
--- | Index table by unique column
--------------------------------------------------------------------------------
uniqueBy :: Table -> Col -> Table
uniqueBy t c = t { body = [ R fs | (_, fs) <- M.toList $ indexBy' True t c] }

indexBy     :: Table -> Col -> Table
indexBy t c = t { body = [ R fs | (_, fs) <- M.toList $ indexBy' False t c] }

indexBy'       :: Bool -> Table -> Col -> M.Map Field [Field]
indexBy' force t c
  | ok || force = M.fromList [ (r !! i, r) | r <- getRows t]
  | otherwise = error $ "indexBy: " ++ show c ++ " is not a unique column!"
  where
    i         = columnIndex t c
    ok        = isUnique    t c

-------------------------------------------------------------------
-- | Is a given column unique
-------------------------------------------------------------------

isUnique      :: Table -> Col -> Bool
isUnique t c  = not $ isDup $ project1 t c

isDup :: [Field] -> Bool
isDup xs      = length xs /= length xs' where xs' = nubOrd xs

nubOrd :: [Field] -> [Field]
nubOrd        = go . L.sort
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
    is'      = L.sort is

projRow :: [Int] -> Row -> Row
projRow is (R xs) = R [ xs !! i | i <- is ]

hide :: Table -> [Col] -> Table
hide t cs = project t (cols t L.\\ cs)


-------------------------------------------------------------------
-- | Map a function over all rows of a table
-------------------------------------------------------------------

mapRows   :: (RowInfo -> a) -> Table -> [a]
mapRows f = map f . rowCols

rowCols   :: Table -> [RowInfo]
rowCols t = [zip cs r | r <- rs]
  where
    cs    = cols    t
    rs    = getRows t

-------------------------------------------------------------------
-- | Map a function over all rows of a table
-------------------------------------------------------------------
newColumn :: Col -> (RowInfo -> Field) -> Table -> Table
newColumn c f t = addColumn t c (mapRows f t)

addColumn :: Table -> Col -> [Field] -> Table
addColumn t c fs = T n' cs' b'
  where
    n'   = 1 + dim t
    cs'  = cols t ++ [c]
    b'   = zipWith addField (body t) fs

addField :: Row -> Field -> Row
addField (R fs) f = R (fs ++ [f])

-------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------

columnIndex     :: Table -> Col -> Int
columnIndex t c = case L.elemIndex c (cols t) of
                    Just n  -> n
                    Nothing -> error $ "columnIndex: missing column: " ++ show c