
module Data.CSV.Table.Types (

  -- * Representation
    Table (..)
  , Row (..)
  , Col (..)
  , RowInfo
  , TField (..)
  , Order (..)

  -- * Accessors
  , getCols
  , getRows
  , lookupCol

  -- * Parsing
  , fromFile
  , fromString

  -- * Saving
  , toFile

  ) where

import           Text.Printf
import           Text.CSV
import           System.FilePath
import           Control.Applicative ((<$>))
import           Data.Maybe
import           Data.List (sort, elemIndex)
import qualified Data.Map.Strict as M

newtype Col  = C Field   deriving (Eq, Ord, Show)
newtype Row  = R [Field] deriving (Eq, Ord, Show)
type RowInfo = [(Col, Field)]

-----------------------------------------------------------------------------------
-- | Types
-----------------------------------------------------------------------------------

data Table  = T { dim :: Int, cols :: [Col], body :: [Row]}

{-@ measure width :: Row -> Int
    width (R xs) = (len xs)                                                     @-}

{-@ type ColsN N = {v:[Col] | (len v)   = N}                                    @-}
{-@ type RowN  N = {r:Row   | (width r) = N}                                    @-}
{-@ data Table   = T (dim :: Nat) (cols :: (ColsN dim)) (body :: [(RowN dim)])  @-}

{-@ getCols   :: t:Table -> ListN Field {(dim t)} @-}
getCols t = [c | C c <- cols t]

{-@ getRows   :: t:Table -> ListN Field {(dim t)} @-}
getRows t = [r | R r <- body t]

lookupCol :: Col -> RowInfo -> Field
lookupCol c cxs = fromMaybe err $ lookup c cxs
  where
    err         = printf "lookupCol: cannot find %s in %s" (show c) (show cxs)

--------------------------------------------------------------------------------
-- | Field Sorts
--------------------------------------------------------------------------------

data TField = FStr | FInt | FDbl
            deriving (Eq, Ord, Show)

data Order  = Asc | Dsc
            deriving (Eq, Ord, Show)
----------------------------------------------------------------------
-- | Converting to CSV
----------------------------------------------------------------------

fromCSV        :: CSV -> Table
fromCSV []     = error "fromCSV: Empty CSV with no rows!"
fromCSV (r:rs) = T n cs b
  where
    n          = length r
    cs         = [C x | x <- r]
    b          = mapMaybe (makeRow n) $ zip [0..] rs

makeRow :: Int -> (Int, Record) -> Maybe Row
makeRow n (i, xs)
  | length xs == n = Just $ R xs
  | empty xs       = Nothing
  | otherwise      = error $ printf "Row %d does not have %d columns:\n%s" i n (show xs)

empty :: Record -> Bool
empty = null . unwords

toCSV   :: Table -> CSV
toCSV t = [c | C c <- cols t] : [xs | R xs <- body t]

--------------------------------------------------------------------------------
-- | Parsing
--------------------------------------------------------------------------------

toFile   :: FilePath -> Table -> IO ()
toFile f = writeFile f . show

fromFile    :: FilePath -> IO Table
fromFile  f = fromString f <$> readFile f

fromString      :: FilePath -> String -> Table
fromString fp s = fromCSV $ parseCSV' fp s

parseCSV' fp s = case parseCSV fp s of
                   Right c -> c
                   Left e  -> error $ printf "parseCSV': %s" (show e)

--------------------------------------------------------------------------------
-- | Printing
--------------------------------------------------------------------------------

instance Show Table where
  show = printCSV . toCSV
