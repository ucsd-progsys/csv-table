
module Data.CSV.Table.Types (

  -- * Representation
    Table (..)
  , Row (..)
  , Col (..)

  -- * Accessors
  , getCols
  , getRows

  -- * Parsing 
  , fromString

  ) where

import           Text.CSV
import           System.FilePath
import           Control.Applicative ((<$>))
import           Data.Maybe
import           Data.List (sort, elemIndex)
import qualified Data.Map.Strict as M

newtype Col = C Field   deriving (Eq, Ord, Show)
newtype Row = R [Field] deriving (Eq, Ord, Show)

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

-----------------------------------------------------------------------------------
-- | Converting to CSV 
-----------------------------------------------------------------------------------

fromCSV        :: CSV -> Either String Table
fromCSV []     = Left  "Empty CSV with no rows!"
fromCSV (r:rs) = do let n  = length r
                    let cs = [C x | x <- r]
                    b     <- mapM (makeRow n) rs                        
                    Right  $ T n cs b 

makeRow            :: Int -> Record -> Either String Row
makeRow n xs 
  | length xs == n = Right $ R xs
  | otherwise      = Left  $ "Row: " ++ show xs ++ "does not have " ++ show n ++ "columns!"


toCSV   :: Table -> CSV
toCSV t = [c | C c <- cols t] : [xs | R xs <- body t] 

-------------------------------------------------------------------
-- | Parsing 
-------------------------------------------------------------------

fromString      :: FilePath -> String -> Either String Table
fromString fp s = fromCSV =<< parseCSV' fp s

parseCSV' fp s = case parseCSV fp s of 
                   Right c -> Right c
                   Left e  -> Left (show e)

-------------------------------------------------------------------
-- | Printing
-------------------------------------------------------------------

instance Show Table where
  show = show . toCSV


