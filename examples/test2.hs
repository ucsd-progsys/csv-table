module AnalyzeLogs (collate) where

import           Data.CSV.Table
import           Text.CSV
import qualified Data.List as L
import System.FilePath ((</>))


--------------------------------------------------------------------------------
-- | Usage:
--   ghci> let gh = "examples/wi25/github.csv"
--   ghci> let fs = ["examples/wi25/wi25-00-lambda.csv", "examples/wi25/wi25-01-haskell.csv", "examples/wi25/wi25-02-random-art.csv"]
--   ghci> collectGraderResults gh fs "examples/wi25/out.csv"
--------------------------------------------------------------------------------
collectResults :: FilePath -> IO ()
collectResults dir = do
  let gh = dir </> "github.csv"
  let fs = (dir </>) <$> ["00-lambda.csv", "01-haskell.csv", "02-random-art.csv"]
  collectGraderResults gh fs (dir </> "out.csv")

--------------------------------------------------------------------------------

collectGraderResults :: FilePath -> [FilePath] -> FilePath -> IO ()
collectGraderResults ghFile resultFiles outFile = do
  let col = C "github"
  ghT   <- uniqueLoad col ghFile
  resTs <- mapM (uniqueLoad col) resultFiles
  let out = L.foldl' (addScore col)  ghT resTs
  toFile outFile out

addScore :: Col -> Table -> Table -> Table
addScore col t1 t2 = joinBy col t1 (hide t2 [C "email"])

uniqueLoad :: Col -> FilePath -> IO Table
uniqueLoad c f = (`uniqueBy` c) <$> fromFile f

-- | Usage:
--      ghci> collate ["summary-1.csv", "summary-2.csv", ... , "summary-n.csv"] "out.csv"
--
--------------------------------------------------------------------------------
collate :: [FilePath] -> FilePath -> IO Table
--------------------------------------------------------------------------------
collate fs f = do
  ts    <- mapM load fs
  let t  = transform ts
  toFile f t
  return t

--------------------------------------------------------------------------------
load :: FilePath -> IO Table
--------------------------------------------------------------------------------
load f = mkTable f <$> readFile f

mkTable :: FilePath -> String -> Table
mkTable f s = fromString f $ unlines (hdr : body)
  where
    hdr     = "test, time-" ++ stamp ++ ",result"
    body    = drop 6 ls
    stamp   = drop 17 (ls !! 2)
    ls      = lines s

--------------------------------------------------------------------------------
transform :: [Table] -> Table
--------------------------------------------------------------------------------
transform = sortBy rngC FInt Dsc
          . addRange
          . foldr1 join
          . map hideResult

hideResult :: Table -> Table
hideResult t = hide t [C "result"]

addRange :: Table -> Table
addRange = newColumn rngC range

rngC :: Col
rngC = C "range"

range :: RowInfo -> Field
range cvs = show $ truncate (hi - lo)
  where
    vs    = [ read v :: Double | (C c, v) <- cvs, "time-" `L.isInfixOf` c ]
    lo    = minimum vs
    hi    = maximum vs
