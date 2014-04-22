
import Text.CSV
import Data.CSV.Table.Types
import Data.CSV.Table.Ops

csvFromFile f = parseCSVFromFile f >>= either (error . show) return

main = do 
  joinTest "test/tab1.csv" "test/tab2.csv" "test/tab12.csv"

joinTest f1 f2 f = do 
  t1 <- fromFile f1 
  t2 <- fromFile f2
  toFile f $ join t1 t2


