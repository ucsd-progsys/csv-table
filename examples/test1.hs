
import Text.Printf            (printf)
import Text.CSV
import Data.CSV.Table.Types
import Data.CSV.Table.Ops
import Data.CSV.Table.Email
import Control.Applicative ((<$>))

csvFromFile f = parseCSVFromFile f >>= either (error . show) return


main = do 
  -- joinTest  "examples/tab1.csv" "examples/tab2.csv" "examples/tab3.csv"
  emailTest "examples/tabe.csv"

----------------------------------------------------------
-- | Joining Two Tables
----------------------------------------------------------

joinTest f1 f2 f = do 
  t1 <- fromFile f1 
  t2 <- fromFile f2
  toFile f $ join t1 t2

----------------------------------------------------------
-- | Email Test
----------------------------------------------------------

emailTest f = do 
  t <- fromFile f
  send t makeEmail

makeEmail r = E {
    uid       = "/tmp/email." ++ lookupCol (C "PID") r
  , to        = lookupCol (C "EMAIL") r
  , cc        = []
  , sender    = "santa.claus@gmail.com"
  , subject   = "ho ho ho"
  , text      = makeText r 
  , send      = True
  }

makeText r = printf "ID=%s: %s eats on %s at %s"
               (lookupCol (C "PID")  r)
               (lookupCol (C "NAME") r)
               (lookupCol (C "DAY")  r)
               (lookupCol (C "HOUR") r)

