
import Text.Printf            (printf)
import Text.CSV
import Data.CSV.Table
import Control.Applicative ((<$>))

csvFromFile f = parseCSVFromFile f >>= either (error . show) return


main = do
  -- joinTest  "examples/tab1.csv" "examples/tab2.csv" "examples/tab3.csv"
  emailTest True "examples/tabe.csv"

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

emailTest b f = do
  t <- fromFile f
  sendMail t (makeEmail b)

makeEmail b r = E {
    uid       = "/tmp/email." ++ lookupCol (C "PID") r
  , to        = lookupCol (C "EMAIL") r
  , cc        = []
  , sender    = "santa.claus@gmail.com"
  , subject   = "ho ho ho"
  , text      = makeText r
  , send      = b
  }

makeText r = printf "ID=%s: %s eats on %s at %s"
               (lookupCol (C "PID")  r)
               (lookupCol (C "NAME") r)
               (lookupCol (C "DAY")  r)
               (lookupCol (C "HOUR") r)
