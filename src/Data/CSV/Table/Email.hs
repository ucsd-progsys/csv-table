module Data.CSV.Table.Email 
  ( -- * Email representation 
    Email (..)
    
    -- * Send function
    , send 

   ) where

import Data.CSV.Table.Types
import Data.CSV.Table.Utils

data Email   = E { uid     :: String 
                 , to      :: String
                 , cc      :: [String]
                 , sender  :: String
                 , subject :: String
                 , body    :: String
                 }

send      :: Table -> (RowInfo -> Email) -> IO ()
send t f  = forM_ (mapRows f t) sendMail 

sendMail :: Email -> IO ()
sendMail e = do 
  let cmd  = mailCmd (subject e) (sender e) (to e) (cc e) (body e)
  status  <- system cmd 
  putStrLn $ printf "[exec: %s] Status[%s]: %s %s" cmd (show status) (uid e) (to e)

mailCmd :: Email -> String
mailCmd e = 
    printf "mail -s \"%s\" -aFrom:%s %s -c %s < %s" (subject e) (sender e) (to e) (ccs e) (uid e)
  where
    ccs = intercalate "," . cc







