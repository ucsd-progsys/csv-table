module Data.CSV.Table.Email 
  ( -- * Email representation 
    Email (..)
    
    -- * Send function
    , send 
    , sendMail

   ) where

import Text.Printf            (printf)
import System.Process
import Control.Monad          (forM_)
import Data.List              (intercalate)
import Data.CSV.Table.Types
import Data.CSV.Table.Ops


data Email   = E { uid     :: String 
                 , to      :: String
                 , cc      :: [String]
                 , sender  :: String
                 , subject :: String
                 , text    :: String
                 } deriving (Show)

send      :: Table -> (RowInfo -> Email) -> IO ()
send t f  = forM_ (mapRows f t) sendMail 

sendMail :: Email -> IO ()
sendMail e = do
  let tmp  = uid e
  let cmd  = mailCmd e 
  writeFile tmp (text e)  
  status  <- system cmd 
  putStrLn $ printf "[exec: %s] Status[%s]: %s %s" cmd (show status) (uid e) (to e)

mailCmd :: Email -> String
mailCmd e = 
    printf "mail -s \"%s\" -aFrom:%s %s -c %s < %s" (subject e) (sender e) (to e) (ccs e) (uid e)
  where
    ccs = intercalate "," . cc







