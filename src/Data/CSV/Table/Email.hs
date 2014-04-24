module Data.CSV.Table.Email 
  ( -- * Email representation 
    Email (..)
    
    -- * Send function
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
                 , send    :: Bool 
                 } deriving (Show)

sendMail      :: Table -> (RowInfo -> Email) -> IO ()
sendMail t f  = forM_ (mapRows f t) sendMail1 

sendMail1 :: Email -> IO ()
sendMail1 e = do
  let tmp  = uid e
  let cmd  = mailCmd e 
  writeFile tmp (text e)  
  status  <- system cmd 
  putStrLn $ printf "[exec: %s] Status[%s]: %s %s" cmd (show status) (uid e) (to e)

mailCmd :: Email -> String
mailCmd e = 
    printf "mail -s \"%s\" -aFrom:%s %s %s < %s" (subject e) (sender e) (to e) (ccs $ cc e) (uid e)
  where
    ccs [] = ""
    ccs xs = "-c " ++ intercalate "," xs







