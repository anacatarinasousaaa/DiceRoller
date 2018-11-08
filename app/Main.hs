module Main where

import System.Environment
import System.Exit 
import Data.List.Split
import System.IO
import Control.Concurrent
import Dice 

main :: IO ()
main = getArgs >>= return . head >>= parse 


parse :: String -> IO () 
parse s = do
    inh <- openFile s ReadMode
    mainloop inh

mainloop :: Handle -> IO ()
mainloop inh = do
    ineof <- hIsEOF inh
    if ineof
        then return ()
        else do 
                inptStr <- hGetLine inh
                let _ = caller inptStr
                mainloop inh

caller :: String -> Int
caller s = case splitOn "d" s of
    [casas, vezes] ->  gofast (read casas :: Int) (read vezes :: Int) 
    _ -> 0
