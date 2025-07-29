module Main where

import Api
import System.Environment (getArgs)
import Types (Env (..))

main :: IO ()
main = do
    args <- getArgs
    let env =
            if "--prod" `elem` args
                then Prod
                else Dev
    run env
