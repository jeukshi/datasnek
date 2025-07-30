module Main where

import App qualified
import System.Environment (getArgs)
import Types (Env (..))

main :: IO ()
main = do
    args <- getArgs
    let env =
            if "--prod" `elem` args
                then Prod
                else Dev
    App.run env
