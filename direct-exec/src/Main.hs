{-# LANGUAGE TemplateHaskell #-}

module Main where

import Parse
import Control.Monad
import Language.Haskell.TH (pprint, runQ)

import Data.Char

$(genParser)

{-
main = do
    code <- runQ genParser
    putStrLn $ pprint code
-}