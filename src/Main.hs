module Main where

import Language.Prolog.Parser
import System.Environment

import Text.Parsec
import Text.Parsec.ByteString

main = do
    (a:as) <- getArgs
    parseProlog2 a
--    parseFromFile (many1 (satisfy (\x->True))) a
