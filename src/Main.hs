module Main where

import Language.Prolog.Parser
import System.Environment

import Text.Parsec
import Text.Parsec.ByteString



main = do
    as <- getArgs
    mapM_ parseProc as
--    parseFromFile (many1 (satisfy (\x->True))) a

parseProc filename = do
    
    do { (p2,_) <- parseProlog2 filename
       ; putStr ("OK     " ++ filename ++ " (clauses " ++ show (length p2) ++ ")\n")
       } `catch` \e -> do { putStr ("FAILED " ++ filename ++ "\n"); }
