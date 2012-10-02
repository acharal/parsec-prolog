module Main where

import Language.Prolog.Parser
import System.Environment

import System.CPUTime
import Text.Printf



main = do
    as <- getArgs
    mapM_ parseProc as
--    parseFromFile (many1 (satisfy (\x->True))) a

parseProc filename = do
    
    do { (t, (p2,_)) <- timeItT $ parseProlog2 filename
       ; printf ("OK     " ++ filename ++ " (clauses " ++ show (length p2) ++ ", time: %6.2fs)\n") t
       } `catch` \e -> do { putStr ("FAILED " ++ filename ++ "\n"); }



-- |Wrap an 'IO' computation so that it returns execution time is seconds as well as the real value.
timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    let t :: Double
	t = fromIntegral (t2-t1) * 1e-12
    return (t, a)

