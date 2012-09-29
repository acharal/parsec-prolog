module Language.Prolog.Operator where

import Data.List

data Operator = Operator { opName :: String,  opAssoc :: String }
   deriving Eq

type OperatorTable = [(Operator, Int)]



precOp (op,prec) = prec

updateOpTable table op = op : deleteBy eqOp op table
   where eqOp (op1, _) (op2, _) = op1 == op2

groupByPrec table = map (map fst) $ groupBy samePrec $ sortBy precComp table
   where precComp op1 op2  = compare (precOp op1) (precOp op2)
         samePrec op1 op2  = precOp op1 == precOp op2


isPrefixOp   (Operator _ s) = length s == 2 && head s == 'f'
isPostfixOp  (Operator _ s) = length s == 2 && last s == 'f'
isInfixOp    (Operator _ s) = length s == 3 && head (tail s) == 'f'

isAssocRight op@(Operator _ s) = isInfixOp op && last s == 'x'
isAssocLeft  op@(Operator _ s) = isInfixOp op && not (isAssocRight op) && head s == 'x'
isAssocNone  op@(Operator _ s) = not (isAssocRight op) && not (isAssocLeft op)


