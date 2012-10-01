
module Language.Prolog.Syntax where


data Expr = 
     Var String                  -- variable
   | Str String [Expr]           -- structure
   | Op String [Expr]            -- operator
   | Cons Expr Expr              -- list
   | Num (Either Integer Double) -- number
  deriving Show
