
module Language.Prolog.Syntax where


data Expr = 
     Var String
   | Str String [Expr]
   | Op String [Expr]
  deriving Show
