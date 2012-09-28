
module Language.Prolog.Parser where

import qualified Language.Prolog.Lexer as L
import Language.Prolog.Syntax
import qualified Language.Prolog.Operators as Operators
import Text.Parsec
import Text.Parsec.Expr
import Data.Char
import Data.List


data ParseState = 
    ParseSt { operatorTable :: Operators.OperatorTable } 

type ParserT s m a = ParsecT s ParseState m a


-- lexer 

commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep  = L.commaSep1 L.prolog

commaSep1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep1 = L.commaSep1 L.prolog

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = L.parens L.prolog

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = L.lexeme L.prolog

identifier :: Stream s m Char => ParsecT s u m String
identifier = L.identifier L.prolog

dot :: Stream s m Char => ParsecT s u m String
dot = L.dot L.prolog

inf :: Stream s m Char => ParsecT s u m String
inf = L.symbol L.prolog ":-"

-- utils

-- located p = do { b <- getPosition; p; e <- getPosition }

-- listExpr (Op "," ls) = concatMap listExpr ls
-- listExpr e = [e]

-- parser

variable :: Stream s m Char => ParserT s m Expr
variable =  do { s <- identifier
               ; if (isUpper (head s))
                 then return (Var s)
                 else unexpected ("lower first letter")
               } <?> ("variable")

struct :: Stream s m Char => ParserT s m Expr
struct =  do { s <- L.quotedString <|> identifier
             ; as <- option [] args
             ; if (isUpper (head s))
               then unexpected ("upper first letter") 
               else return (Str s as)
              } <?> ("structure")
    where args  = parens (commaSep1 expr')
          expr' = parens expr <|> term

term :: Stream s m Char => ParserT s m Expr
term = variable <|> struct <?> ("term")


expr :: Stream s m Char => ParserT s m Expr
expr = do {  opTable <- buildOpTable 
          ;  buildExpressionParser opTable term'
          }
    where
          term' = term <|> parens expr

buildOpTable :: Stream s m Char => ParsecT s ParseState m (OperatorTable s ParseState m Expr)
buildOpTable = do { st <- getState
                  ; return $ mkOpTable (operatorTable st)
                  }
    where mkOpTable ops = map (map opMap) $ Operators.groupByPrec ops

          opMap op | Operators.isPrefixOp  op = prefixOp (Operators.opName op)
                   | Operators.isPostfixOp op = postfixOp (Operators.opName op)
                   | otherwise                = infixOp (Operators.opName op) (assocMap op)
                    where oper f name = try $ do { L.symbol L.prolog name; return (f name) }
                          infixOp name assoc = Infix   (oper (\n -> \x -> \y -> Op n [x,y]) name) assoc
                          prefixOp name      = Prefix  (oper (\n -> \x -> Op n [x]) name)
                          postfixOp name     = Postfix (oper (\n -> \x -> Op n [x]) name)

                          assocMap op | Operators.isAssocLeft  op = AssocLeft
                                      | Operators.isAssocRight op = AssocRight
                                      | otherwise                = AssocNone

literal :: Stream s m Char => ParserT s m Expr
literal = expr <?> ("literal")

body :: Stream s m Char => ParserT s m Expr
body = expr <?> ("body literal")

fact :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
fact = do {  h <- struct; dot; return (Just h, Nothing); }

rule :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
rule = do 
   h <- struct
   b <- between inf dot body
   return (Just h, Just b)

clause :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
clause = try fact <|> rule

command :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
command = do { b <- between inf dot body
             ; return (Nothing, Just b) 
             }

goal :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
goal = do { b <- between q dot body
          ; return (Nothing, Just b)
          }
   where q = L.symbol L.prolog "?-"

sentence :: Stream s m Char => ParserT s m (Maybe Expr, Maybe Expr)
sentence = clause <|> command' <|> goal
    where command' = do { c <- command; opDirective1 c; return c }

initProlog :: Stream s m Char => ParserT s m ()
initProlog = updateState (\s -> s{ operatorTable = ops }) 
    where ops = [(Operators.Operator "," "xfx", 1000)]


-- | Directives must be only in goal clause (without head literal)

opDirective1 (Nothing, Just e) = opDirective e
opDirective1 _ = return ()

opDirective (Str "op" [(Str p []), (Str a []), (Str n [])]) =
          updateState (updateOpTable op)
    where updateOpTable op st = st{ operatorTable = t }
               where t = Operators.updateOpTable (operatorTable st) op

          op  = mkOp ((read p)::Int, a, n)
          mkOp (p, a, n) = (Operators.Operator n a, p)

opDirective _ = return ()

-- top-level parsing
-- TODO: Parse block { }
-- TODO: Parse lists [ ]
-- TODO: Parse anonymous variable _
-- TODO: Parse operators in head
-- TODO: Parse predicates of form module:predicate.
-- TODO: Attach position to AST

parseProlog p input = 
    case runPrologParser of 
        Left err -> do putStr "parse error at" 
                       print err
        Right x -> print x
    where runPrologParser =  runP ( initProlog >> L.whiteSpace L.prolog >> p) st "" input
          st = ParseSt []


