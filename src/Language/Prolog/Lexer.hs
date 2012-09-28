module Language.Prolog.Lexer (
        module Text.Parsec.Token,
        prologStyle, prologDef, prolog, quotedString
    ) where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language


prologStyle :: Stream s m Char => GenLanguageDef s u m
prologStyle = emptyDef 
               { commentStart    = "/*"
               , commentEnd      = "*/"
               , commentLine     = "%"
               , nestedComments  = False
               , identStart      = letter <|> digit <|> oneOf "_"
               , identLetter     = alphaNum <|> oneOf "_"
               , opStart         = identStart prologStyle
               , opLetter        = identLetter prologStyle
               , reservedNames   = []
               , reservedOpNames = []
               , caseSensitive   = True
               }


prologDef :: Stream s m Char => GenLanguageDef s u m
prologDef = prologStyle

prolog :: Stream s m Char => GenTokenParser s u m
prolog  = makeTokenParser prologDef

quotedString :: Stream s m Char => ParsecT s u m String
quotedString = lexeme prolog (
                   do{ str <- between (char '\'')
                                      (char '\'' <?> "end of string")
                                      (many stringChar)
                     ; return (foldr (maybe id (:)) "" str)
                     }
                   <?> "literal string")

stringChar :: Stream s m Char => ParsecT s u m (Maybe Char)
stringChar = do{ c <- stringLetter; return (Just c) }
             <|> stringEscape
             <|> escapedQuote

stringLetter :: Stream s m Char => ParsecT s u m Char
stringLetter = satisfy (\c -> (c /= '\'') && (c /= '\\'))

stringEscape :: Stream s m Char => ParsecT s u m (Maybe Char)
stringEscape = do { char '\\'
                  ;     do { escapeGap ; return Nothing }
                    <|> do { esc <- escapeCode; return (Just esc) }
                  }

escapedQuote :: Stream s m Char => ParsecT s u m (Maybe Char)
escapedQuote = try $ do { char '\'' ; c <- char '\''; return (Just c) }

escapeGap :: Stream s m Char => ParsecT s u m Char
escapeGap = do { many1 space
               ; char '\\' <?> "end of string gap"
               }

escapeCode :: Stream s m Char => ParsecT s u m Char
escapeCode = charEsc

charEsc :: Stream s m Char => ParsecT s u m Char
charEsc = choice (map parseEsc escMap)
    where parseEsc (c,code) = do{ char c; return code }
          escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")

