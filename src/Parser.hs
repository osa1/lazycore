{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}
module Parser where


import Language

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*), (<$>))
import Control.Monad (when)


spChar :: Char -> Parser Char
spChar c = char c <* spaces

spStr :: String -> Parser String
spStr s = string s <* spaces

int :: Parser Int
int = read <$> (many1 digit <* spaces)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

program :: Parser CoreProgram
program = sc `sepBy` spChar ';'

var :: Parser Name
var = do
    firstChar <- letter
    rest <- many idChar
    when (firstChar : rest `elem` keywords) (parserFail "expected var, found keyword")
    return (firstChar : rest) <* spaces
  where
    idChar = alphaNum <|> digit <|> char '_'

sc :: Parser CoreScDefn
sc = do
    scName <- var
    args <- many var
    spChar '='
    body <- expr
    return (scName, args, body)

expr :: Parser CoreExpr
expr = choice [ try app, try infixApp, try letE, try letrecE, try caseE, try lambda, aexp ] <* spaces
  where
    app = do
        fn <- expr' <* spaces
        rest <- many1 $ expr' <* spaces
        return $ mkApp fn rest
      where
        expr' = choice (map try [ letE, letrecE, caseE, lambda, aexp ])

    mkApp = foldl EAp

    infixApp = do
      e1 <- choice (map try [ app, letE, letrecE, caseE, lambda, aexp ]) <* spaces
      fn <- binop
      e2 <- expr
      return $ EAp (EAp (EVar fn) e1) e2

    letE = do
      spStr "let"
      defs <- defns
      spStr "in"
      body <- expr
      return (ELet nonRecursive defs body)

    letrecE = do
      spStr "letrec"
      defs <- defns
      spStr "in"
      body <- expr
      return (ELet recursive defs body)

    caseE = do
      spStr "case"
      scr <- expr
      spStr "of"
      as <- alts
      return (ECase scr as)

    lambda = do
      spChar '\\'
      vars <- many1 var
      spChar '.'
      body <- expr
      return (ELam vars body)

defns :: Parser [(Name, CoreExpr)]
defns = flip sepBy1 (spChar ';') $ do
    defName <- var
    spChar '='
    body <- expr
    return (defName, body)

alts :: Parser [CoreAlt]
alts = alt `sepBy` spChar ';'
  where
    alt = do
      tag <- read <$> between (spChar '<') (spChar '>')
                              (many1 digit)
      vars <- many var
      spStr "->"
      body <- expr
      return (tag, vars, body)

binop :: Parser Name
binop = choice [ arithop, relop, boolop ]
  where
    arithop = choice [ spStr "+", spStr "-", spStr "*", spStr "/" ]
    relop = choice [ spStr "<", spStr "<=", spStr "==", spStr "~=", spStr ">=", spStr ">" ]
    boolop = choice [ spStr "&", spStr "|" ]

aexp :: Parser CoreExpr
aexp = choice [ EVar <$> try var, ENum <$> int, pack, paren ] <* spaces
  where
    pack = do
      spStr "Pack"
      spChar '{'
      tag <- int
      spChar ','
      arity <- int
      spStr "}"
      return $ EConstr tag arity

    paren = spStr "(" >> expr <* spStr ")"
