{-
 - Copyright © 2017-2018 The Charles Stark Draper Laboratory, Inc. and/or Dover Microsystems, Inc.
 - All rights reserved. 
 -
 - Use and disclosure subject to the following license. 
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 - 
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 - 
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}
module PolicyLexer where


import Text.Megaparsec
import Text.Megaparsec.String

import qualified Text.Megaparsec.Lexer as L
import Control.Monad.State.Lazy

import CommonTypes

lineComment :: Parser ()
lineComment = L.skipLineComment "//"
blockComment :: Parser ()
blockComment = L.skipBlockComment "/*" "*/"

spaceConsumer :: Parser ()
spaceConsumer = L.space (void spaceChar) lineComment blockComment

{-
nl :: DPLParser ()
nl = do
  l <- lift eol
  return ()
-}

-- special case for handling line delimited parsing, doesn't eat nl whitespace

lineConsumer :: Parser ()
lineConsumer = L.space (void $ oneOf " \t") lineComment blockComment

lexeme :: DPLParser a -> DPLParser a
lexeme p = do
  sc <- get
  tokn <- p
  lift sc
  return tokn

--symbol    = L.symbol sc
symbol :: String -> DPLParser String
symbol sym = do
  sc <- get
  tokn <- lift (L.symbol sc sym)
  return tokn

parens :: StateT SpaceParser Parser a -> StateT SpaceParser Parser a
parens    = between (symbol "(") (symbol ")")

braces :: StateT SpaceParser Parser a -> StateT SpaceParser Parser a
braces    = between (symbol "{") (symbol "}")

angles :: StateT SpaceParser Parser a -> StateT SpaceParser Parser a
angles    = between (symbol "<") (symbol ">")

brackets :: StateT SpaceParser Parser a -> StateT SpaceParser Parser a
brackets  = between (symbol "[") (symbol "]")

semi :: DPLParser String
semi      = symbol ";"

comma :: DPLParser String
comma     = symbol ","

colon :: DPLParser String
colon     = symbol ":"

dblColon :: DPLParser String
dblColon  = symbol "::"

dot :: DPLParser String
dot       = symbol "."

matchEq :: DPLParser String
matchEq    = symbol "=="

-- Special case to give good error message when rejecting "==" where "=" is
-- intended.  There is probably a cleaner way to do this.
assignEq :: DPLParser String
assignEq    =
  (symbol "==" >> fail "\"==\" used where \"=\" expected") <|> symbol "="

underbar :: DPLParser String
underbar  = symbol "_"

integer :: DPLParser Integer
integer = do
  sc <- get
  int <- lift (L.signed sc L.integer)
  lift sc
  return int
hexadecimal :: DPLParser Integer  
hexadecimal = lexeme (char '0' >> char' 'x' >> L.hexadecimal)

nonIndented :: DPLParser a -> DPLParser a
nonIndented = L.nonIndented (lift spaceConsumer)

indentBlock
  :: StateT
     SpaceParser Parser (L.IndentOpt (StateT SpaceParser Parser) a b)
     -> StateT SpaceParser Parser a

indentBlock = L.indentBlock (lift spaceConsumer)


indMany :: Maybe Pos -> ([b] -> m a) -> m b -> L.IndentOpt m a b
indMany = L.IndentMany
indSome :: Maybe Pos -> ([b] -> m a) -> m b -> L.IndentOpt m a b
indSome = L.IndentSome

indentMany :: Monad m => ([b] -> a) -> m b -> L.IndentOpt m a b
indentMany typ parser = L.IndentMany Nothing (return . typ) parser

indentSome :: Monad m => ([b] -> a) -> m b -> L.IndentOpt m a b
indentSome typ parser = L.IndentSome Nothing (return . typ) parser

