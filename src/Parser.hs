module Parser
  ( Expression(VarT, LambdaT, AppT),
    Identifier,
    Program(P),
    MacroStorage,
    Parser.parseProgramFromFile,
    Parser.parseTermFromString
  ) where

import qualified Data.Map    as M
import           Debug.Trace
import           Text.Parsec

type Identifier = String

data Expression =
  VarT Identifier
  | LambdaT Identifier Expression
  | AppT Expression Expression
  deriving (Eq)

instance Show Expression where
  show (VarT id)            = id
  show (AppT left right)    = "(" ++ show left ++ " " ++ show right ++ ")"
  show (LambdaT id subexpr) = "(λ" ++ id ++ "." ++ show subexpr ++ ")"

type MacroStorage = M.Map Identifier Expression

data Program = P MacroStorage Expression deriving (Show)

parseParenthesized :: Parsec String u a -> Parsec String u a
parseParenthesized = between (char '(') (char ')')

parseIdentifier = many1 letter

sp = oneOf " \t"

parseLambda = do
  char '\\' <|> char 'λ'
  skipMany sp
  identifier <- parseIdentifier
  skipMany sp
  char '.'
  skipMany sp
  inner <- parseTerm
  return $ LambdaT identifier inner

parseVar = do
  identifier <- parseIdentifier
  return $ VarT identifier

parseTerm = parseNonApp `chainl1` parseTermCont
  where
    parseNonApp = parseParenthesized parseTerm <|> parseLambda <|> parseVar
    parseTermCont = do
      skipMany1 sp
      return AppT

parseMacroStatement = do
  string "macro"
  skipMany1 sp
  name <- parseIdentifier
  skipMany1 sp
  expression <- parseTerm
  endOfLine
  return $ (name, expression)

parseProgram = do
  macros <- many parseMacroStatement
  expr <- parseTerm
  skipMany endOfLine <|> eof
  return $ P (M.fromList macros) expr

parseProgramFromFile :: String -> IO (Either ParseError Program)
parseProgramFromFile filename = readFile filename >>= return . (runParser parseProgram () filename)

parseTermFromString :: String -> Either ParseError Expression
parseTermFromString = runParser parseTerm () ""
