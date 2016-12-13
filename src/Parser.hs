module Parser
  ( Expression(VarT, LambdaT, AppT),
    Identifier,
    Statement(Macro, Compute),
    Program,
    Parser.parseProgramFromFile,
    Parser.parseTermFromString
  ) where

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

data Statement =
  Macro Identifier Expression
  | Compute Expression

instance Show Statement where
  show (Macro id expr) = "macro " ++ id ++ " " ++ show expr
  show (Compute expr)  = show expr

type Program = [Statement]

parseParenthesized :: Parsec String u a -> Parsec String u a
parseParenthesized = between (char '(') (char ')')

parseIdentifier :: Parsec String u String
parseIdentifier = many1 letter

parseLambda = do
  identifier <- between ((char '\\' <|> char 'λ') >> spaces) (spaces >> char '.') parseIdentifier
  spaces
  inner <- parseTerm
  return $ LambdaT identifier inner

parseVar = do
  identifier <- parseIdentifier
  return $ VarT identifier

parseTerm = between spaces spaces $ parseNonApp `chainl1` parseTermCont
  where
    parseNonApp = parseTerm <|> parseLambda <|> parseVar
    parseTermCont = do
      spaces
      return AppT

parseMacroStatement = do
  string "macro"
  spaces
  name <- parseIdentifier
  spaces
  expression <- parseTerm
  return $ Macro name expression

parseComputeStatement = do
  expression <- parseTerm
  return $ Compute expression

parseStatement = parseMacroStatement <|> parseComputeStatement

parseProgram = parseStatement `sepBy` many1 endOfLine

parseProgramFromFile :: String -> IO (Either ParseError Program)
parseProgramFromFile filename = readFile filename >>= return . (runParser parseProgram () filename)

parseTermFromString :: String -> IO (Either ParseError Expression)
parseTermFromString = return . (runParser parseTerm () "")
