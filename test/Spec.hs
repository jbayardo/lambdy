import           Interpreter
import           Parser
import           Test.Hspec
import           Test.QuickCheck

rightParseString :: String -> Expression
rightParseString s =
  case parseTermFromString s of
    Left err -> error err
    Right f  -> f

reduceString :: String -> Expression
reduceString s = reduce $ rightParseString s

main :: IO ()
main = hspec $ do
  describe "Parser.parseTermFromString" $ do
    it "parses a variable" $ do
      rightParseString "x" `shouldBe` VarT "x"

    it "parses a variable surrounded by parentheses" $ do
      rightParseString "(x)" `shouldBe` VarT "x"

    it "parses a single variable surrounded by multiple parentheses" $ do
      rightParseString "(((x)))" `shouldBe` VarT "x"

    it "parses a variable with a long name" $ do
      rightParseString "hello" `shouldBe` VarT "hello"

    it "parses a lambda" $ do
      rightParseString "\\x.x" `shouldBe` LambdaT "x" (VarT "x")

    it "parses an application" $ do
      rightParseString "(\\x.y) zeal" `shouldBe` AppT (LambdaT "x" (VarT "y")) (VarT "zeal")

    it "parses nested lambdas" $ do
      rightParseString "(\\x.\\y.y) z" `shouldBe` AppT (LambdaT "x" (LambdaT "y" (VarT "y"))) (VarT "z")

    it "parses application of lambdas" $ do
      rightParseString "(\\x.x x) (\\x.x x)" `shouldBe` AppT (LambdaT "x" (AppT (VarT "x") (VarT "x"))) (LambdaT "x" (AppT (VarT "x") (VarT "x")))

  describe "Interpreter.reduce" $ do
    it "can apply into the identity" $ do
      reduceString "(\\x.x) y" `shouldBe` VarT "y"

    it "reduces the hang correctly" $ do
      reduceString "(\\x.x x) (\\y.y y)" `shouldBe` rightParseString "(\\y.y y) (\\y.y y)"
