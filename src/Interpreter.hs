module Interpreter where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import           Parser

isValue :: Expression -> Bool
isValue (VarT _)      = True
isValue (LambdaT _ _) = True
isValue (AppT _ _)    = False

freeVariables :: Expression -> S.Set Identifier
freeVariables expr =
  case expr of
    VarT x             -> S.singleton x
    AppT left right    -> freeVariables left `S.union` freeVariables right
    LambdaT id subexpr -> freeVariables subexpr S.\\ S.singleton id

rename :: Expression -> Identifier -> Identifier -> Expression
rename original@(VarT name) old new =
  if name == old then VarT new
  else original
rename (AppT left right) old new = AppT (rename left old new) (rename right old new)
rename original@(LambdaT bound subexpr) old new =
  if bound == old then original
  else LambdaT bound (rename subexpr old new)

freshIdentifierFor :: Identifier -> [Expression] -> Identifier
freshIdentifierFor prefix avoid =
  head . dropWhile (`elem` blacklist) . map (prefix ++) $ suffixes
  where
    suffixes = [""] ++ map show [1..]
    blacklist = S.unions $ map freeVariables avoid

substitute :: Expression -> Identifier -> Expression -> Expression
substitute original@(VarT target) identifier expression
  | target == identifier = expression
  | otherwise = original
substitute (AppT left right) identifier expression =
  AppT (substitute left identifier expression) (substitute right identifier expression)
substitute original@(LambdaT bound subexpression) identifier expression
  | bound == identifier = original
  | bound `S.member` freeVariables expression =
    let fresh = freshIdentifierFor bound [expression] in
    let renamed = LambdaT fresh (rename subexpression bound fresh) in
    substitute renamed identifier expression
  | otherwise = LambdaT bound $ substitute subexpression identifier expression

reduceByValue :: Expression -> Expression
reduceByValue (AppT a b) =
  if isValue a then
    if isValue b then beta a b -- Beta reduction
    else AppT a (reduceByValue b) -- Eta reduction
  else AppT (reduceByValue a) b -- Mu reduction
  where
    beta (LambdaT id expr) b = substitute expr id b
reduceByValue x = x

reduceByName :: Expression -> Expression
reduceByName (AppT a b) =
  if isValue a then beta a b
  else AppT (reduceByName a) b
  where
    beta (LambdaT id expr) b = substitute expr id b
reduceByName x = x

evaluate :: Program -> [Expression]
evaluate (P macros expr) = iterate reduceByName (expandMacros expr)
  where
    expandMacros :: Expression -> Expression
    expandMacros =
      until
        -- Until there aren't any macros to expand
        (\expr -> S.null (freeVariables expr `S.intersection` macroS))
        -- Expand the expression
        (\expr -> foldr (\varName e ->
                           case M.lookup varName macros of
                             Nothing -> e
                             Just replacement -> substitute e varName replacement) expr (freeVariables expr))

    macroS = S.fromList $ M.keys macros
