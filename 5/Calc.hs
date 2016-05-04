{-# LANGUAGE TypeSynonymInstances,FlexibleInstances  #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as S
import qualified Data.Map as M

--Exercise 1

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

--Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

--Exercise 4

newtype MinMax= MinMax Integer deriving (Eq, Show)
newtype Mod7= Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
    | x < 1 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 (mod (x + y) 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (mod (x * y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5

instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | VarVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit
  add = VarAdd
  mul = VarMul

instance HasVars VarExprT where
  var = VarVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x m= Just x
  add x y m = (+) <$> x m <*> y m
  mul x y m = (*) <$> x m <*> y m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


