{-# LANGUAGE TupleSections #-}
module Main where

import Data.Char
import  Tokenizer
import qualified Data.Map as Map
import Data.Map ((!?))
import qualified Data.Sequence as Map

data Operator = Add
    | Sub
    | Mult
    | Div
    | Mod
    | Assign
    deriving (Read, Show, Eq, Ord)

newtype Varname = Varname [Char]
    deriving (Read, Show, Eq, Ord)

data Expression = Number Double
    | Variable Varname
    | Operator Operator
    | Subexpression [Expression]
    deriving (Read, Show, Eq, Ord)

newtype Infix = Infix [Expression]
    deriving (Read, Show, Eq, Ord)
newtype Suffix = Suffix [Expression]
    deriving (Read, Show, Eq, Ord)

type Context = Map.Map Varname Double

parseOp :: String -> Maybe Operator

parseOp tok
    | tok == "+" = Just Add
    | tok == "-" = Just Sub
    | tok == "*" = Just Mult
    | tok == "/" = Just Div
    | tok == "%" = Just Mod
    | tok == "=" = Just Assign
    | otherwise = Nothing

parseNum :: String -> Maybe Double

parseNum tok
    | all isNumber tok = Just (read tok)
    | otherwise = Nothing

(>>?) :: (t1 -> Maybe t2) -> (t2 -> a) -> t1 -> Maybe a
(>>?) b c a
  = case b a of
      Just r -> Just $ c r
      Nothing -> Nothing

(>?) :: Maybe t2 -> (t2 -> a) -> Maybe a
(>?) m f = case m of
    Just r -> Just $ f r
    Nothing -> Nothing

(?:) :: Maybe a -> Maybe [a] -> Maybe [a]
(?:) a b = case (a, b) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just x, Just y) -> Just (x : y)

(||?) :: (a -> Maybe b) -> (a -> Maybe b) -> (a -> Maybe b)
(||?) b c a =  case b a of
    Just x -> Just x
    Nothing -> c a

parseExpr :: String -> Maybe Expression
parseExpr  = (parseNum >>? Number) ||? (parseOp >>? Operator) ||? (Just . Variable . Varname)


addImplicitMult :: [Expression] -> [Expression]
addImplicitMult [] = []
addImplicitMult [a] = [a]
addImplicitMult (lhs :(rhs : es)) = if isValue lhs && isValue rhs
    then [lhs, Operator Mult, rhs]
    else [lhs, rhs]
    ++ addImplicitMult es

parse :: [String] -> Maybe Infix
parse = parse' >>? addImplicitMult >>? Infix
    where
        parse' :: [String] -> Maybe [Expression]
        parse' [] = Just []
        parse' [tok] =  case parseExpr tok of
            Just expr -> Just [expr]
            Nothing -> Nothing
        parse' toks = if head toks /= "("
            then parseExpr (head toks) ?:  parse' (tail toks)
            else (\(inner, outer) ->  (parse' >>? Subexpression) inner ?: parse' outer) (delimitLastWhen (==")") $ tail toks)

-- empty subexpressions either mean empty parenthensis
-- or unclosed parenthensis
validateExpr :: [Expression] -> Bool
validateExpr = elem (Subexpression [])

isValue ::  Expression -> Bool
isValue e = case e of
    Operator _ -> False
    _ -> True

isSubexpr :: Expression -> Bool
isSubexpr e = case e of
    Subexpression _ -> True
    _ -> False


printmany :: [String] -> IO [()]
printmany = mapM putStrLn

reduce :: ([Expression]->Expression) -> Expression -> Expression
reduce filt expr = case expr of
    Subexpression sub -> filt sub
    other -> other

-- toSuffixNotation :: [Expression] -> [Expression]

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    print ( parse $ tokenize "2 + (3 - 5) * 2" )
    return ()


