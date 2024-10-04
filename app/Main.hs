module Main where

import Data.Char
import  Tokenizer

data Operator = Add
    | Sub
    | Mult
    | Div
    | Mod
    | Assign
    deriving (Read, Show, Eq, Ord)

data Expression = Number Double
    | Variable [Char]
    | Operator Operator
    | Subexpression [Expression]
    deriving (Read, Show, Eq, Ord)


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

(?:) :: Maybe a -> Maybe [a] -> Maybe [a]
(?:) a b = case (a, b) of
    (Nothing, _) -> Nothing
    (_, Nothing) -> Nothing
    (Just x, Just y) -> Just (x : y)

try ::  [t -> Maybe a] -> t -> Maybe a
try [f] x = f x
try fs x = case head fs x of
    Just r -> Just r
    Nothing -> try (tail fs) x


parseExpr :: String -> Maybe Expression
parseExpr = try [ parseNum >>? Number, parseOp >>? Operator]

parse :: [String] -> Maybe [Expression]
parse [] = Just []
parse [tok] =  mapM (try [ parseNum >>? Number, parseOp >>? Operator]) [tok]
parse toks = if head toks /= "("
    then parseExpr (head toks) ?:  parse (tail toks)
    else parse' (delimitLastWhen (==")") $ tail toks)
        where
            parse':: ([String], [String]) -> Maybe [Expression]
            parse' (inner, outer) =  (parse >>? Subexpression) inner ?: parse outer

-- empty subexpressions either mean empty parenthensis
-- or unclosed parenthensis
validateExpr :: [Expression] -> Bool
validateExpr = elem (Subexpression [])

printmany :: [String] -> IO [()]
printmany = mapM putStrLn

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    print (parse $ tokenize "2 + ( 3 - 5 ) * 2")
    return ()


