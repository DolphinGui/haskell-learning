module Tokenizer(
    tokenize,
    delimitWhen,
    delimitLastWhen,
) where

import Data.Char (isSpace, isNumber)

delimitWhen ::(a -> Bool) -> [a] -> ([a], [a])
delimitWhen c l = delimitWhen' c l []
    where
        delimitWhen':: (a -> Bool) -> [a] -> [a] -> ([a], [a])
        delimitWhen' _ [] o  = (o, [])
        delimitWhen' cond i o = if cond $ head i
            then (o, tail i)
            else delimitWhen' cond (tail i) (o ++ [head i])

delimitLastWhen :: (a -> Bool) -> [a] -> ([a], [a])
delimitLastWhen c l = do
    let (a, b) = delimitWhen c $ reverse l
    (reverse b, reverse a)

data CharClass = Whitespace | Numeric | Paren | Operator| Other
    deriving (Eq, Show)

classify :: Char -> CharClass
classify c | isSpace c = Whitespace
    | isNumber c = Numeric
    | c == '(' || c == ')'  = Paren
    | c `elem` "-+/%=*" = Operator
    | otherwise = Other


findSplit :: String -> Int
findSplit str = findSplit' str 0
    where
        findSplit' :: String -> Int -> Int
        findSplit' [_] _ = 1
        findSplit' s i = if classify (s !! i) /= classify (s !!( i + 1))
            then 0
            else 1 + findSplit' (tail s) i

splitAlphaNumeric :: String -> [String]
splitAlphaNumeric "" = []
splitAlphaNumeric str = let (x, y) = splitAt (1 + findSplit str) str in x : splitAlphaNumeric y

tokenize :: String -> [String]
tokenize str = filter (not . any isSpace) $ splitAlphaNumeric str
