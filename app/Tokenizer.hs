module Tokenizer(
    tokenize,
    delimitWhen,
    delimitLastWhen
) where

import Data.List.Split

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

splitAlphaNumeric :: String -> [String]
splitAlphaNumeric "" = []
-- splitAlphaNumeric str = 


tokenize :: String -> [String]
tokenize = split . dropDelims . dropBlanks $ oneOf "() \t"
