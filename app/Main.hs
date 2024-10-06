module Main where

import Data.Char
import  Tokenizer
import Debug.Trace (trace)

data Operator = Add
    | Sub
    | Mult
    | Div
    | Assign
    | Exp
    deriving (Read, Show, Eq, Ord)

newtype Varname = Varname [Char]
    deriving (Read, Show, Eq, Ord)

data Expression = Number Double
    | Variable Varname
    | Operator Operator
    | Subexpression [Expression]
    deriving (Read, Show, Eq, Ord)

type Context = [(Varname, Double)]

parseOp :: String -> Maybe Operator

parseOp tok
    | tok == "+" = Just Add
    | tok == "-" = Just Sub
    | tok == "*" = Just Mult
    | tok == "/" = Just Div
    | tok == "=" = Just Assign
    | tok == "^" = Just Exp
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


(?<) ::  (t2 -> a) -> Maybe t2 -> Maybe a
(?<) f m = m >? f

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

parse :: [String] -> Maybe [Expression]
parse = parse' >>? addImplicitMult
    where
        parse' :: [String] -> Maybe [Expression]
        parse' [] = Just []
        parse' [tok] =  case parseExpr tok of
            Just expr -> Just [expr]
            Nothing -> Nothing
        parse' toks = if head toks /= "("
            then parseExpr (head toks) ?:  parse' (tail toks)
            else let (inner, outer) = (delimitLastWhen (==")") $ tail toks) in (parse' >>? Subexpression) inner ?: parse' outer

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

eval :: Context -> Expression -> (Maybe Double, Context)
eval ct expr =  case expr of
    Number n -> (Just n, ct)
    Variable v -> (lookup v ct, ct)
    Subexpression sub -> reduce ct sub
    _ -> (Nothing, ct)

getOp :: Operator -> Context -> Expression -> Expression -> (Maybe Expression, Context)
getOp op ct = \lhs rhs -> case eval ct lhs of
    (Just left, ct2) -> case eval ct2 rhs of
        (Just right, ct3) -> (Just . Number $ getOp' op left right, ct3)
        (Nothing, _) -> (Nothing, ct)
    (Nothing, _) -> (Nothing, ct)
    where
        getOp' :: Operator -> Double -> Double -> Double
        getOp' Add = (+)
        getOp' Sub = (-)
        getOp' Mult = (*)
        getOp' Div = (/)
        getOp' Exp = (**)
        getOp' Assign = error "Assign should never be called on this"

reduce :: Context -> [Expression] -> (Maybe Double, Context)
reduce ct [] = (Nothing, ct)
reduce ct [e] = (case e of
    Number n -> Just n
    _ -> Nothing
    , ct)
reduce ct [_, _] = (Nothing, ct)
reduce ct (lhs : (op : (rhs : es))) = case op of
    Operator o -> case getOp o ct lhs rhs of
        (Just e, c) -> reduce c (e : es)
        (Nothing, c)  -> (Nothing, c)
    _ -> (Nothing, ct)


printmany :: [String] -> IO [()]
printmany = mapM putStrLn

-- toSuffixNotation :: Infix -> Suffix
-- toSuffixNotation i = Suffix []


main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    let a = parse $ tokenize "2 + (3 - 12) ^ 2"
    let ct = []
    print a
    print (a>>= fst . reduce ct)
    return ()


