-- Scan, parse, and interpret an arithmetic expression

import Data.Char (isNumber, isLetter)
import Text.Printf ( printf )

data Token = Number Int | Op Operator | ParenOpen | ParenClose deriving(Eq)
data Operator = Plus | Minus | Times | Divide | Modulo | Absolute deriving(Eq)

data AST = Literal Int | BinOp BinOperator AST AST | UnOp UnOperator AST
data BinOperator = Add | Sub | Mul | Div | Mod deriving (Eq)
data UnOperator = Neg | Abs deriving (Eq)

-- Main
main = do
    input <- getLine
    let result = evaluate $ parse $ scan input
    print result

-- Scanning
scan :: String -> [Token]
scan s = scanToken s []
    where
        scanToken str tokens
            | null str = tokens
            | isNumber (head str) =
                let (num, str') = scanNumber str
                    tokens' = tokens ++ [Number num]
                in scanToken str' tokens'
            | isOperator (head str) = 
                let (op, str') = scanOperator str
                    tokens' = tokens ++ [Op op]
                in scanToken str' tokens'
            | head str == '(' = scanToken (tail str) (tokens ++ [ParenOpen])
            | head str == ')' = scanToken (tail str) (tokens ++ [ParenClose])
            | otherwise = scanToken (tail str) tokens

scanNumber :: String -> (Int, String)
scanNumber xs = (val, remaining)
    where
        (num, remaining) = span isNumber xs
        val = read num

scanOperator :: String -> (Operator, String)
scanOperator xs = (val, remaining)
    where 
        (op, remaining) = span isOperator xs
        val = toOperator op

isOperator :: Char -> Bool
isOperator '+' = True
isOperator '-' = True
isOperator '*' = True
isOperator '/' = True
isOperator '%' = True
isOperator a = isLetter a

toOperator :: String -> Operator
toOperator "+" = Plus
toOperator "-" = Minus
toOperator "*" = Times
toOperator "/" = Divide
toOperator "%" = Modulo
toOperator "abs" = Absolute
toOperator s = error ("Invalid symbol: " ++ s)

-- Parsing

-- expr := factor | factor + factor | factor - factor
-- factor := unary | unary * unary | unary / unary | unary % unary
-- unary := term | -term | abs term
-- term := int | (expr)

parse :: [Token] -> AST
parse ts = case parseExpr ts of
    Nothing -> error "Failed to parse expression"
    Just (ast, _) -> ast

parseExpr :: [Token] -> Maybe (AST, [Token])
parseExpr [] = Nothing
parseExpr ts = case parseFactor ts of
    Nothing -> Nothing
    Just (left, ts') -> loop ts' left
    where
        loop (Op Plus:ts) left = case parseFactor ts of
            Nothing -> Nothing
            Just (right, ts') -> loop ts' (BinOp Add left right)
        loop (Op Minus:ts) left = case parseFactor ts of
            Nothing -> Nothing
            Just (right, ts') -> loop ts' (BinOp Sub left right)
        loop ts left = Just (left, ts)

parseFactor :: [Token] -> Maybe (AST, [Token])
parseFactor [] = Nothing
parseFactor ts = case parseUnary ts of
    Nothing -> Nothing
    Just (left, ts') -> loop ts' left
    where
        loop (Op Times:ts) left = case parseUnary ts of
            Nothing -> Nothing
            Just (right, ts') -> loop ts' (BinOp Mul left right)
        loop (Op Divide:ts) left = case parseUnary ts of
            Nothing -> Nothing
            Just (right, ts') -> loop ts' (BinOp Div left right)
        loop (Op Modulo:ts) left = case parseUnary ts of
            Nothing -> Nothing
            Just (right, ts') -> loop ts' (BinOp Mod left right)
        loop ts left = Just (left, ts)

parseUnary :: [Token] -> Maybe (AST, [Token])
parseUnary [] = Nothing
parseUnary (Op Minus:ts) = case parseUnary ts of
    Nothing -> Nothing
    Just (unary, ts') -> Just (UnOp Neg unary, ts')
parseUnary (Op Absolute:ts) = case parseUnary ts of
    Nothing -> Nothing
    Just (unary, ts') -> Just (UnOp Abs unary, ts')
parseUnary ts = parseTerm ts

parseTerm :: [Token] -> Maybe (AST, [Token])
parseTerm (Number n:ts) = Just (Literal n, ts)
parseTerm (ParenOpen:ts) = case parseExpr ts of
    Nothing -> Nothing
    Just (expr, ts') -> Just (expr, tail ts')
parseTerm _ = Nothing

-- Evaluate
evaluate :: AST -> Int
evaluate (Literal n) = n
evaluate (BinOp op a b) = evaluateBinOp op a b
evaluate (UnOp op a) = evaluateUnOp op a

evaluateBinOp :: BinOperator -> AST -> AST -> Int
evaluateBinOp Add a b = evaluate a + evaluate b
evaluateBinOp Sub a b = evaluate a - evaluate b
evaluateBinOp Mul a b = evaluate a * evaluate b
evaluateBinOp Div a b = evaluate a `div` evaluate b
evaluateBinOp Mod a b = evaluate a `mod` evaluate b

evaluateUnOp :: UnOperator -> AST -> Int
evaluateUnOp Neg a = -evaluate a
evaluateUnOp Abs a = abs (evaluate a)

-- Printing token
instance Show Operator where
    show Plus = show '+'
    show Minus = show '-'
    show Times = show '*'
    show Divide = show '/'
    show Modulo = show '%'
    show Absolute = show "abs"

instance Show Token where
    show (Number n) = show n
    show (Op op) = show op
    show ParenOpen = show '('
    show ParenClose = show ')'

-- Printing tree
instance Show BinOperator where
    show Add = show '+'
    show Sub = show '-'
    show Mul = show '*'
    show Div = show '/'
    show Mod = show '%'

instance Show UnOperator where
    show Neg = show '-'
    show Abs = show "abs"

instance Show AST where
    show = showAtLevel 0
        where
            showAtLevel l (Literal n) = addSpace l ++ show n
            showAtLevel l (BinOp op a b) = printf "%s%s\n%s\n%s" (addSpace l) (show op) (showAtLevel (l + 1) a) (showAtLevel (l + 1) b)
            showAtLevel l (UnOp op a ) = printf "%s%s\n%s" (addSpace l) (show op) (showAtLevel (l + 1) a)
            addSpace = flip replicate '\t'