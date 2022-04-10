{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Attoparsec.ByteString.Char8 (isSpace)
import Data.Attoparsec.Text
import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text.IO (putStr, putStrLn, readFile)
import Prelude hiding (putStr, putStrLn, readFile)

data Expr
    = Lit Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Show)

eval :: Expr -> Int
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

parseLit :: Parser Expr
parseLit = Lit <$> decimal

parseOp :: Text -> (Expr -> Expr -> Expr) -> Parser Expr
parseOp op f = do
    num1 <- parseBracketed <|> parseLit
    skipWhile isSpace
    string op
    skipWhile isSpace
    num2 <- parseExpr <|> parseLit
    return $ f num1 num2

parseAdd :: Parser Expr
parseAdd = do
    num1 <- parseMul <|> parseBracketed <|> parseLit
    skipWhile isSpace
    string "+"
    skipWhile isSpace
    num2 <- parseExpr <|> parseBracketed
    return $ Add num1 num2

parseMul :: Parser Expr
parseMul = do
    num1 <- parseBracketed <|> parseLit
    skipWhile isSpace
    string "*"
    skipWhile isSpace
    num2 <- parseLit <|> parseBracketed
    return $ Mul num1 num2

parseBracketed :: Parser Expr
parseBracketed = do
    string "("
    expr <- parseExpr
    string ")"
    return expr

parseExpr :: Parser Expr
parseExpr = parseAdd <|> parseMul <|> parseLit

printExpr :: Text -> IO ()
printExpr input = do
    let expr = fromRight (Lit (-1)) (parseOnly parseExpr input)
    putStr input
    putStr " = "
    print $ eval expr

main :: IO ()
main = do
    forM_
        [ "1+2*3+4+5"
        , "(1+2)*(3+4)+5"
        , "2*2+3"
        , "2*(2+3)"
        , "4+(3+(1+2)*3)"
        ]
        printExpr
