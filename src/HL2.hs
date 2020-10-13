module HL2 where

import Data.Maybe (fromMaybe)

someFunc :: IO ()
someFunc = print "Hello Haskell"

type Env = [(String, Expr)]

extEnv :: String -> Expr -> Env -> Env
extEnv x v env = (x, v) : env

data Oper = Plus | Minus | Mult | Div deriving (Show)

data Expr
    = Boolean Bool
    | Number Double
    | Str String
    | Param String
    | Do Oper Expr Expr
    | Let Expr Expr Expr
    | Error String
    | Nil
    deriving (Show)

calc :: Oper -> Expr -> Expr -> Expr

calc Plus (Number n) (Number m) = Number (n + m)
calc Minus (Number n) (Number m) = Number (n - m)
calc Mult (Number n) (Number m) = Number (n * m)
calc Div (Number n) (Number m) = Number (n / m)
calc _ _ _ = Error "syntax error"
-- TODO

interp :: Expr -> Env -> Expr

interp b@(Boolean _) _ = b
interp n@(Number _) _ = n
interp s@(Str _) _ = s
interp Nil _ = Nil

interp (Do op n m) env = calc op n' m'
                        where
                            n' = interp n env
                            m' = interp m env

interp (Let (Param x) e1 e2) env = interp e2 (extEnv x (interp e1 env) env) 
-- (Let (Param "x") 10 (Do Plus (Param "x") (Number 1000)))
-- is equal to "let x = 10 in x + 1000"

interp (Param x) env = fromMaybe (Error ("undefined variable: " ++ x)) (lookup x env)

interp _ _ = Error "syntax error"

nullenv :: Env
nullenv = [("", Nil)]