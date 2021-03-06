module HL2 where

import Data.Maybe (fromMaybe)

type Env = [(String, Expr)]

extEnv :: String -> Expr -> Env -> Env
extEnv x v env = (x, v) : env

data Oper = Plus | Minus | Mult | Div | Link | Index | Or | And | NotN deriving (Show)

data Expr
    = Boolean Bool
    | Number Double
    | Str String
    | Param String
    | Do Oper Expr Expr
    | Let Expr Expr Expr
    | Lambda Expr Expr
    | Closure Expr Env
    | Call Expr Expr
    | Error String
    | Nil
    deriving (Show)

calc :: Oper -> Expr -> Expr -> Expr

calc Plus (Number n) (Number m) = Number (n + m)
calc Minus (Number n) (Number m) = Number (n - m)
calc Mult (Number n) (Number m) = Number (n * m)
calc Div (Number n) (Number m) = Number (n / m)
calc Link (Str n) (Str m) = Str (n ++ m)
calc Index (Str n) (Number m) = Str ((n !! round m):"")
calc Or (Boolean n) (Boolean m) = Boolean (n || m)
calc And (Boolean n) (Boolean m) = Boolean (n && m)
calc NotN (Boolean n) _ = Boolean (not n)
calc _ _ _ = Error "syntax error"


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

interp s@(Lambda _ _) env = Closure s env

interp (Call f e) env = case v of
                      Number _ -> callExpr
                      Boolean _ -> callExpr
                      Str _ -> callExpr
                      _ -> Error "syntax error"
                      where
                          v = interp e env
                          clo = interp f env
                          callExpr = case clo of
                                    (Closure (Lambda (Param x) fb) env') -> interp fb (extEnv x v env')
                                    _ -> Error "syntax error"

interp _ _ = Error "syntax error"

nullenv :: Env
nullenv = [("", Nil)]

hl2 :: Expr -> Expr
hl2 e = interp e nullenv

example = hl2 (Do And (Boolean True) (Boolean False))