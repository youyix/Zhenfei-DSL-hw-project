-- Homework 7 DSL 
-- @author Zhenfei Nie<zhenfei.nie@gmail.com>
-- @author Xiyu Lyu<xiyu.lyu@gmail.com>

-- We did first question together.
-- We discussed together about the details of second part, the interpreter.
-- Zhenfei finally finised the `fv` function and Xiyu did the `subst`. 
-- We actually did the `step` funtion. However, there is a bug in this program and finally
-- are not able to solve it. We guess the problem happens at the `subst` function. 
-- We hope we can ask this question in Office Hour.

-- This program works well with these inputs below.
--(function(f){return f(10)})(function(x) {return x + 10;})
--(function(x){ return (function(y){return x+y;})(20)})(10)
--(function(x){ return x*x;})(10)

-------------------------------
-- # 1 Scoping and evaluation
-------------------------------
-- A)
{-- HASKELL *************
x = 3

f :: Int
f = let x = 2 in x

g :: Int -> Int
g a = a + x 

**************** --}
-- static scope
-- call `f` then `g 5`, the result is 8

-- dynamic scope
-- call `f` then `g 5`, the result is 7

-- B)
{- *** JS CODE  ************
function foo(a, b) {
    return a > 0 ? (a+1) : b;
}
***************************** -}
-- because JS is 'call-by-value', then `foo(3, 4/0)` arise a 'divided by zero exception'
-- while if it was 'call-by-need', then `foo(3, 4/0)` just return '4'. 

-------------------------------
-- # 2 Interpreter
-------------------------------
module Main where
import Text.Parsec hiding (token)
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative((<$>), (<*>))
import System.Console.Haskeline
import System.Environment (getArgs)
import Data.Fixed (mod')
import Data.List ((\\))
import Data.Set (Set, empty, singleton, delete, union, member, toList, intersection, fromList)



-- The abstract syntax
type Id = String

-- Binary operators
data Op = Add | Sub | Mul | Div | Mod
        | Lt | Gt | Le | Ge
        | Eq | SEq | Ne | SNe
        | And | Or
  deriving (Show, Eq, Read)

-- Unary operators
data UOp = Minus | Not deriving (Show, Eq, Read)
           
-- Expressions
data Exp = Un UOp Exp         {- A unary operation -}
         | Bin Op Exp Exp     {- A binary operation -}
         | Tern Exp Exp Exp   {- e0 ? e1 : e2 -}
         | Undefined          {- the undefined constant -}
         | K Double           {- numbers -}
         | B Bool             {- booleans -}
         | Fun Id Exp         {- function (x) { return e } -}
         | App Exp Exp        {- function application: e1(e2) -}
         | Var Id             {- x -}
  deriving (Eq, Read)


------------------------------------------------------------------------
------------------------------------------------------------------------
-- Free variables
-- Return the free variables in the given term.
fv :: Exp -> Set Id

fv (Var x)        = singleton x
fv (App e1 e2)    = (fv e1) `union` (fv e2) -- fvApp (fv e1) (fv e2) 
fv (Fun x e)      = delete x (fv e)  

fv (Un op e)      = fv e
fv (Bin op e1 e2) = fv e1 `union` fv e2
fv (Tern c e1 e2) = fv c `union` fv e1 `union` fv e2
fv (K n)          = empty
fv (B b)          = empty
fv Undefined      = empty
--fv e              = error $ "missing case: no fv for " ++ (show e)

fvApp :: Set Id -> Set Id -> Set Id
fvApp s1 s2 = union su $ (fromList $ map (makeFresh' su) li )
    where 
        su = s1 `union` s2
        li = toList $ s1 `intersection` s2

makeFresh' :: Set Id -> Id -> Id
makeFresh' exclude base = makeFresh base exclude


-- Substitution
-- (subst x v e) substitutions v for x in e -- e[x:=v]
subst :: Id -> Exp -> Exp -> Exp

subst x v (Var y)            = if x == y then v else (Var y) -- ok
subst x v (App e1 e2)        = App (subst x v e1) (subst x v e2)
subst x v (Fun i e)          = if x == i then Fun i e else Fun i (subst x v e) -- x = i, biao shi zi han shu fu gai
subst x v (Un op e)          = Un op (subst x v e)
subst x v (Bin op e1 e2)     = Bin op (subst x v e1) (subst x v e2)
subst x v (Tern e1 e2 e3)    = Tern (subst x v e1) (subst x v e2) (subst x v e3)
subst x v (K n)              = K n
subst x v (B b)              = B b
subst x v Undefined          = Undefined
--subst x v e                  = error $ "missing case: no subst for " ++ (show e)

-- Create a fresh variable name that isn't in the exclude list
-- The exclude list is typically a list of free variables.
-- Ex: makeFresh "y" (fromList ["x"])       --> "y"
-- Ex: makeFresh "y" (fromList ["x", "y"])  --> "y'"
-- Ex: makeFresh "y" (fromList ["y", "y'"]) --> "y''"
makeFresh :: Id -> Set Id -> Id
makeFresh base exclude
  | base `member` exclude = makeFresh (base ++ "'") exclude
  | otherwise             = base

-- The evaluator
step :: Exp -> Exp

-- Values
step e | value e = e

-- Function application (i.e., beta reduction)
step (App x y) 
  | value y = case x of 
                (Fun i e) -> case y of 
                        (App x'' y'') -> step $ subst i (step (App x'' y'') ) e
                        otherwise -> step $ subst i y e
                otherwise -> case x of 
                        (App x' y') ->  case (step (App x' y')) of 
                              (Fun i' e') -> step $ subst i' y e'
                              otherwise -> step y
                        otherwise -> step y
  | otherwise = step y
--step (App x y) 
--  | value y = case x of 
--                (Fun i e) -> step $ subst i y e
--                otherwise -> step y
--  | otherwise = step y


-- Unary congruence
step (Un op x)
  | not (value x) = Un op (step x)

-- Unary computation
step (Un Minus e) = K (-(toNumber e))
step (Un Not e)   = B (not (toBool e))

-- Ternary congruence
step (Tern c e1 e2)
  | not (value c) = Tern (step c) e1 e2

-- Ternary computation
step (Tern c e1 e2)
  | toBool c  = e1
  | otherwise = e2

-- First handle && and || where the first operand is a value.
-- We need to do this before the usual congruence rules to get
-- short-circuiting right.
step (Bin And x y)
  | value x = if toBool x then y else x

step (Bin Or x y)
  | value x = if toBool x then x else y

-- Binary congruence
step (Bin op x y)
  | not (value x)            = Bin op (step x) y
  | value x && not (value y) = Bin op x (step y)

-- Binary computation...

-- arithmetic
step (Bin Add x y) = K (toNumber x + toNumber y)
step (Bin Sub x y) = K (toNumber x - toNumber y)
step (Bin Mul x y) = K (toNumber x * toNumber y)
step (Bin Div x y) = K (toNumber x / toNumber y)
step (Bin Mod x y) = K (toNumber x `rem'` toNumber y)
  where
    rem' :: Double -> Double -> Double
    rem' x y
      | isNaN x        = nan
      | isNaN y        = nan
      | isInfinite x   = nan
      | y == 0         = nan
      | isInfinite y   = x
      | x < 0 && y > 0 = 0 - mod' (-x) y
      | x > 0 && y < 0 = mod' x (-y)
      | otherwise      = mod' x y

-- relational
step (Bin Lt x y)  = B (toNumber x < toNumber y)
step (Bin Gt x y)  = B (toNumber x > toNumber y)
step (Bin Le x y)  = B (toNumber x <= toNumber y)
step (Bin Ge x y)  = B (toNumber x >= toNumber y)

-- strict equality and disequality
step (Bin SEq (B x) (B y)) = B (x == y)
step (Bin SEq (K x) (K y)) = B (x == y)
step (Bin SEq (K x) (B y)) = B False
step (Bin SEq (B x) (K y)) = B False
step (Bin SEq Undefined Undefined) = B True
step (Bin SEq (Fun _ _) (Fun _ _)) = B False -- wrong
step (Bin SEq v1 v2) = B False

step (Bin SNe (B x) (B y)) = B (x /= y)
step (Bin SNe (K x) (K y)) = B (x /= y)
step (Bin SNe (K x) (B y)) = B True
step (Bin SNe (B x) (K y)) = B True
step (Bin SNe Undefined Undefined) = B False
step (Bin SNe (Fun _ _) (Fun _ _)) = B True -- wrong
step (Bin SNe v1 v2) = B True

-- non-strict equality and disequality
step (Bin Eq (B x) (B y)) = B (x == y)
step (Bin Eq (K x) (K y)) = B (x == y)
step (Bin Eq (K x) (B y)) = B (toNumber (K x) == toNumber (B y))
step (Bin Eq (B x) (K y)) = B (toNumber (B x) == toNumber (K y))
step (Bin Eq Undefined Undefined) = B True
step (Bin Eq (Fun _ _) (Fun _ _)) = B False -- wrong
step (Bin Eq v1 v2) = B False

step (Bin Ne (B x) (B y)) = B (x /= y)
step (Bin Ne (K x) (K y)) = B (x /= y)
step (Bin Ne (K x) (B y)) = B (toNumber (K x) /= toNumber (B y))
step (Bin Ne (B x) (K y)) = B (toNumber (B x) /= toNumber (K y))
step (Bin Ne Undefined Undefined) = B False
step (Bin Ne (Fun _ _) (Fun _ _)) = B True -- wrong
step (Bin Ne v1 v2) = B True

-- values
step v @ (K _)     = v
step v @ (B _)     = v
step v @ Undefined = v
step v @ (Fun _ _) = v

-- catch missing patterns
step (Var x) = error $ "free variable " ++ x ++ " (subst is broken)"
step e = error $ "missing pattern " ++ (show e)

-- is this term a value?
value (K _) = True
value (B _) = True
value Undefined = True
value (Fun _ _) = True
value _     = False

----------------------------------------------------
-- evaluate an expression given a step function, returning the trace of
-- intermediate terms
-- stop stepping when the term does not change
trace :: Exp -> [Exp]
trace e
  | value e   = [e]
  | otherwise = e : trace (step e)

-- evaluate an expression given a step function
eval :: Exp -> Exp
eval e
  | value e   = e
  | otherwise = eval (step e)

-- name checker
-- check the top-level term is closed
checkNames :: Monad m => Exp -> (Id -> m ()) -> m () -> m ()
checkNames e printLn performEval =
  case toList $ fv e of
    [] -> performEval
    xs -> mapM_ printLn $ map (\x -> "variable " ++ x ++ " not found") xs


------------------------------------------------------------------------
------------------------------------------------------------------------
-- main and the REPL
------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  if null args then
    runInputT defaultSettings readEvalPrintLoop
  else do
    input <- readFile $ head args
    ok <- replCmd putStr root input
    return ()

replCmd :: Monad m => (String -> m ()) -> Parser Cmd -> String -> m Bool
replCmd outputStr parser line = case parse parser "input" line of
  Right (Trace e) -> do
    checkNames e outputStrLn (outputStr $ unlines $ map show $ trace e)
    return True
  Right (Eval e)  -> do

    checkNames e outputStrLn (outputStrLn $ show $ eval e)
    return True
  Right Quit ->
    return False
  Left err -> do
    outputStrLn $ show err
    return True
  where
    outputStrLn = outputStr . (++ "\n")

readEvalPrintLoop :: InputT IO ()
readEvalPrintLoop = do
  maybeLine <- getInputLine "> "
  case maybeLine of
    Nothing -> return ()
    Just line -> do
      ok <- replCmd outputStr command line
      if ok then readEvalPrintLoop else return ()

-- REPL commands
data Cmd = Trace Exp | Eval Exp | Quit

-- A simple parser
command :: Parser Cmd
command = (do
    spaces
    c <- (do { char ':'; spaces; (trace <|> quit)}) <|> eval
    spaces
    eof
    return c) <?> "expression, :trace expression, or :quit"
  where
    eval  = do { x <- expr; return (Eval x) }
    trace = do { string "trace"; spaces; x <- expr; return (Trace x) }
    quit  = do { string "quit"; return Quit }

------------------------------------------------------------------------
------------------------------------------------------------------------
-- expression parser
------------------------------------------------------------------------
root :: Parser Cmd
root = (do
    spaces
    e <- expr
    eof
    return $ Eval e) <?> "top-level expression"

expr :: Parser Exp
expr = (try fun) <|> cond <?> "expression"

-- Ensure all 'char' and 'string' are followed by 'spaces'.
-- These should be the only places 'spaces' are needed.
token :: String -> Parser ()
token s = do { string s; spaces; return () }

tchar :: Char -> Parser ()
tchar c = do { char c; spaces; return () }

fun :: Parser Exp
fun = do
  token "function"
  tchar '('
  x <- ident
  tchar ')'
  tchar '{'
  token "return"
  e <- expr
  option () (do { tchar ';'; return ()})
  tchar '}'
  return $ Fun x e

cond :: Parser Exp
cond = do
  c <- try binary
  option c (do
    tchar '?'
    t <- expr
    tchar ':'
    e <- expr
    return $ Tern c t e)

binary :: Parser Exp
binary = buildExpressionParser table factor <?> "expression" where
  table = 
    [
    [Prefix (do { tchar '-'; return $ Un Minus })],
    [Prefix (do { tchar '!'; return $ Un Not })],
    [Infix (do { tchar '*'; return $ Bin Mul }) AssocLeft,
     Infix (do { tchar '/'; return $ Bin Div }) AssocLeft,
     Infix (do { tchar '%'; return $ Bin Mod }) AssocLeft],
    [Infix (do { tchar '+'; return $ Bin Add }) AssocLeft,
     Infix (do { tchar '-'; return $ Bin Sub }) AssocLeft],
    [Infix (do { try $ token ">="; return $ Bin Ge }) AssocLeft,
     Infix (do { tchar '>'; return $ Bin Gt }) AssocLeft,
     Infix (do { try $ token "<="; return $ Bin Le }) AssocLeft,
     Infix (do { tchar '<'; return $ Bin Lt }) AssocLeft],
    [Infix (do { try $ token "==="; return $ Bin SEq }) AssocLeft,
     Infix (do { token "=="; return $ Bin Eq }) AssocLeft,
     Infix (do { try $ token "!=="; return $ Bin SNe }) AssocLeft,
     Infix (do { try $ token "!="; return $ Bin Ne }) AssocLeft],
    [Infix (do { token "&&"; return $ Bin And }) AssocLeft],
    [Infix (do { token "||"; return $ Bin Or }) AssocLeft]
    ]

par :: Parser Exp
par = (do { tchar '('; x <- expr; tchar ')'; return x })
        <?> "parenthesized expression"

call :: Parser Exp
call = do
  f <- var <|> par
  es <- many par
  return $ foldl App f es 

factor :: Parser Exp
factor = (do 
            f <- number <|> try boolean <|> try undef <|> call
            return f) <?> "simple expression"

sign :: Parser Char
sign = char '+' <|> char '-'

makeNumber '+' ds [] = ds
makeNumber '+' ds mds = ds ++ "." ++ mds
makeNumber '-' ds [] = "-" ++ ds
makeNumber '-' ds mds = "-" ++ ds ++ "." ++ mds

number :: Parser Exp
number = (K . read) <$> (do { s <- option '+' sign;
                              ds <- many1 digit;
                              mds <- option "0" $ do { char '.'; ds' <- many digit; return ds'};
                              spaces;
                              return $ makeNumber s ds mds})

keywords = ["true", "false", "undefined", "function", "return"]

ident :: Parser Id
ident = do 
  c <- letter
  s <- many (letter <|> digit)
  spaces
  let x = c:s in if x `elem` keywords then fail "couldn't match identifier" else return x

var :: Parser Exp
var = Var <$> ident

undef :: Parser Exp
undef = do {token "undefined"; return Undefined}

boolean :: Parser Exp
boolean =  B <$> (do {token "true"; return True} <|> do {token "false"; return False})


------------------------------------------------------------------------
------------------------------------------------------------------------
-- A simple pretty-printer for Exps
------------------------------------------------------------------------
instance Show Exp where
  show (K n) = show n
  show (B True) = "true"
  show (B False) = "false"
  show Undefined = "undefined"
  show e @ (Bin Add e1 e2) = (paren e1) ++ " + " ++ (paren e2)
  show e @ (Bin Sub e1 e2) = (paren e1) ++ " - " ++ (paren e2)
  show e @ (Bin Mul e1 e2) = (paren e1) ++ " * " ++ (paren e2)
  show e @ (Bin Div e1 e2) = (paren e1) ++ " / " ++ (paren e2)
  show e @ (Bin Mod e1 e2) = (paren e1) ++ " % " ++ (paren e2)
  show e @ (Bin And e1 e2) = (paren e1) ++ " && " ++ (paren e2)
  show e @ (Bin Or e1 e2) = (paren e1) ++ " || " ++ (paren e2)
  show e @ (Bin Lt e1 e2) = (paren e1) ++ " < " ++ (paren e2)
  show e @ (Bin Gt e1 e2) = (paren e1) ++ " > " ++ (paren e2)
  show e @ (Bin Le e1 e2) = (paren e1) ++ " <= " ++ (paren e2)
  show e @ (Bin Ge e1 e2) = (paren e1) ++ " >= " ++ (paren e2)
  show e @ (Bin Eq e1 e2) = (paren e1) ++ " == " ++ (paren e2)
  show e @ (Bin SEq e1 e2) = (paren e1) ++ " === " ++ (paren e2)
  show e @ (Bin Ne e1 e2) = (paren e1) ++ " != " ++ (paren e2)
  show e @ (Bin SNe e1 e2) = (paren e1) ++ " !== " ++ (paren e2)
  show (Tern c t e) = (paren c) ++ " ? " ++ (paren t) ++ " : " ++ (paren e)
  show (Un Minus e) = "-" ++ (paren e)
  show (Un Not e) = "!" ++ (paren e)
  show (Fun x e) = "function (" ++ x ++ ") { return " ++ (show e) ++ "; }"
  show (App e1 e2) = (paren e1) ++ "(" ++ (paren e2) ++ ")"
  show (Var x) = x

paren :: Exp -> String
paren e @ (Bin _ _ _) = "(" ++ show e ++ ")"
paren e @ (Fun _ _) = "(" ++ show e ++ ")"
paren e @ (App _ _) = "(" ++ show e ++ ")"
paren e @ _           = show e

-- Utility functions used during evaluation
toNumber :: Exp -> Double
toNumber (B False) = 0
toNumber (B True) = 1
toNumber (K n) = n
toNumber _ = nan

toBool :: Exp -> Bool
toBool (K n) | n == 0       = False
             | isNaN n      = False
             | isInfinite n = True
             | otherwise    = True
toBool (B b) = b
toBool (Fun _ _) = True
toBool e = False

nan :: Double
nan = 0/0

inf :: Double
inf = 1/0

minf :: Double
minf = (-1)/0
