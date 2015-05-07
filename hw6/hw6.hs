--- Homework 6 DSL
--- This homework is worked out by the coopeartion of Zhenfei Nie and Xiyu Lyu, who worked together.

---
--- Xiyu Lyu did the `C AST` and `Translate`
--- ZHenfei did the  `Optimize` and `Pretty-printer`
 
module Main where
import Text.Parsec hiding (State)
import Text.Parsec.String
import Text.Parsec.Expr
import Control.Applicative((<$>), (<*>), (<*))
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe (fromJust)
import System.Console.Haskeline
import System.Environment (getArgs)
import Debug.Trace (trace)
import Data.List


-- printf debugging
debug :: Monad m => String -> m ()
debug s = trace s (return ())

----------------------------------------------------------------
-- Evaluator
----------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  if null args then
    putStrLn "Usage: hw6 input.txt \n\n[NOTE]: Please do NOT add semicolon at the last expression!\n\noptional parameters: \n  -optimize:\tconstant folding"
  else if (length args) == 2 then do
    if ( args !! 0 ) == "-optimize" then do
      putStrLn "//runing with optimization"
      input <- readFile ( args !! 1 )
      case translate2C input of
        Just ces -> do
          parseCExpAndPrint ces True
          return ()
        Nothing -> do
          putStrLn "Error: translation failed"
          return ()
    else do
      putStrLn "Error: undefined parameter"
      return ()
  else do
    input <- readFile $ head args
    case translate2C input of
      Just ces -> do
        parseCExpAndPrint ces False
        return ()
      Nothing -> do
        putStrLn "Error: cannot read file"
        return ()

parseCExpAndPrint :: CExp -> Bool -> IO ()
parseCExpAndPrint ces opt = do
  putStrLn $ printHeader 
  putStrLn $ declearVars $ runState ( checkVaribles $ addAutoVar ces ) []
  if opt == True then putStrLn $ show $ tryOptimize $ addAutoVar ces 
                 else putStrLn $ show $ addAutoVar ces 
  putStrLn $ dispPrint $ runState ( checkVaribles $ addAutoVar ces ) []
  putStrLn $ printEnd   
  return ()    

translate2C :: String -> Maybe CExp
translate2C input = case parse expr "input" input of
  Right es -> Just (translate es (CSeq []) )
  Left err -> Nothing



----------------------------------------------------------------
-- C AST
----------------------------------------------------------------
data COp = CAdd | CSub | CMul | CDiv 
data CExp = CK Integer
         | CBin COp CExp CExp
         | CVar String
         | CAssign String CExp
         | CSeq [CExp]

instance Eq COp where
  CAdd == CAdd = True
  CSub == CSub = True
  CMul == CMul = True
  CDiv == CDiv = True
  _ == _ = False

instance Eq CExp where
  CK x == CK y = (x == y)
  CBin xop xce1 xce2 == CBin yop yce1 yce2 
        = (xop == yop) && (xce1 == yce1) && (xce2 == yce2)
  CVar xstr == CVar ystr 
        = (xstr == ystr)
  CAssign xstr xce == CAssign ystr yce 
        = (xstr == ystr) && (xce == yce)
  CSeq (xe:xes) == CSeq (ye:yes) 
        = (xe == ye) && (xes == yes)
  _ == _ = False


----------------------------------------------------------------
-- Translation: from Exp to CExp
----------------------------------------------------------------
translate :: Exp -> CExp -> CExp
translate (Seq []) (CSeq cs)  = CSeq cs
translate (Seq (e:es)) (CSeq cs) = translate (Seq es) (CSeq (cs ++ [transExp e]))

transExp :: Exp -> CExp
transExp (K n) = (CK n)
transExp (Bin op e1 e2) = CBin (transOp op) (transExp e1) (transExp e2) 
transExp (Var n) = (CVar n)
transExp (Assign str e) = CAssign str (transExp e)
transExp (Seq es) = translate (Seq es) (CSeq [])

transOp :: Op -> COp
transOp Add = CAdd
transOp Sub = CSub
transOp Mul = CMul
transOp Div = CDiv

----------------------------------------------------------------
-- Check Varibles 
----------------------------------------------------------------
type CStore = [(String, CExp)]

checkVaribles :: CExp -> State CStore CExp
checkVaribles (CSeq []) = return (CK 0)
checkVaribles (CSeq [e]) = checkVaribles e
checkVaribles (CSeq (e:es)) = do
  v <- checkVaribles e
  checkVaribles (CSeq es)

checkVaribles (CVar x) = do
  s <- get
  case lookup x s of
    Just v -> return v
    Nothing -> fail $ "Variable '" ++ x ++ "' not found"

checkVaribles (CAssign x e) = do
  v <- checkVaribles e
  s <- get
  put $ (x,v):s
  return v

checkVaribles (CK n) = return (CK n)

checkVaribles (CBin op x y) = do
  CK x' <- checkVaribles x
  CK y' <- checkVaribles y
  return (CK (theOp op x' y'))
  where
    theOp CAdd = (+)
    theOp CSub = (-)
    theOp CMul = (*)
    theOp CDiv = div

----------------------------------------------------------------
-- Optimization : constant folding
----------------------------------------------------------------

tryOptimize :: CExp -> CExp
tryOptimize old = let new = optimize old (CSeq []) in case (old == new ) of 
                    True -> old 
                    otherwise -> tryOptimize new

optimize :: CExp -> CExp -> CExp 
optimize (CSeq []) (CSeq new) = CSeq new
optimize (CSeq (e:es)) (CSeq new) = optimize (CSeq es) (CSeq (new ++ [optimizeExp e]))

optimizeExp :: CExp -> CExp
optimizeExp (CK n) = (CK n)
optimizeExp (CBin op e1 e2) = case e1 of 
                                CK n -> case e2 of 
                                    CK m -> folding (CBin op e1 e2)
                                    otherwise -> CBin op (optimizeExp e1) (optimizeExp e2) 
                                otherwise -> CBin op (optimizeExp e1) (optimizeExp e2) 
optimizeExp (CVar n) = (CVar n)
optimizeExp (CAssign str e) = CAssign str (optimizeExp e)
optimizeExp (CSeq es) = optimize (CSeq es) (CSeq [])

folding :: CExp -> CExp
folding (CBin op (CK x) (CK y)) = (CK (theOp op x y))
  where
    theOp CAdd = (+)
    theOp CSub = (-)
    theOp CMul = (*)
    theOp CDiv = div

----------------------------------------------------------------
-- Pretty Printer for C-AST
----------------------------------------------------------------
instance Show CExp where
  show (CK n) = show n
  show (CBin CAdd e1 e2) = (cparen e1) ++ " + " ++ (cparen e2) 
  show (CBin CSub e1 e2) = (cparen e1) ++ " - " ++ (cparen e2)
  show (CBin CMul e1 e2) = (cparen e1) ++ " * " ++ (cparen e2)
  show (CBin CDiv e1 e2) = (cparen e1) ++ " / " ++ (cparen e2)
  show (CSeq []) = ";"
  show (CSeq [e]) = show e
  show (CSeq (e:es)) = indentation ++ (foldl (\l r -> l ++ ";\n" ++ indentation ++ show r) (show e) es) ++ ";"
  show (CVar x) = x
  show (CAssign x e) = x ++ " = " ++ show e

cparen :: CExp -> String
cparen e @ (CBin _ _ _) = "(" ++ show e ++ ")"
cparen e @ (CK _ ) = show e 
cparen e @ (CVar _ ) = show e 
cparen e @ _           = "(" ++ show e ++ ")"

printHeader :: String
printHeader = "#include <stdio.h>\n\nint main() {\n"

printEnd :: String
printEnd = "\n}\n"

indentation :: String
indentation = "    "

-- declear varibles in C
declearVars :: (CExp, CStore) -> String
declearVars (x, s) = indentation ++ "int " ++ toString getVariblesList ++ ";"
  where 
    rmdups = map head . group . sort
    getVariblesList = rmdups $ fmap fst s
    toString x = if length x == 0 then ""
                                  else if length x > 1 then head x ++ ", " ++ toString (tail x) else head x

-- printf expression in C
dispPrint :: (CExp, CStore) -> String
dispPrint ((CK x), s) = indentation ++ "printf(\"%d\\n\",  " ++ (fst $ head s) ++ ");"

-- Add auto varibles 
addAutoVar :: CExp -> CExp
addAutoVar (CSeq ces) = assignVaribles ces 0 (CSeq [])

assignVaribles :: [CExp] -> Int -> CExp -> CExp
assignVaribles [] i new = new
assignVaribles (e:es) i (CSeq nl) = case e of
          (CAssign name a) -> let new = CSeq (nl ++ [e]) in assignVaribles es i new
          (CBin op e1 e2) -> let g = CAssign ("auto_v" ++ show i) e; new = CSeq (nl ++ [g]) in assignVaribles es (i+1) new
          (CVar str) -> let g = CAssign ("auto_v" ++ show i) e; new = CSeq (nl ++ [g]) in assignVaribles es (i+1) new
          (CK x) -> let g = CAssign ("auto_v" ++ show i) e; new = CSeq (nl ++ [g]) in assignVaribles es (i+1) new





----------------------------------------------------------------------------------------
-- ** Expr **
----------------------------------------------------------------------------------------

-- trees
data Op = Add | Sub | Mul | Div 
data Exp = K Integer
         | Bin Op Exp Exp
         | Var String
         | Assign String Exp
         | Seq [Exp]

-- parser
expr :: Parser Exp
expr = do
  es <- exprs
  return $ Seq es

exprs :: Parser [Exp]
exprs = sepBy1 expr1 (string ";" <* spaces)

expr1 :: Parser Exp
expr1 = try (do { 
                  x <- ident; 
                  spaces; 
                  string "="; 
                  spaces; 
                  e <- binary; 
                  spaces; 
                  return $ Assign x e 
                }) 
        <|> binary <* spaces

binary :: Parser Exp
binary = buildExpressionParser table factor <?> "expression" where
  table = [
    [Infix (do { char '*'; return $ Bin Mul }) AssocLeft,
     Infix (do { char '/'; return $ Bin Div }) AssocLeft],
    [Infix (do { char '+'; return $ Bin Add }) AssocLeft,
     Infix (do { char '-'; return $ Bin Sub }) AssocLeft]]

factor :: Parser Exp
factor = (do
            spaces
            f <- do { char '('; spaces; x <- expr; char ')'; return x } <|> try number <|> var
            spaces
            return f)
        <?> "simple expression"

sign :: Parser Char
sign = char '+' <|> char '-'

number :: Parser Exp
number = (K . read) <$> (do { s <- option '+' sign;
                              ds <- many1 digit;
                              return $ makeNumber s ds })
  where
    makeNumber '+' ds = ds
    makeNumber '-' ds = "-" ++ ds

ident :: Parser String
ident = (do { c <- letter; s <- many digit; return $ c:s })

var :: Parser Exp
var = Var <$> ident

 --A simple pretty-printer for Exps
instance Show Exp where
  show (K n) = show n
  show (Bin Add e1 e2) = (paren e1) ++ " + " ++ (paren e2)
  show (Bin Sub e1 e2) = (paren e1) ++ " - " ++ (paren e2)
  show (Bin Mul e1 e2) = (paren e1) ++ " * " ++ (paren e2)
  show (Bin Div e1 e2) = (paren e1) ++ " / " ++ (paren e2)
  show (Seq []) = ";"
  show (Seq [e]) = show e
  show (Seq (e:es)) = foldl (\l r -> l ++ "; " ++ show r) (show e) es
  show (Var x) = x
  show (Assign x e) = x ++ " = " ++ show e

paren :: Exp -> String
paren e @ (Bin _ _ _) = "(" ++ show e ++ ")"
paren e @ (K _ ) = show e 
paren e @ (Var _ ) = show e 
paren e @ _           = "(" ++ show e ++ ")"


-- print of Exp
parseAndPrint :: String -> IO ()
parseAndPrint input = case parse expr "input" input of
  Right es -> do
    putStrLn $ show es
    return ()
  Left err -> do
    putStrLn $ show err
    return ()











