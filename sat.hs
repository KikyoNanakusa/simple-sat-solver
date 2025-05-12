import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Operator(..), Assoc(..))
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M

-- 論理式
data Prop
    = Var String    -- 変数記号
    | Not Prop      -- 否定
    | And Prop Prop -- 論理積
    | Or Prop Prop  -- 論理和
    | Imp Prop Prop -- 含意
    deriving (Eq,Show)

parseProp :: String -> Either ParseError Prop
parseProp = parse expr ""

-- パーサを定義
expr :: Parser Prop
expr = buildExpressionParser table term

table =
  [ [Prefix (Not <$ string "not")]
  , [Infix  (And <$ string "and") AssocLeft]
  , [Infix  (Or  <$ string "or") AssocLeft]
  , [Infix  (Imp <$ string "->") AssocRight]
  ]

-- 項(最小構成要素)
term :: Parser Prop
term =  parens expr
    <|> Var <$> many1 letter

parens  = between (char '(') (char ')') 

-- 論理式内の変数記号を列挙する
vars :: Prop -> [String]
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Imp p q) = vars p ++ vars q

-- 割り当て(全ての変数記号と真偽値の組の冪集合)を返す
allAssignments :: [String] -> [Map String Bool]
allAssignments xs = 
    let xs' = nub xs 
        bools = sequence [ [(v, True), (v, False)] | v <- xs' ]
    in map M.fromList bools

eval :: Prop -> Map String Bool -> Bool
eval (Var x) m = M.findWithDefault False x m
eval (Not p) m = not (eval p m)
eval (And p q) m = eval p m && eval q m 
eval (Or p q) m = eval p m || eval q m 
eval (Imp p q) m = not (eval p m) || eval q m

isSatisfiable :: Prop ->  Bool
isSatisfiable p = 
    let assignments = allAssignments (vars p)
    in any (eval p) assignments

main :: IO ()
main = do
    putStrLn "Enter a propositional logic formula:"
    formula <- getLine
    case parseProp formula of
        Left err -> putStrLn $ "Error: " ++ show err
        Right p -> putStrLn $ if isSatisfiable p then "Satisfiable" else "Unsatisfiable"
