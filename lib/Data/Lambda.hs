module Data.Lambda where

import Data.List
import Text.Parser.Char
import Text.Trifecta.Parser
import Control.Applicative
import Data.Functor
import Text.Trifecta

import qualified Debug.Trace as T

data Term = Var String | Lambda String Term | Ap Term Term
  deriving (Eq, Show)

lambda :: String -> Term -> Term
lambda = Lambda
var :: String -> Term
var = Var
ap :: Term -> Term -> Term
ap = Ap

type Stack = [(String, Term)]

findInStack :: String -> Stack -> Maybe Term
findInStack n s = snd <$> find (\d -> fst d == n) s

eval :: Int -> Stack -> Term -> (Stack, Term)
eval 0 s t = (s, t)
eval d s (Var n) = case findInStack n s of
  Nothing -> (s, Var n)
  Just t -> eval (d-1) s $ T.traceShowId t
eval d s (Lambda n t) =
  let (s', t') = eval (d-1) s t
  in (s', Lambda n $ T.traceShowId t')
eval d s (Ap (Lambda a t) b) = eval (d-1) s $ T.traceShowId $ beta b a t
eval d s (Ap (Ap x y) b) =
  let (_, t') = eval (d-1) s $ Ap x y
  in eval (d-1) s $ T.traceShowId $ Ap t' b
eval d s (Ap (Var a) b) =
  let (_, t) = eval (d-1) s $ T.traceShowId $ Var a
  in if t == Var a
  then
    let (_, b') = eval (d-1) s b
    in (s, T.traceShowId $ Ap t b')
  else eval (d-1) s $ T.traceShowId $ Ap t b

eval' :: Term -> (Stack, Term)
eval' = eval 100000 emptyScope

-- (\x.t) s -> t[x:=s]
beta :: Term -> String -> Term -> Term
beta s x (Var a) = if a == x then s else Var a
beta s x (Lambda a t) = if a == x then Lambda a t else Lambda a $ beta s x t
beta s x (Ap a b) = Ap (beta s x a) (beta s x b)

emptyScope :: Stack
emptyScope =
  [ ("succ",  Lambda "n" $ Lambda "z" $ Lambda "s" $ Ap (Var "s") (Var "n"))
  , ("zero", Lambda "z" $ Lambda "s" $ Var "z")
  , ("one", Ap (Var "succ") (Var "zero"))
  , ("two", Ap (Var "succ") (Var "one"))
  , ("Y", Lambda "f" $ Ap
      (Lambda "x" $ Ap (Var "f") (Ap (Var "x") (Var "x")))
      (Lambda "x" $ Ap (Var "f") (Ap (Var "x") (Var "x"))))
  , ("true", Lambda "f" $ Lambda "t" $ Var "t")
  , ("false", Lambda "f" $ Lambda "t" $ Var "f")
  , ("const", lambda "x" $ lambda "y" $ var "x")
  , ("id", lambda "x" $ var "x")
  , ("swap", lambda "f" $ lambda "x" $ lambda "y" $ ap (ap (var "f") (var "y")) (var "x"))
  , ("add",
      Ap (Var "Y") $
      Lambda "r" $
      Lambda "p" $
      Lambda "q" $
      Ap
        -- Handle the zero case
        (Ap (Var "p") (Var "q"))
        (Lambda "m" $ Ap (Var "succ") (Ap (Ap (Var "r") (Var "m")) (Var "q"))))
  , ("W", lambda "x" $ ap (var "x") (var "x"))
  , ("W3", lambda "x" $ ap (ap (var "x") (var "x")) (var "x"))
  ]


block :: Parser Term -> Parser Term
block p = do
  m <- optional $ char '('
  t <- p
  case m of
    Nothing -> pure ()
    Just _ -> void $ char ')'
  pure t

parse :: Parser Term
parse =
  parseVar <|>
  try (block parseLambda) <|>
  parseAp

parseVarRaw :: Parser String
parseVarRaw = some alphaNum

parseVar :: Parser Term
parseVar = var <$> parseVarRaw

parseLambda :: Parser Term
parseLambda = lambda 
  <$> (char '\\' *> parseVarRaw)
  <*> (char '.' *> parse)

parseAp :: Parser Term
parseAp = between (char '(') (char ')') $ do
  ap <$> parse <* char ' ' <*> parse


