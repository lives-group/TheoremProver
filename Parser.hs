module Parser(runt, deepInst, varInst) where

import Term
import Data.Functor.Identity
import Text.Parsec.String
import Text.Parsec hiding (State)
import Text.Parsec.Expr
import Types

zero :: State
zero = (0, [], TFalse,0, [],0)

prel :: Parser a -> Parser a
prel p = spaces >> p

posl :: Parser a -> Parser a
posl p =  p >>= (\r -> spaces >> return r)

parens :: Parser a -> Parser a
parens p = posl (char '(') >>
           (p >>= (\r -> posl (char ')') >> return r))

typeName :: Parser Tipo
typeName = many1 (alphaNum) >>= return.T

expr    = prel (buildExpressionParser table (posl term))
          <?> "Expressao Lógica"

term    = (parens expr <|> posl typeName)  <?> "Expressao Simples "

table   = [ [binary "&" (:&:) AssocLeft, binary "|" (:|:) AssocLeft ],
            [binary "->" (:>:) AssocRight  ]
          ]

binary :: String -> (Tipo -> Tipo -> Tipo) -> Assoc ->  Operator String () Identity Tipo
binary name fun assoc = Infix (do{ posl (string name); return fun }) assoc

newVar :: State -> (String,State)
newVar (sc,sq,obj,vc,ctx,sup) = ("x" ++ (show vc), (sc,sq,obj,vc+1,ctx,sup))

varInst :: State -> Tipo -> (Term,State)
varInst s t = let (v,s') = newVar s in ((V v) ::: t, s')

(>=>) :: (a,State) -> (State -> a -> (b,State)) -> (b,State)
(>=>) (x,s) f = f s x

(>->) :: (a,State) -> (State -> (b,State)) -> (b,State)
(>->) (x,s) f = f s

deepInst :: State -> Tipo -> (Term,State)
deepInst s t@(T v)       = varInst s t
deepInst s t@(t1 :|: t2) = varInst s t
deepInst s (t1 :>: t2)   = (deepInst s t2) >=> (\s' t' -> newVar s' >=> (\s'' var -> (LamT var t1 t',s'')  ))
deepInst s (t1 :&: t2)   = (deepInst s t1) >=> (\s' esq -> deepInst s' t2 >=> (\s'' dir -> ((esq :*: dir),s'')))

runt :: String -> Either ParseError Tipo
runt = runParser expr () ""
