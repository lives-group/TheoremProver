module Term where

type Var = String

-- | Term :+: Term
-- show (t1 :+: t2) = "( " ++ (show t1) ++ " | " ++ (show t2) ++ " )"
data Term = V Var
          | Term :@: Term
          | Term ::: Tipo
          | Term :*: Term
          | Lam Var Term
          | LamT Var Tipo Term
          | Fst Term
          | Snd Term
          | TLeft Term  --Coprod Esq
          | TRight Term --CoProd Dir
          | TEither Term Term Term
      deriving Eq

instance Show Term where
  show (V v) = v
  show (t1 :@: t2@(_ :@: _)) = (show t1) ++ " ( " ++ (show t2) ++ " )"
  show (t1 :@: t2) = (show t1) ++ " " ++ (show t2)
  show (te ::: ti) = (show te) ++ " : " ++ (show ti)
  show (t1 :*: t2) = "( " ++ (show t1) ++ " , " ++ (show t2) ++ " )"
  show (LamT v ti te) = "\\(" ++ v ++ "::" ++ (show ti) ++ "). {" ++ (show te) ++ "}"
  show (Lam v te) = "\\(" ++ v ++ "). {" ++ (show te) ++ "}"
  show (Fst t) = "FST" ++ (show t)
  show (Snd t) = "SND (" ++ (show t) ++ ")"
  show (TLeft t) = "Left" ++ (show t)
  show (TRight t) = "Right (" ++ (show t) ++ ")"


data Tipo = T Var
          | Tipo :>: Tipo
          | Tipo :&: Tipo
          | Tipo :|: Tipo
          | TFalse
        deriving (Eq)

instance Show Tipo where
  show (T v) = v
  show (t1@(_ :>: _) :>: t2) = "( " ++ (show t1) ++ " ) -> " ++ (show t2)
  show (t1 :>: t2) = (show t1) ++ " -> " ++ (show t2)
  show (t1 :&: t2) = "( " ++ (show t1) ++ " , " ++ (show t2) ++ " )"
  show (t1 :|: t2) = "{ " ++ (show t1) ++ " | " ++ (show t2) ++ " }"
  show TFalse = "False"
