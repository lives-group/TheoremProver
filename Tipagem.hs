module Tipagem (curryTerm,termCont)where

import Term
import Control.Monad.Except
import Types

curryTerm :: Contexto -> Term -> Result Tipo
curryTerm c (V v) = case lookup v c of
                        Just t -> return t
                        Nothing -> throwError $ (show v) ++ " não esta no contexto"
curryTerm c (t1 :@: t2) = case curryTerm c t1 of
                            Right (p1 :>: p2) -> do l <- curryTerm c t2
                                                    un (p1 == l) p2 "Tipo errado"
                            _ -> throwError "TIpo errado"
curryTerm c (LamT v t te) = do a <- curryTerm ((v,t):c) te
                               return (t :>: a)
curryTerm c (TLeft te ::: t@(a :|: b)) = do ti <- curryTerm c te
                                            case (a == ti) of
                                               True -> return t
                                               False -> throwError "Tipo errado"
curryTerm c (TRight te ::: t@(a :|: b)) = do ti <- curryTerm c te
                                             case (b == ti) of
                                                True -> return t
                                                False -> throwError "Tipo errado"
curryTerm c (te ::: ti) = do a <- curryTerm c te
                             case (a == ti) of
                               True -> return ti
                               False -> throwError "Tipo errado"
curryTerm c (t1 :*: t2) = do a <- curryTerm c t1
                             b <- curryTerm c t2
                             return (a :&: b)
curryTerm c (Fst t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p1
                            _ -> throwError "TIpo errado"
curryTerm c (Snd t) = case curryTerm c t of
                            Right (p1 :&: p2) -> return p2
                            _ -> throwError "TIpo errado"
curryTerm c (TEither t@(te1 ::: (a :|: b)) te2 te3) = case curryTerm c t of
                                                        Right _ -> case curryTerm c te2 of
                                                                      Right t1@(x :>: y) -> case curryTerm c te3 of
                                                                                    Right t2@(d :>: c) -> case (lastT t1 == (lastT t2)) && (a == x) && (b == d)  of
                                                                                                  True -> return $ lastT t1
                                                                                                  False -> throwError "Tipo incorreto"
                                                                                    Left _ -> throwError "Tipo errado"
                                                                      Left _ -> throwError "tipo Errado"
                                                        Left _ -> throwError "Tipo incorreto"
curryTerm c (NaughtElim t ti) = case curryTerm c t of
                                  Right TFalse -> return ti
                                  _ -> throwError "Error"

lastT :: Tipo -> Tipo
lastT (t1 :>: t2) = lastT t2
lastT t1 = t1

un :: Bool -> Tipo -> String -> Result Tipo
un b t s
  | b = return t
  | otherwise = throwError s

termCont :: Contexto -> Term -> Contexto
termCont c a@((V v) ::: t) = (v,t):c
termCont c (a@(V _)) = c
termCont c (Snd a) = termCont c a
termCont c (Fst a) = termCont c a
termCont c (a ::: t) = termCont c a
termCont c (t1 :@: t2) = termCont x t2
  where
     x = termCont c t1
termCont c (t1 :*: t2) = termCont x t2
  where
     x = termCont c t1
termCont c (LamT v s t) = termCont c t
