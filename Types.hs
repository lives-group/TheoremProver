module Types where

import Term

type Premissa = [Term]
type Objetivo = Tipo
type Provas = [Term]
type Contexto = [(String,Tipo)]
type Result a = Either String a
type SuposedVars = [(Tipo,String)]
type SeqCounter = Int
type VarCounter = Int
type SupCounter = Int
type State = (SeqCounter, Provas, Objetivo, VarCounter, Contexto,SupCounter)
