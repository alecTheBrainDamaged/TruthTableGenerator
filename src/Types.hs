{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

data Proposition
   = Var Char
   | Top
   | Bottom
   | Not Proposition
   | And Proposition Proposition
   | Or Proposition Proposition
   | Xor Proposition Proposition
   | IfThen    Proposition Proposition
   | Iff   Proposition Proposition
   | Nand  Proposition Proposition
   | Nor   Proposition Proposition
   | If    Proposition Proposition
   deriving (Eq, Ord)

bracket :: String -> String
bracket [] = []
bracket [x] = [x]
bracket s = "(" ++ s ++ ")"

instance Show Proposition where
   show (Var c) = [c]
   show Top     = "\x22A4"
   show Bottom = "\x22A5"
   show (Not p) = "\x00AC" ++ (show p)
   show (And p p') = (bracket  $ show p ) ++ " " ++ "\x2227" ++ " " ++ (bracket $ show p')
   show (Nand p p') = (bracket  $ show p ) ++ " " ++ "\x22BC" ++ " " ++ (bracket $ show p')
   show (Or p p')  = (bracket $ show p) ++ " " ++ "\x2228" ++ " " ++ (bracket $ show p')
   show (Nor p p')  = (bracket $ show p) ++ " " ++ "\x22BD" ++ " " ++ (bracket $ show p')
   show (Xor p p') = (bracket $ show p) ++ " " ++ "\x22BD" ++ " " ++ (bracket $ show p')
   show (IfThen p p')  = (bracket $ show p) ++ " " ++ "\x2192" ++ " " ++ (bracket $ show p')
   show (Iff p p')  = (bracket $ show p) ++ " " ++ "\x2194" ++ " " ++ (bracket $ show p')
   show (If p p')  = (bracket $ show p) ++ " " ++ "\x2190" ++ " " ++ (bracket $ show p')

type Premises = [Proposition]
type Conclusion = Proposition

type Argument = (Premises, Conclusion)


data Validity = Valid | Invalid | VacouslyValid deriving (Eq , Show)

data TruthTable = TruthTable
                { eap :: Either Argument Premises
                , variables :: String
                , assignments :: [(String, [Bool])]
                , everyPremiseEval :: [[(Proposition, Maybe Bool)]]
                , everyConclusionEval :: Maybe [(Conclusion, Maybe Bool)]
                , validity :: Maybe Validity
                }
                deriving (Eq, Show)








---------------------------------------------





