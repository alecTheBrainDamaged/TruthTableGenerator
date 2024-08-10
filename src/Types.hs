{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

data Proposition
   = Var Char
   | Not Proposition
   | And Proposition Proposition
   | Or Proposition Proposition
   | Xor Proposition Proposition
   | If    Proposition Proposition
   | Iff   Proposition Proposition
   | Nand  Proposition Proposition
   | Nor   Proposition Proposition
   deriving (Eq, Ord)

bracket :: String -> String
bracket [] = []
bracket [x] = [x]
bracket s = "(" ++ s ++ ")"

instance Show Proposition where
   show (Var c) = [c]
   show (Not p) = "\x00AC" ++ (show p)
   show (And p p') = (bracket  $ show p ) ++ " " ++ "\x2227" ++ " " ++ (bracket $ show p')
   show (Nand p p') = (bracket  $ show p ) ++ " " ++ "\x22BC" ++ " " ++ (bracket $ show p')
   show (Or p p')  = (bracket $ show p) ++ " " ++ "\x2228" ++ " " ++ (bracket $ show p')
   show (Nor p p')  = (bracket $ show p) ++ " " ++ "\x22BD" ++ " " ++ (bracket $ show p')
   show (Xor p p') = (bracket $ show p) ++ " " ++ "\x22BD" ++ " " ++ (bracket $ show p')
   show (If p p')  = (bracket $ show p) ++ " " ++ "\x2192" ++ " " ++ (bracket $ show p')
   show (Iff p p')  = (bracket $ show p) ++ " " ++ "\x2194" ++ " " ++ (bracket $ show p')

type Premises = [Proposition]
type Conclusion = Proposition
type Conclusoins = [Conclusion]

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




data MastubatoryFactors = AccessToPornography Bool
                        | AccessToSexToys Bool
                        | DaysSinceLastEjaculation Int
                        | AverageEjaculationPerWeek Float
                        | HealthyHormones Bool
                        | TestosteroneLevels Float
                        | RelationShipStatus String
                        | BeliefsAboutMasturbation String
                        | LivingSituation String
                        | LocationTheyWillSleepTonight String
                        | Mood
                        deriving (Show, Eq)
data Mood = Sad
          | Happy
          | Angry
          | Horny
          | Hungry
          deriving (Show, Eq)

