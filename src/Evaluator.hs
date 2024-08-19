{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Evaluator where

import Types
import Data.List (nub, concatMap, findIndex, elemIndex)
import Control.Monad (replicateM)
import Data.Maybe (fromJust)

top :: Char
top = '\x22A4'
bottom :: Char
bottom = '\x22A5'

generateTruthTable:: (Either Argument Premises) -> TruthTable
generateTruthTable e =
    case e  of
     (Right  prem) -> let vars = getVarsInArg e

                          booleanCombos = getBooleanCombos vars

                          assignments = getAssignments vars booleanCombos

                          everyPremiseEval' = map (map (\(p,_,e) -> (p,e) )) [getEveryEval prem assignment | assignment <- assignments]



                          truthTableData = TruthTable
                                         { eap = e
                                         , variables = vars
                                         , assignments = assignments
                                         , everyPremiseEval = everyPremiseEval'
                                         , everyConclusionEval = Nothing
                                         , validity = Nothing
                                         }
                      in truthTableData
     (Left t)  ->  let vars = getVarsInArg e

                       booleanCombos = getBooleanCombos vars

                       assignments = getAssignments vars booleanCombos
                       p = fst t
                       c = snd t

                       everyPremiseEval' = [getEveryEval p assignment | assignment <- assignments]

                       everyConclusionEval' = [(c, getEval c assignment) | assignment <- assignments]

                       onlyTruePremises = filter (\list -> all (\(_ , _ ,maybeb) -> maybeb == (Just True) ) list ) everyPremiseEval'

                       validity = case onlyTruePremises of
                                   [] -> VacouslyValid
                                   _  -> evalValidity onlyTruePremises c

                       truthTableData = TruthTable
                                      { eap = e
                                      , variables = vars
                                      , assignments = assignments
                                      , everyPremiseEval =  map (map (\(p',_,e) -> (p',e))) everyPremiseEval'
                                      , everyConclusionEval = Just everyConclusionEval'
                                      , validity = Just validity
                                      }
                   in truthTableData
     where
      getVarsInArg :: Either Argument Premises -> String
      getVarsInArg (Right premises) = nub $ foldr (\p acc -> (getVars p) ++ acc) [] premises
        where
          getVars :: Proposition -> String
          getVars (Var c) = [c]
          getVars Top     = [top]
          getVars Bottom  = [bottom]
          getVars (Not p) = getVars p
          getVars (And prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Or prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (IfThen prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Iff prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Xor prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Nor prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Nand prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (If   prop1 prop2) = (getVars prop1) ++ (getVars prop2)
      getVarsInArg (Left ((premises , conclusion))) = nub $ foldr (\p acc -> (getVars p) ++ acc) (getVars conclusion) premises
        where
          getVars :: Proposition -> String
          getVars (Var c) = [c]
          getVars (Not p) = getVars p
          getVars Top     = [top]
          getVars Bottom  = [bottom]
          getVars (And prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Or prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Xor prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (IfThen prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Iff prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Nor prop1 prop2) = (getVars prop1) ++ (getVars prop2)
          getVars (Nand prop1 prop2)  = (getVars prop1) ++ (getVars prop2)
          getVars (If   prop1 prop2) = (getVars prop1) ++ (getVars prop2)


      getBooleanCombos :: String -> [[Bool]]
      getBooleanCombos s =
          case s of
           [x] -> case (x == top, x == bottom) of
                   (True, _) -> return [True]
                   (_   , True)     -> return [False]
                   _                ->  do
                                         true      <- return [True]
                                         false     <- return [False]
                                         list      <- [true,false]
                                         return list

           _        -> do
                        functions <-  replicateM (length s) [f, g]
                        charAndFs <-  map (zip s) (return functions)
                        answer    <-  map (\l -> map (\(arg, f) -> f arg) l ) (return charAndFs)
                        return answer
                           where f :: Char -> Bool
                                 f c = case (c == top, c == bottom) of
                                        (True, _ )   -> True
                                        (_   , True) -> False
                                        _            -> True
                                 g :: Char -> Bool
                                 g c = case (c == top, c == bottom) of
                                        (True, _ )      -> True
                                        (_   , True)    -> False
                                        _               -> False


      getAssignments :: String -> [[Bool]] -> [(String, [Bool])]
      getAssignments s bools =  [ (s, combo) | combo <- bools ]

      getEveryEval :: Premises -> (String, [Bool]) -> [(Proposition, (String, [Bool]) ,Maybe Bool)]
      getEveryEval premises tuple =
                  case premises of
                      [] -> []
                      (p : ps) -> (p , tuple, getEval p tuple) : (getEveryEval ps tuple)

      evalValidity :: [[(Proposition, (String, [Bool]) , Maybe Bool)]]
                   -> Conclusion
                   -> Validity
      evalValidity allTruePremises conclusion =
        case allTruePremises of
                [] -> Valid
                (l : ls) ->  case getEval conclusion (fromJust $ takeAssignment $ l) of
                              (Just False) -> Invalid
                              (Just True)  -> evalValidity ls conclusion
takeAssignment :: [(a , (String,[Bool]), c)] -> Maybe (String, [Bool])
takeAssignment [] = Nothing
takeAssignment ((_ , assignment  , _ ) : _ ) = Just assignment

getEval :: Proposition -> (String, [Bool]) -> Maybe Bool
getEval (Var c) (vars, bools) =  lookup c ((vars `zip` bools))
getEval (Not p) tuple =  do
             b <- getEval p tuple
             return $ not b
getEval (And p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ b && b'
getEval (Or p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ b || b'
getEval (Xor p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ b `xor` b'
             where xor True True = False
                   xor False False = False
                   xor _     _     = True
getEval (Nand p1 p2) tuple = do
        b <- (getEval p1 tuple)
        b' <- (getEval p2 tuple)
        return $ not (b && b')
getEval (Nor p1 p2) tuple = do
        b <- (getEval p1 tuple)
        b' <- (getEval p2 tuple)
        return $ not (b || b')
getEval (IfThen p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ b `ifthen'` b'
   where ifthen' True False = False
         ifthen' _ _ = True
getEval (Iff p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ b == b'
getEval (If p1 p2) tuple = do
             b <- (getEval p1 tuple)
             b' <- (getEval p2 tuple)
             return $ case b' of
                       True -> case b of
                                False -> False
                                _     -> True
                       _    -> True
getEval Top _ = return True
getEval Bottom _ = return False


