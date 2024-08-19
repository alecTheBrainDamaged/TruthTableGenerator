{-

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Test.hs where

import Evaluator hiding (top, bottom)
import Data.List (elemAtIndex, findIndex)


top :: Char
top = '\x22A4'
bottom :: Char
bottom = '\x22A5'




TruthTable
{ eap :: Either Argument Premises
, variables :: String
, assignments :: [(String, [Bool])]
, everyPremiseEval :: [[(Proposition, Maybe Bool)]]
, everyConclusionEval :: Maybe [(Conclusion, Maybe Bool)]
, validity :: Maybe Validity
}
deriving (Eq, Show)



-- testProperty :: Testable a => TestName -> a -> TestTree
--property :: prop -> Property




tableHasTop :: TruthTable -> Bool
tableHasTop truthtable =
  let vars = variables truthtable
  in  case top `elem` vars of
       True -> True
       False -> False

properAssignmentsOfTop :: TruthTable -> Bool
properAssignmentsOfTop trutable
  | not $ tableHasTop truthtable = True
  | otherwise = let assignments' = assignments truthtable
                    topIndex     = findIndex topIndex (variables truthtable)
                in  do
                     combo <- assignme

testTable = testProperty "Propertable"


-}
