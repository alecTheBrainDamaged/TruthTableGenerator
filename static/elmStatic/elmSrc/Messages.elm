module Messages exposing (..)

import Http


type Msg = SendArgHttpPost
         | GotJson (Result Http.Error HaskellServerResponse)
         | UpdateArgument String
         | LightSwitch Bool
         | LogicButton BooleanFunction
         | DisplayMat Bool

type BooleanFunction = IfThen
                     | Iff
                     | Not
                     | And
                     | Or
                     | Xor
                     | Therefore
                     | Nand
                     | Nor
                     | If
                     | Top -- nullary functions
                     | Bottom  -- nullary functions

type alias HaskellServerResponse =
      { err : String
      , headers : List String
      , assignments : List (String, List Bool)
      , premiseEval : List (List (String,Bool))
      , conclusionEval : List (String, Bool)
      , validityy : String
      }
