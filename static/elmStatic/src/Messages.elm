module Messages exposing (..)

import Http


type Msg = SendArgHttpPost
         | GotJson (Result Http.Error HaskellServerResponse)
         | UpdateArgument String
         | LightSwitch Bool
         | LogicButton BooleanFunction
         | DisplayMat Bool

type BooleanFunction = If
                     | Iff
                     | Not
                     | And
                     | Or
                     | Xor
                     | Therefore
                     | Nand
                     | Nor

type alias HaskellServerResponse =
      { err : String
      , headers : List String
      , assignments : List (String, List Bool)
      , premiseEval : List (List (String,Bool))
      , conclusionEval : List (String, Bool)
      , validityy : String
      }
