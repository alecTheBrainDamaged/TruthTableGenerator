port module Main exposing (..)

import Element as E exposing (..)
import Element.Border as B exposing (..)
import Element.Input as EI exposing (..)
import Element.Events exposing (onClick)
import Element.Background as Background exposing (..)
import Element.Font as Font exposing (..)
import Html exposing (..)
import Http 
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Browser
import Messages exposing (..)
import Colors exposing (..)
import LogicButtons as L exposing (keyboard)

main = Browser.element
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

type alias Model =
   { status : Status
   , argument : String
   , lightMode : LightMode
   , mat       : Bool
   }

type alias LightMode = Bool

type Status
   = Failure (Maybe String, Maybe Int)
   | Loading 
   | Success Messages.HaskellServerResponse
   | Default

init : () -> (Model , Cmd Msg)
init _ = 
   ( 
     {status = Default
     , argument = ""
     , lightMode = False
     , mat = False
     }
   , Cmd.none
   )

port copyToClipboard : String -> Cmd msg

-- JUST PICK DARK ONES AND LIGHT ONES EACH
{-
background colors
background pane Base
secondary pane crust | mantle
surface elements surface0 | surface1| surface2
overlays = overlay0 | overlay1| overlay2

-}


-- TYPOGRAPHY
{-
Main Headline = Text
Body Copy = Text
Sub-Headlines, Labels = Subtext0 | Subtext1
Links,URLs = Blue
Success = Green
Warnings = Yellow
Errors = Red
Tags, Pills = Blue
Selection Background = Overlay2 20%,-30% Opacity
Cursor = Rosewater
-}

-- Latte Section
-- LIGHT


myRowOfStuff : Model -> Element Msg
myRowOfStuff model =
    row [E.width fill, centerY, spacing 30, Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark)]
    [ E.el [E.alignRight] (myArgumentBoxElement model)
    , submitArgButton model 
    , lightModebox model
    ]

myArgumentBoxElement : Model -> Element Msg
myArgumentBoxElement model =
             E.el 
             [ Background.color (if model.lightMode then surfaceElementLight else secondaryPaneDark), B.rounded 3
             , padding 30
             , B.rounded 4
             ]
             (inputArgumentBox model) 
 

inputArgumentBox : Model -> Element Msg
inputArgumentBox model = 
              EI.multiline 
              [Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)
              ,Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
              , Font.family [Font.typeface "Regular"]
              ] 
              { onChange = UpdateArgument
              , text = model.argument
              , placeholder = Nothing
              , label = EI.labelAbove 
                        [Font.color (if model.lightMode then subHeadlinesLabelsLight else subHeadlinesLabelsLight)
                        , Font.family [Font.typeface "Bold"]
                        ] 
                        (E.text "Prop Box")
              , spellcheck = False
              }


submitArgButton : Model -> Element Msg
submitArgButton model = 
            E.el
            [Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)
            , Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
            , B.rounded 4
            , B.color secondaryPaneLight
            , padding 30
            ]
           (EI.button [pointer, Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)] 
            { onPress = Just SendArgHttpPost
            , label = E.el 
                      [Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
                      ,Font.family [Font.typeface "Regular"]
                      ] 
                      (E.text "Submit")
            }
           )

lightModebox : Model -> Element Msg
lightModebox model = 
             EI.checkbox
             [ Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)
             , pointer
             ]
             { onChange = LightSwitch
             , icon = \_ -> E.none
             , checked = model.lightMode
             , label = EI.labelBelow 
                       [ Font.color (if model.lightMode then subHeadlinesLabelsLight else subHeadlinesLabelsDark)
                       , Font.family [Font.typeface "Bold"]
                       ] 
                       (E.text "Toggle Light Mode" )
             } 



loadingMessage : Model -> Element Msg
loadingMessage model =
            E.el 
            [ Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)
            , Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
            , B.rounded 3
            ]
            (E.text "loading table ...") 

truthTableElement : Model -> Element Msg -> Element Msg
truthTableElement model m =
            E.el 
            [ Background.color (if model.lightMode then surfaceElementLight else surfaceElementDark)
            , Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
            , B.rounded 3
            , E.width E.fill
            , E.height E.fill
            ]
            (m)

keyboardMat : Model -> Element Msg
keyboardMat model =
    E.el
    [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark)
    ]
    (L.keyboard model.lightMode)

displayMat : Model -> Element Msg
displayMat model =
  EI.checkbox 
  []
  {onChange = DisplayMat
  ,icon = \b -> E.none
  ,checked = model.mat
  , label = EI.labelBelow 
            [ Font.color (if model.lightMode then subHeadlinesLabelsLight else subHeadlinesLabelsDark)
            , Font.family [Font.typeface "Bold"]
            ] 
            (E.text "Toggle Logic Keyboard" )
  }

matBox : Element Msg -> Model -> Element Msg
matBox underThis model = 
   E.el [Background.color yellow, E.below (displayMat model)]  
   (underThis)

view : Model -> Html Msg
view model =
    E.layout
        [ Background.color (if model.lightMode then backgroundPaneLight else backgroundPaneDark)
        ]
        (E.column
            [ E.width E.fill
            , E.height E.fill
            , E.spacing 20
            ]
            [ viewTitle model
            , E.el
                [ E.width E.fill
                , E.height E.fill
                , E.centerY
                ]
                (viewPrime model)
            ,   E.row[spacing 20]
                [ E.el
                  [ E.width (E.px 55)
                  , E.height (E.px 55)
                  , E.alignLeft
                  , E.alignBottom
                  , Background.image "images/elmLogo.png"
                  ]
                  (E.el [] E.none)
                , E.el  
                  [Font.family [Font.typeface "SemiBold"]
                  ,Font.color (if model.lightMode then bodyCopyLight else bodyCopyDark)
                  ]
                  (E.text "www.simplifyLogic.com")
                ]
            ]
        )

viewTitle : Model -> Element Msg
viewTitle model =
    E.el
        [ E.width E.fill
        , Font.center
        , Font.size 24
        , Font.family [ Font.typeface "Bold"]
        , Font.color (if model.lightMode then mainHeadlineLight else mainHeadlineDark)
        , Background.color (if model.lightMode then backgroundPaneLight else backgroundPaneDark)
        ]
        (E.text "Truth Table Generator")



viewPrime : Model -> Element Msg
viewPrime  model =
      case model.status of
       Default -> E.column [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark), spacing 50, E.centerY, E.centerX] 
                  [ matBox (myRowOfStuff model) model 
                  , if model.mat then keyboardMat model else E.none
                  ]
       Loading -> E.column [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark), spacing 50, E.centerY, E.centerX] 
               [ matBox (myRowOfStuff model) model 
               , loadingMessage model
               ]
       Failure tuple  ->  column [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark), spacing 50, E.centerY, E.centerX] 
                       [ matBox (myRowOfStuff model) model 
                       , if model.mat then keyboardMat model else E.none
                       , E.el [Font.color (if model.lightMode then errorsLight else errorsDark)] (E.text "Error found")
                       , E.el [Font.color (if model.lightMode then errorsLight else errorsDark)] ((E.text (Maybe.withDefault "" (Tuple.first tuple))))
                       ]
                     
       Success h -> case String.isEmpty h.err of
                     True -> column [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark), spacing 50,E.centerY, E.centerX]
                             [ matBox (myRowOfStuff model) model 
                             , if model.mat then keyboardMat model else E.none
                             , truthTableElement model (createTruthTable h)
                             , case h.validityy of
                                "Nothing" -> E.none
                                _         -> (showValidity model h.validityy)
                             ]

                     _    -> column [Background.color (if model.lightMode then secondaryPaneLight else secondaryPaneDark), spacing 50,E.centerY, E.centerX]
                             [ matBox (myRowOfStuff model) model 
                             , if model.mat then keyboardMat model else E.none
                             , E.el [Font.color (if model.lightMode then errorsLight else errorsDark)] (E.text "There was a parsing error")
                             , E.el [Font.color (if model.lightMode then errorsLight else errorsDark)] ((E.text "Sorry :("))
                             , E.el [Font.center, Font.color (if model.lightMode then errorsLight else errorsDark)] (E.text h.err)
                             ]
       

   
showValidity : Model -> String -> Element Msg
showValidity model s =
      case s of
       "Invalid" -> E.el
                    [Font.color (if model.lightMode then errorsLight else errorsDark)
                    ,Font.size 18
                    , Font.center
                    , E.centerX
                    , Font.family [Font.typeface "Regular"]
                    ]
                    (E.text s)
       _         -> E.el
                    [Font.color (if model.lightMode then successLight else successDark)
                    ,Font.size 18
                    , Font.center
                    , E.centerX
                    , Font.family [Font.typeface "Regular"]
                    ]
                    (E.text s)






update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
   SendArgHttpPost -> ( {model | status = Loading}
                 , Http.post
                 { url = "/api/submit"
                 , body = Http.jsonBody (E.object [("argument",E.string model.argument)])
                 , expect = Http.expectJson GotJson haskellResponseDecoder
                 } 
                 )
   GotJson result ->
    case result of
     Ok response ->
      ({model | status = Success response}, Cmd.none)
     Err error   ->
         case error of
          (Http.BadUrl s) ->  
                ({model | status = Failure (Just s, Nothing)} , Cmd.none)
          Http.Timeout -> 
               ({model | status = Failure (Just "Timeout",Nothing)}, Cmd.none)
          Http.NetworkError -> 
               ({model | status = Failure (Just "NetworkError",Nothing)} ,Cmd.none)
          (Http.BadStatus i) -> 
               ({model | status = Failure (Just "BadStatus", Just i)} , Cmd.none)
          (Http.BadBody s) -> 
               ({model | status = Failure (Just ("Badbody " ++ s), Nothing) }, Cmd.none) 

   UpdateArgument newArgument ->
    ({model | argument = newArgument}, Cmd.none)
   LightSwitch b -> ({model | lightMode = b}, Cmd.none) 
   LogicButton bf   -> case bf of
                          If -> (model, copyToClipboard "\u{2192}")
                          Iff ->(model, copyToClipboard "\u{2194}")
                          Not -> (model, copyToClipboard "\u{00AC}")
                          And -> (model, copyToClipboard "\u{2227}")
                          Or  -> (model, copyToClipboard "\u{2228}")
                          Xor -> (model, copyToClipboard "\u{22BB}")
                          Therefore -> (model,copyToClipboard "\u{2234}")
                          Nand -> (model,copyToClipboard "\u{22BC}")
                          Nor  -> (model,copyToClipboard "\u{22BD}")
   DisplayMat m   -> case m of
                      True -> ({model | mat = True }, Cmd.none)
                      False -> ({model | mat = False}, Cmd.none)
                  



--------------------------------------------------
-- THIS IS OUR DATA TYPE FOR CREATING A TABLE
type alias Argument =
      { assignment : (String, List Bool)
      , propositions : List (String, Bool)
      , conclusion  : Maybe (String, Bool)
      }

------
----- CREATING TRUTH TABLE FROM RESPONSE FROM BACK END -------------

createTruthTable : Messages.HaskellServerResponse -> Element Msg
createTruthTable h =
    let vars = Tuple.first (Maybe.withDefault ("",[]) (List.head (h.assignments)))
        isArg : Bool
        isArg = case h.validityy of 
                 "Nothing" -> False
                 _         -> True 
        args = createArguments h isArg                 
  
    in E.table [B.width 1, B.solid, (B.color backgroundPaneDark)]
       { data = args
       , columns = createColumns vars h.headers 
       }

-- foldr : (a -> b -> b) -> b -> List a -> b

createArguments : Messages.HaskellServerResponse -> Bool -> List Argument
createArguments h isArgument = 
      case h.assignments of
       [] -> []
       _ -> let firstAssignment = Maybe.withDefault ("",[] ) (List.head h.assignments)
                headProp        = Maybe.withDefault [] (List.head h.premiseEval)
                conclusioN      =   case isArgument of
                                     True -> Just (Maybe.withDefault ("",False) (List.head h.conclusionEval))
                                     False -> Nothing
                arg = { assignment   = firstAssignment
                      , propositions = headProp
                      , conclusion   = conclusioN
                      }
                newResponse = 
                   {h | assignments = Maybe.withDefault [] (List.tail h.assignments) , premiseEval = Maybe.withDefault [] (List.tail h.premiseEval), conclusionEval = Maybe.withDefault [] (List.tail h.conclusionEval)}
            in arg :: (createArguments newResponse isArgument)

stylePropositions : String -> Element Msg
stylePropositions s = E.el
                  [ Font.color backgroundPaneDark
                  , Font.family [Font.typeface "SemiBold"]
                  , Font.center
                  ]
                  (E.text s)

createColumns : String -> List String -> List (Column Argument Msg)
createColumns vars headerS =   
  let tailify : String -> String
      tailify s =  String.fromList (Maybe.withDefault [] (List.tail (String.toList s)))
      tailifyList : List a -> List a
      tailifyList l = Maybe.withDefault [] (List.tail l) 
  in case headerS of
      [] -> []
      [h] -> List.singleton { header =  stylePropositions h
                            , width = fill
                            , view = \arg -> viewMaybeConc h arg
                            }
      (headerr :: headerss) -> case List.isEmpty (String.toList vars) of
                                True -> {header = stylePropositions headerr
                                        , width = fill
                                        , view  = \arg -> viewProp headerr arg
                                        } :: (createColumns (tailify vars) (tailifyList headerS) )
                                False -> {header = E.el [Font.family [Font.typeface "SemiBold"] , Font.center] (E.text headerr)
                                         , width = fill
                                         , view  = \arg -> viewVar headerr arg
                                         } :: (createColumns (tailify vars) (tailifyList headerS))

toChar : String -> Char
toChar s =
     case (String.toList s) of
      [x] -> x
      _   -> '?'

getIndex : Char -> String -> Maybe Int
getIndex x l = g x (String.toList l) (List.length (String.toList l))
g : a -> List a -> Int -> Maybe Int 
g x l i = case l of
           [] -> Nothing
           (y :: ys) -> case x == y of
                         True  -> Just (i - (List.length ys) - 1)
                         False -> g x ys i                  

index : Int -> List a -> Maybe a
index i l =
   case (i < 0) || (i > List.length l) of
    True -> Nothing
    False -> case i == 0 of
              True -> case l of
                       [] -> Nothing
                       (x :: xs) -> Just x
              False -> case l of
                        [] -> Nothing
                        (x :: xs) -> index (i - 1) xs

fromBool : Bool -> String
fromBool b = 
   case b of
    True -> "T"
    False -> "F"


viewVar : String -> Argument -> Element Msg
viewVar var arg = 
   let indexOfVar  = Maybe.withDefault (-1) (getIndex (toChar var) (Tuple.first arg.assignment))
       boolean     = Maybe.withDefault True (index indexOfVar (Tuple.second arg.assignment))
   in  E.el [Font.family [Font.typeface "Regular"] , Font.center] (E.text (fromBool boolean))


viewMaybeConc : String -> Argument -> Element Msg
viewMaybeConc maybeConc arg = 
     case arg.conclusion of
      Nothing -> viewProp maybeConc arg
      (Just t) -> let b = Tuple.second t
                  in E.el [Font.family [Font.typeface "Regular"] , Font.center ] (E.text <| fromBool <| b)

viewProp : String -> Argument -> Element Msg
viewProp prop arg = 
     let props : List (String,Bool)
         props = arg.propositions
         findMyProp : List (String, Bool) -> String -> List (String, Bool)
         findMyProp l s = List.filter (\(x,y) -> s == x) l
         myPropDouble : (String,Bool)
         myPropDouble = case (findMyProp props prop) of
                         []    -> ("couldNotFindProp",False)
                         [d]   -> d
                         (d :: ds) -> ("Dumbass you wrote the prop more than once" ++ (Tuple.first d), Tuple.second d)
         myPropBoolean : Bool
         myPropBoolean = Tuple.second myPropDouble
     in 
      E.el [Font.family [Font.typeface "Regular"] , Font.center ](E.text (fromBool myPropBoolean))
--- ENDING OF CREATING TRUTH TABLE
-----------------------------------------                            
--- JSON DECODER FOR HASKELL BACKEND
haskellResponseDecoder : Decoder Messages.HaskellServerResponse
haskellResponseDecoder =
    D.map6 Messages.HaskellServerResponse 
        (D.field "err" D.string)
        (D.field "headers" (D.list D.string))
        (D.field "assignments" (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 (D.list D.bool)))))
        (D.field "premiseEval" (D.list (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.bool)))))
        (D.field "conclusionEval" (D.list (D.map2 Tuple.pair (D.index 0 D.string) (D.index 1 D.bool))))
        (D.field "validityy" D.string)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
--
