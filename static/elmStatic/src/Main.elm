module Main exposing (..)

import Element as E exposing (..)
import Element.Border as B exposing (..)
import Element.Input as EI exposing (..)
import Element.Events exposing (onClick)
import Element.Background as Background exposing (..)
import Html exposing (..)
import Http 
import Element.Font as Font exposing (..)
import Json.Decode as D exposing (..)
import Json.Encode as E exposing (..)
import Browser

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
   }

type alias LightMode = Bool

type Status
   = Failure (Maybe String, Maybe Int)
   | Loading 
   | Success HaskellServerResponse
   | Default

init : () -> (Model , Cmd Msg)
init _ = 
   ( 
     {status = Default
     , argument = ""
     , lightMode = False
     }
   , Cmd.none
   )
---- OUR MSGS
type Msg = SendArgHttpPost
         | GotJson (Result Http.Error HaskellServerResponse)
         | UpdateArgument String
         | LightSwitch Bool

------- THIS IS part of our VIEW MODULE, What in the FUCK we are RENDERING ON SCREEN. 



------------------



-- BACKGROUNDS BOTH DARK AND LIGHT RULES
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
backgroundPaneLight : Color
backgroundPaneLight = rgb255 239 241 245  
secondaryPaneLight = rgb255 220 224 232   -- Crust
surfaceElementLight = rgb255 172 176 190 -- Surface2
overlayLight = rgb255 124 127 147 --overlay 2
-- Typography section
mainHeadlineLight = rgb255 76 79 105
bodyCopyLight = mainHeadlineLight
subHeadlinesLabelsLight = rgb255 92 95 119
successLight = rgb255 64 260 43
errorsLight = rgb255 210 15 57
cursorLight = rgb255 220 238 120


-- FRAPPE DARK
backgroundPaneDark : Color 
backgroundPaneDark = rgb255 48 52 70 -- Base
secondaryPaneDark = rgb255 35 38 52  -- Crust
surfaceElementDark = rgb255 98 104 128 -- Surface2
overlayDark = rgb255 148 156 187 --Overlay2
--TypoGraphy section
mainHeadlineDark = rgb255 198 208 245 --Text
bodyCopyDark = mainHeadlineDark
subHeadlinesLabelsDark = rgb255 181 191 226 -- subtext1
successDark = rgb255 166 209 137
errorsDark = rgb255 231 130 132

           
myRowOfStuff : Model -> Element Msg
myRowOfStuff model =
    case model.lightMode of
     True -> row [E.width fill, centerY, spacing 30, Background.color secondaryPaneLight]
             [E.el [ E.alignRight] (myArgumentBoxElement model)
             , submitArgButton model 
             , lightModebox model
             ]
     False -> row [E.width fill, centerY, spacing 30, Background.color secondaryPaneDark]
              [E.el [ E.alignRight] (myArgumentBoxElement model)
              , submitArgButton model 
              , lightModebox model
              ]
myArgumentBoxElement : Model -> Element Msg
myArgumentBoxElement model =
    case model.lightMode of
     True -> E.el 
             [ Background.color surfaceElementLight, B.rounded 3
             , padding 30
             ]
             (inputArgumentBox model) 
     False ->  E.el 
             [ Background.color surfaceElementDark, B.rounded 3
             , padding 30
             ]
             (inputArgumentBox model) 

inputArgumentBox : Model -> Element Msg
inputArgumentBox model = 
     case model.lightMode of
      True -> EI.multiline [Background.color surfaceElementLight,Font.color bodyCopyLight, Font.family [Font.typeface "Regular"]] 
              { onChange = UpdateArgument
              , text = model.argument
              , placeholder = Nothing
              , label = EI.labelAbove [Font.color subHeadlinesLabelsLight, Font.family [Font.typeface "Bold"]] (E.text "Prop Box")
              , spellcheck = False
              }
      False -> EI.multiline [Background.color surfaceElementDark, Font.color bodyCopyDark, Font.family [Font.typeface "Regular"]] 
               { onChange = UpdateArgument
               , text = model.argument
               , placeholder = Nothing
               , label = EI.labelAbove [Font.color subHeadlinesLabelsDark, Font.family [Font.typeface "Bold"]] (E.text "Prop Box")
               , spellcheck = False
               }

submitArgButton : Model -> Element Msg
submitArgButton m = 
   case m.lightMode of
    True -> E.el
            [Background.color surfaceElementLight
            , Font.color bodyCopyLight
            , B.rounded 3
            , B.color secondaryPaneLight
            , padding 30
            ]
           (EI.button [pointer, Background.color surfaceElementLight] 
            { onPress = Just SendArgHttpPost
            , label = E.el [Font.color bodyCopyLight] (E.text "Submit")
            }
           )
    False -> E.el
            [Background.color surfaceElementDark
            , Font.color bodyCopyDark
            , B.rounded 3
            , B.color secondaryPaneDark
            , padding 30
            ]
           (EI.button [pointer, Background.color surfaceElementDark] 
            { onPress = Just SendArgHttpPost
            , label = E.el [Font.color bodyCopyDark] (E.text "Submit")
            }
           )

lightModebox : Model -> Element Msg
lightModebox m = 
    case m.lightMode of
     True -> EI.checkbox
             [ Background.color surfaceElementLight
             , pointer
             ]
             {onChange = LightSwitch
             , icon = defaultCheckbox
             , checked = m.lightMode
             , label = EI.labelBelow [Font.color subHeadlinesLabelsLight, Font.family [Font.typeface "Bold"]] (E.text "Toggle Light Mode" )
             } 
     False ->  EI.checkbox
             [ Background.color surfaceElementDark
             , pointer
             ]
             {onChange = LightSwitch
             , icon = defaultCheckbox
             , checked = m.lightMode
             , label = EI.labelBelow [Font.color subHeadlinesLabelsDark, Font.family [Font.typeface "Bold"]] (E.text "Toggle Light Mode" )
             } 

title : Model -> Element Msg
title model =
  case model.lightMode of
   True -> E.el 
           [ 
             Background.color surfaceElementLight
           , Font.color mainHeadlineLight
           , Font.family
             [Font.typeface "ExtraBold"
             ]
           , B.rounded 3
           , padding 0
           , alignTop
           , E.centerX
           ]
           (E.text "Truth Table Generator") 
   False ->  E.el 
             [ Background.color surfaceElementDark
             , Font.color mainHeadlineDark
             , Font.family
               [Font.typeface "ExtraBold"
               ]
             , B.rounded 3
             , padding 0
             , alignTop
             , E.centerX
             ]
             (E.text "Truth Table Generator") 


loadingMessage : Model -> Element Msg
loadingMessage model =
   case model.lightMode of
    True -> E.el 
            [ Background.color surfaceElementLight
            , Font.color bodyCopyLight
            , B.rounded 3
            ]
            (E.text "loading table ...") 
    False -> E.el 
             [ Background.color surfaceElementDark
             , Font.color bodyCopyDark
             , B.rounded 3
             ]
             (E.text "loading table ...") 

truthTableElement : Model -> Element Msg -> Element Msg
truthTableElement model m =
   case model.lightMode of
    True -> E.el 
            [ Background.color surfaceElementLight
            , Font.color bodyCopyLight
            , B.rounded 3
            ]
            (m)
    False ->  E.el 
              [ Background.color surfaceElementDark
              , Font.color bodyCopyDark
              , B.rounded 3
              ]
              (m)


view : Model -> Html Msg
view model =
 case model.lightMode of
  False   -> E.layout [Background.color backgroundPaneDark] (viewPrime model)
  True    -> E.layout [Background.color backgroundPaneLight] (viewPrime model) 

viewPrime : Model -> Element Msg
viewPrime  model =
   case model.lightMode of
    True -> 
      case model.status of
       Default -> E.column [Background.color secondaryPaneLight , E.centerX , spacing 10] 
                  [ title model
                  , myRowOfStuff model 
                  ]
       Loading -> E.column [Background.color secondaryPaneLight , E.centerX , spacing 10] 
               [ title model
               , myRowOfStuff model 
               , loadingMessage model
               ]
       Failure tuple  ->  column [Background.color secondaryPaneLight , E.centerX , spacing 10] 
                       [ title model
                       , myRowOfStuff model 
                       , E.el [Font.color errorsLight] (E.text "Error found")
                       , E.el [Font.color errorsLight] ((E.text (Maybe.withDefault "" (Tuple.first tuple))))
                       ]
                     
       Success h -> case String.isEmpty h.err of
                     True -> column [Background.color secondaryPaneLight, E.centerX ,  spacing 10]
                             [ title model
                             , myRowOfStuff model 
                             , truthTableElement model (createTruthTable h)
                             , case h.validityy of
                                "Nothing" -> E.none
                                _         -> (E.text h.validityy)
                             ]

                     _    -> column [Background.color secondaryPaneLight, E.centerX , spacing 10]
                             [ title model
                             , myRowOfStuff model 
                             , E.el [Font.color errorsLight] (E.text "There was a parsing error")
                             , E.el [Font.color errorsLight] ((E.text "Sorry :("))
                             , E.el [Font.center, Font.color errorsLight] (E.text h.err)
                             ]
    False -> case model.status of
              Default -> E.column [Background.color secondaryPaneDark, E.centerX , spacing 10] 
                         [ title model
                         , myRowOfStuff model 
                         ]
              Loading -> E.column [Background.color secondaryPaneDark, E.centerX , spacing 10] 
                         [ title model
                         , myRowOfStuff model 
                         , loadingMessage model
                         ]
              Failure tuple  ->  column [Background.color secondaryPaneDark, E.centerX , spacing 10] 
                                 [ title model
                                 , myRowOfStuff model 
                                 , E.el [Font.color errorsDark] (E.text "Error found")
                                 , E.el [Font.color errorsDark] ((E.text (Maybe.withDefault "" (Tuple.first tuple))))
                                 ]
                     
              Success h -> case String.isEmpty h.err of
                            True -> 
                               column [Background.color secondaryPaneDark, E.centerX ,  spacing 10]
                                      [ title model
                                      , myRowOfStuff model 
                                      , truthTableElement model (createTruthTable h)
                                      , case h.validityy of
                                         "Nothing" -> E.none
                                         _         -> (showValidity model h.validityy)
                                      ]

                            _    -> column [Background.color secondaryPaneDark, E.centerX , spacing 10]
                                    [ title model
                                    , myRowOfStuff model 
                                    , E.paragraph [Font.color errorsDark] 
                                      [E.text "There was a parsing error"
                                      ,E.text "Sorry :("
                                      ,E.el [Font.center] (E.text h.err)
                                      ]
                                    ]

   
showValidity : Model -> String -> Element Msg
showValidity model s =
  case model.lightMode of
   True ->   case s of
              "Invalid" -> E.el
                           [Font.color errorsLight
                           ,Font.size 18
                           , Font.center
                           , Font.family
                           [Font.typeface "Open Sans"
                           ,Font.sansSerif
                           ]
                           ]
                           (E.text s)
              _         -> E.el
                           [Font.color successLight
                           ,Font.size 18
                           , Font.center
                           , Font.family
                           [Font.typeface "Open Sans"
                           ,Font.sansSerif
                           ]
                           ]
                           (E.text s)

   False ->    case s of
               "Invalid" -> E.el
                            [Font.color errorsDark
                            ,Font.size 18
                            , Font.center
                            , Font.family
                            [Font.typeface "Open Sans"
                            ,Font.sansSerif
                            ]
                            ]
                            (E.text s)
               _         -> E.el
                            [Font.color successDark
                            ,Font.size 18
                            , Font.center
                            , Font.family
                            [Font.typeface "Open Sans"
                            ,Font.sansSerif
                            ]
                            ]
                            (E.text s)


type alias HaskellServerResponse =
      { err : String
      , headers : List String
      , assignments : List (String, List Bool)
      , premiseEval : List (List (String,Bool))
      , conclusionEval : List (String, Bool)
      , validityy : String
      }

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

--------------------------------------------------
-- THIS IS OUR DATA TYPE FOR CREATING A TABLE
type alias Argument =
      { assignment : (String, List Bool)
      , propositions : List (String, Bool)
      , conclusion  : Maybe (String, Bool)
      }

------
----- CREATING TRUTH TABLE FROM RESPONSE FROM BACK END -------------

createTruthTable : HaskellServerResponse -> Element Msg
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

createArguments : HaskellServerResponse -> Bool -> List Argument
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
                  [Font.color backgroundPaneDark
                  , Font.italic
                  , Font.family
                    [Font.typeface "Open Sans"
                    ,Font.sansSerif
                    ]
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
      [h] -> List.singleton { header = column [B.width 1, B.solid, B.color backgroundPaneDark] [E.el [E.centerX, E.centerY] (stylePropositions h)]
                            , width = fill
                            , view = \arg -> column [B.width 1, B.solid, B.color backgroundPaneDark] [viewMaybeConc h arg]
                            }
      (headerr :: headerss) -> case List.isEmpty (String.toList vars) of
                                True -> {header = column [B.width 1, B.solid, B.color backgroundPaneDark] [E.el [E.centerX, E.centerY] (stylePropositions headerr)]
                                        , width = fill
                                        , view  = \arg -> column [B.width 1, B.solid, B.color backgroundPaneDark] [viewProp headerr arg]
                                        } :: (createColumns (tailify vars) (tailifyList headerS) )
                                False -> {header = column [B.width 1, B.solid, B.color backgroundPaneDark] [E.el [E.centerX, E.centerY] (E.text headerr)]
                                         , width = fill
                                         , view  = \arg -> column [B.width 1, B.solid, B.color backgroundPaneDark] [viewVar headerr arg]
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
   in  E.el [E.centerX, E.centerY] (E.text (fromBool boolean))


viewMaybeConc : String -> Argument -> Element Msg
viewMaybeConc maybeConc arg = 
     case arg.conclusion of
      Nothing -> viewProp maybeConc arg
      (Just t) -> let b = Tuple.second t
                  in E.el [E.centerX, E.centerY] (E.text <| fromBool <| b)

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
      E.el [E.centerX, E.centerY](E.text (fromBool myPropBoolean))
--- ENDING OF CREATING TRUTH TABLE
-----------------------------------------                            
--- JSON DECODER FOR HASKELL BACKEND
haskellResponseDecoder : Decoder HaskellServerResponse
haskellResponseDecoder =
    D.map6 HaskellServerResponse 
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

