module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes as Attributes exposing (..)
import Element exposing (..)
import Element.Input as Input exposing (..)
import Element.Background as Background exposing (..)
import Element.Font as Font exposing (..)
import Element.Border as Border exposing (..)

main =
  Browser.element 
  {
    init = init
    , view = view 
    , update = update
    , subscriptions = subscriptions
  }

-- Colors
peachMocha : Color
peachMocha = rgb255 250 179 135
skyMocha : Color
skyMocha = rgb255 137 220 235
redMocha : Color
redMocha = rgb255 210 15 57
baseMocha : Color
baseMocha = rgb255 30 30 46
surface2Mocha : Color
surface2Mocha = rgb255 88 91 112
textMocha : Color
textMocha = rgb255 205 214 244

type alias Model =
    { lightMode : Bool
    , semanticList : Bool
    , syntacticList : Bool
    , contactInfo : Bool
    }
init : () -> (Model, Cmd Msg)
init _ = (
         { lightMode = False
         , semanticList = False
         , syntacticList = False
         , contactInfo = False
         }
         , 
         Cmd.none
         )

type Msg = DefaultPage 
         | LightMode Bool
         | ProofList Proof
         | ContactInfo Bool

type Proof = Semantic Bool
           | Syntactic Bool

view : Model -> Html Msg
view model =
  Element.layout [Background.color baseMocha]
  (
    Element.column
    [ Element.spacing 50
    , Element.centerX
    , Element.width Element.fill
    , Border.rounded 50
    ]
    [
      let title = Element.el
                  [
                    Element.centerX 
                  , Background.color surface2Mocha
                  , Font.family [Font.typeface "Bold"]
                  , Font.size 50
                  , Font.color peachMocha
                  , Border.rounded 50

                  ]
                  (Element.text "www.LogicTools.com")
      in 
       title
    , let introduction =  Element.column
                          [ Element.padding 100
                          , Element.centerX
                          , Background.color surface2Mocha
                          , spacing 50
                          , Border.rounded 50
                          , Font.family [Font.typeface "Regular"]
                          , Font.color textMocha
                          ]
                          [
                             Element.text "Logic has been a very important tool for me."
                          ,  lineBreak
                          ,  Element.text "Propsitional logic has two components."
                          ,  lineBreak 

                          ,  Element.el 
                             [ Element.below <| if model.semanticList then semanticProofList else Element.none
                             ,Element.alignLeft
                             ]
                             (
                             Element.el 
                             [ Element.onRight (semanticListButton model) ]
                             (Element.text "Semantics")
                             )

                          ,  lineBreak
                          ,  lineBreak
                          ,  lineBreak
                          ,  lineBreak
                          ,  Element.el 
                             [Element.below <| if model.syntacticList then syntacticProofList else Element.none
                             ,Element.alignLeft
                             ]
                             (
                             Element.el 
                             [ Element.onRight (syntacticListButton model) ]
                             (Element.text "Syntax")
                             )

                          ,  lineBreak
                          ,  lineBreak
                          ,  lineBreak
                          ,  lineBreak
                          ,  Element.text "We have semantic proofs and syntactic proofs."
                          ,  lineBreak
                          ,  Element.text "This is my contact information."
                          ,  contactInfoButton model
                          ,  case model.contactInfo of
                              True -> myContactInfo
                              False -> Element.none
                          ]
                         
      in
       introduction
    
      
    ]
  )
lineBreak : Element Msg
lineBreak = Element.html (Html.br [] [])



myContactInfo : Element Msg
myContactInfo = 
     Element.column 
     [spacing 5, Background.color skyMocha, Font.color redMocha, Border.rounded 50]
     [ Element.el 
       [Font.family [Font.typeface "Bold"], Font.center, Element.centerX ]
       (Element.text "My Contact Info")
     , let mytwitter = Element.el 
                      [Element.padding 10
                      , Font.family [Font.typeface "Medium"]]
                      (
                        Element.link 
                        []
                        {
                          url = "https://twitter.com/AlecRod23252112"
                        , label = Element.text "A link to my twitter"
                        }
                      )
      in mytwitter
    , let myEmail = Element.el 
                      [Element.padding 10, Font.family [Font.typeface "Medium"]]
                      (
                        Element.text "alecrodriguez1247@gmail.com"
                      )
    
      in myEmail
    , let myGithub = Element.el 
                      [Element.padding 10, Font.family [Font.typeface "Medium"]]
                      (
                        Element.link 
                        []
                        {
                          url = "https://github.com/alecTheBrainDamaged"
                        , label = Element.text "A link to my github"
                        }
                      )
      in myGithub
     ]
contactInfoButton : Model -> Element Msg
contactInfoButton model = 
      Input.checkbox
      [ Element.pointer
      ]
      {
        onChange = ContactInfo
      , icon     = Input.defaultCheckbox
      , checked  = model.contactInfo
      , label = Input.labelHidden "show semantic proof links"
      }

semanticProofList : Element Msg
semanticProofList = 
    Element.html
    (
    Html.ul 
    [

    ]
    [
      Html.li [] 
      [ Html.a 
        [
          Attributes.href "/t"
        , Attributes.target "_blank"
        ]
        [
        Html.text "Truth Table Generator"
        ]
      ]
    ] 
    )
syntacticProofList : Element Msg
syntacticProofList = 
    Element.html
    (
    Html.ul 
    [

    ]
    [
      Html.li [] 
      [
        Html.a 
        [
          Attributes.href "www.bing.com"
        , Attributes.target "_blank"
        ] 
        [
        Html.text "Natural Deduction"
        ]
      ]
    ] 
    )
    
semanticListButton : Model -> Element Msg
semanticListButton model = 
      Input.checkbox
      [ Element.pointer
      ]
      {
        onChange = \b -> ProofList <| Semantic b
      , icon     = Input.defaultCheckbox
      , checked  = model.semanticList
      , label = Input.labelHidden "show semantic proof links"
      }

syntacticListButton : Model -> Element Msg
syntacticListButton model = 
      Input.checkbox
      [ Element.pointer
      ]
      {
        onChange = \b -> ProofList <| Syntactic b
      , icon     = Input.defaultCheckbox
      , checked  = model.syntacticList
      , label = Input.labelHidden "show syntactic proofs list"
      }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
 case msg of
  DefaultPage    -> (model,Cmd.none) 
  LightMode b    -> ({model | lightMode = b}, Cmd.none)
  ProofList proof -> case proof of
                      Semantic b -> ({model | semanticList = b}, Cmd.none)
                      Syntactic b -> ({model | syntacticList = b}, Cmd.none)
  ContactInfo bool -> ({model | contactInfo = bool}, Cmd.none)
       

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

