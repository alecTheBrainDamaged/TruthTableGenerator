module LogicButtons exposing (..)

import Element.Background as Background exposing (color)
import Element as E exposing (el, text, row, column)
import Element.Input as EI exposing (button)
import Element.Font as Font exposing (typeface,family,center)
import Colors exposing (..)
import Messages exposing (..)



topKeys : Bool -> E.Element Msg
topKeys lightMode =
   E.el 
   [Background.color mauve
   , E.centerX
   ]
   (
   E.row [E.spacing 5, E.padding 10, E.centerY] 
         [ ifButton lightMode
         , iffButton lightMode
         , notButton lightMode
         , andButton lightMode
         ]
   )

bottomKeys : Bool -> E.Element Msg
bottomKeys lightMode =
   E.el 
   [Background.color mauve
   , E.centerX
   ]
   (
   E.row [E.spacing 5, E.padding 10, E.centerY] 
         [ orButton lightMode
         , xorButton lightMode
         , nandButton lightMode
         , norButton lightMode
         , thereforeButton lightMode
         ]
   )

keyboard : Bool -> E.Element Msg
keyboard b =
   E.column [E.centerX, E.centerY, E.spacing 1]
            [topKeys b
            ,bottomKeys b
            ]
   
  
ifButton : Bool -> E.Element Msg
ifButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton If
   , label  =  E.el [Font.color yellow, Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{2192}")
   }
   )          

iffButton : Bool -> E.Element Msg
iffButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Iff
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{2194}")
   }
   )  

notButton : Bool -> E.Element Msg
notButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Not
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{00AC}")
   }
   )  


andButton : Bool -> E.Element Msg
andButton lightMode =
   E.el 
   [Background.color (if lightMode then surfaceElementLight else surfaceElementDark)]
   (EI.button [] 
    { onPress = Just <| LogicButton And 
    , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{2227}")
    }
   )  


orButton : Bool -> E.Element Msg
orButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Or
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{2228}")
   }
   )  


xorButton : Bool -> E.Element Msg
xorButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Xor
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{22BB}")
   }
   )  

thereforeButton : Bool -> E.Element Msg
thereforeButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Therefore
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{2234}")
   }
   )  

nandButton : Bool -> E.Element Msg
nandButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Nand
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{22BC}")
   }
   )  



norButton : Bool -> E.Element Msg
norButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   ]
   (
   EI.button 
   [
   ]
   { onPress = Just <| LogicButton Nor
   , label  =  E.el [Font.color yellow,Font.center, Font.family [Font.typeface "Thin"]] (E.text "\u{22BD}")
   }
   )  


