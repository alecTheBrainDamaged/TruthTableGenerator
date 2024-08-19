module LogicButtons exposing (..)

import Element.Background as Background exposing (color)
import Element as E exposing (el, text, row, column)
import Element.Input as EI exposing (button)
import Element.Font as Font exposing (typeface,family,center)
import Colors exposing (..)
import Messages exposing (..)

redLatte = E.rgb255 210 15 57

topKeys : Bool -> E.Element Msg
topKeys lightMode =
   E.el 
   [Background.color <| if lightMode then redLatte else peachMocha
   , E.centerX
   , E.width E.fill
   ]
   (
   E.row [E.spacing 5, E.padding 10, E.centerY, E.width E.fill] 
         [ notButton lightMode
         , ifThenButton lightMode
         , iffButton lightMode
         , ifButton lightMode
         , andButton lightMode
         ]
   )

bottomKeys : Bool -> E.Element Msg
bottomKeys lightMode =
   E.el 
   [Background.color <| if lightMode then redLatte else peachMocha
   , E.centerX
   , E.width E.fill
   ]
   (
   E.row [E.spacing 5, E.padding 10, E.centerY, E.width E.fill] 
         [ nandButton lightMode
         , orButton lightMode
         , xorButton lightMode
         , norButton lightMode
         , topButton lightMode
         ]
   )

evenMoreBottomKeys : Bool -> E.Element Msg
evenMoreBottomKeys lightMode = 
   E.el 
   [Background.color <| if lightMode then redLatte else peachMocha
   , E.centerX
   , E.width E.fill
   ]
   (
   E.row [E.spacing 5, E.padding 10, E.centerY, E.width E.fill] 
         [ bottomButton lightMode
         , thereforeButton lightMode
         ]
   )

keyboard : Bool -> E.Element Msg
keyboard b =
   E.column [E.centerX, E.centerY, E.spacing 1, E.width E.fill]
            [ topKeys b
            , bottomKeys b
            , evenMoreBottomKeys b 
            ]
   
  
-- (if lightMode then bodyCopyLight else bodyCopyDark)
ifThenButton : Bool -> E.Element Msg
ifThenButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [ E.width E.fill
   ]
   { onPress = Just <| LogicButton IfThen
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark), Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{2192}")
   }
   )          

iffButton : Bool -> E.Element Msg
iffButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Iff
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{2194}")
   }
   )  

notButton : Bool -> E.Element Msg
notButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Not
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{00AC}")
   }
   )  


andButton : Bool -> E.Element Msg
andButton lightMode =
   E.el 
   [Background.color (if lightMode then surfaceElementLight else surfaceElementDark), E.width E.fill]
   (EI.button [E.width E.fill] 
    { onPress = Just <| LogicButton And 
    , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{2227}")
    }
   )  


orButton : Bool -> E.Element Msg
orButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Or
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{2228}")
   }
   )  


xorButton : Bool -> E.Element Msg
xorButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Xor
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{22BB}")
   }
   )  

thereforeButton : Bool -> E.Element Msg
thereforeButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Therefore
   , label  =  
    E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] 
    (E.text "\u{22A8}")
   }
   )  

nandButton : Bool -> E.Element Msg
nandButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Nand
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{22BC}")
   }
   )  



norButton : Bool -> E.Element Msg
norButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Nor
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{22BD}")
   }
   )  
ifButton : Bool -> E.Element Msg
ifButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton If
   , label  =  E.el [Font.color (if lightMode then bodyCopyLight else bodyCopyDark),Font.center, Font.family [Font.typeface "Bold"], E.width E.fill] (E.text "\u{2190}")
   }
   )  
topButton : Bool -> E.Element Msg
topButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Top
   , label  =  E.el 
               [ Font.color (if lightMode then bodyCopyLight else bodyCopyDark)
               , Font.center
               , Font.family [Font.typeface "Bold"]
               , E.width E.fill
               ] 
               (E.text "\u{22A4}")
   }
   )
bottomButton : Bool -> E.Element Msg
bottomButton lightMode =
   E.el 
   [ Background.color (if lightMode then surfaceElementLight else surfaceElementDark)
   , E.width E.fill
   ]
   (
   EI.button 
   [E.width E.fill
   ]
   { onPress = Just <| LogicButton Bottom
   , label  =  E.el 
               [ Font.color (if lightMode then bodyCopyLight else bodyCopyDark)
               , Font.center
               , Font.family [Font.typeface "Bold"]
               , E.width E.fill
               ] 
               (E.text "\u{22A5}")
   }
   )

