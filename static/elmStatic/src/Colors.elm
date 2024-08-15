module Colors exposing (..)

import Element as E exposing (rgb255, Color) 

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
mauve = rgb255 136 57 239
yellow = rgb255 223 142 29
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
