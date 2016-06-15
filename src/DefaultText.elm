module DefaultText exposing (draw)

import Text
import Color
import Collage exposing (Form)


draw : Float -> String -> Form
draw size =
    Text.fromString
        >> Text.style
            { typeface = [ "Courier New" ]
            , height = Just size
            , color = Color.white
            , bold = False
            , italic = False
            , line = Nothing
            }
        >> Collage.text
