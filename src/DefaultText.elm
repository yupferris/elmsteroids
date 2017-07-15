module DefaultText exposing (defaultText)

import Text exposing (fromString, style)
import Color exposing (..)
import Collage exposing (Form, text)


defaultText : Float -> String -> Form
defaultText size =
    fromString
        >> style
            { typeface = [ "Courier New" ]
            , height = Just size
            , color = white
            , bold = False
            , italic = False
            , line = Nothing
            }
        >> text
