module DrawUtilities exposing (drawGroup)

import List exposing (map)
import Collage exposing (Form, group)

drawGroup : (a -> Form) -> List a -> Form
drawGroup drawFn = map drawFn >> group
