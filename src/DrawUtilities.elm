module DrawUtilities exposing (drawMaybe, emptyForm, alpha')

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form)
import Element


-- </editor-fold> END IMPORTS
-- <editor-fold> FUNCTIONS


drawMaybe : (a -> Form) -> Maybe a -> Form
drawMaybe drawFn maybeObj =
    Maybe.map drawFn maybeObj
        |> Maybe.withDefault emptyForm


emptyForm : Form
emptyForm =
    Collage.toForm Element.empty


{-| The alpha from Collage is bugged because drawing a form
with an alpha of 0 renders the object as fully opaque. This
function works around that by rendering the object at a very,
very low alpha when it should be 0.
-}
alpha' : Float -> Form -> Form
alpha' t =
    Collage.alpha (max 0.00001 t)



-- </editor-fold>
