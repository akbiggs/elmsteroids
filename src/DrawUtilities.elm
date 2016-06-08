module DrawUtilities exposing (drawMaybe, emptyForm)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form, group)
import Element


-- </editor-fold> END IMPORTS
-- <editor-fold> FUNCTIONS


drawMaybe : (a -> Form) -> Maybe a -> Form
drawMaybe drawFn maybeObj =
    case maybeObj of
        Just obj ->
            drawFn obj

        Nothing ->
            emptyForm


emptyForm : Form
emptyForm =
    Collage.toForm Element.empty



-- </editor-fold>
