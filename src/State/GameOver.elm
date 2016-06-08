module State.GameOver exposing (Model, Msg(..), Effect(..), init)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Collage exposing (Form)


-- LOCAL IMPORTS

import DrawUtilities


-- </editor-fold> END IMPORTS


type alias Model =
    { score : Int
    , sector : Int
    }


type alias InitArgs =
    { score : Int
    , sector : Int
    }


init : InitArgs -> ( Model, List Effect )
init args =
    let
        initialModel =
            { score = args.score
            , sector = args.sector
            }
    in
        ( initialModel, [] )


type Msg
    = Dismiss


type Effect
    = ReturnToTitle


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        Dismiss ->
            ( model, [ ReturnToTitle ] )


view : Model -> Form
view model =
    DrawUtilities.emptyForm
