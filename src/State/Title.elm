module State.Title exposing (Model, Msg, Effect, init, update, draw)

-- EXTERNAL IMPORTS

import Collage exposing (Form)


-- LOCAL IMPORTS

import DrawUtilities


type alias Model =
    { dismissed : Bool
    , secondsSinceDismissal : Float
    }


init : ( Model, List Effect )
init =
    let
        initialModel =
            { dismissed = False
            , secondsSinceDismissal = 0
            }
    in
        ( initialModel, [] )


fadeAwayTime : Float
fadeAwayTime =
    1.5


type Msg
    = SecondsElapsed Float
    | Dismiss


type Effect
    = StartGame


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        SecondsElapsed dt ->
            ( model, [] )

        Dismiss ->
            ( model, [ StartGame ] )


draw : Model -> Form
draw model =
    DrawUtilities.emptyForm
