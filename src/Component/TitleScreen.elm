module Component.TitleScreen exposing (Model, Msg(..), Effect(..), init, update, draw)

-- EXTERNAL IMPORTS

import Game.Update as Update exposing (Update)
import Effects exposing (Effects)
import Collage exposing (Form)
import Time exposing (Time)


-- LOCAL IMPORTS

import DefaultText
import DrawUtilities


-- MODEL


type alias Model =
    { timeSinceDismissal : Maybe Time
    }


init : Effects Model Effect
init =
    Effects.return { timeSinceDismissal = Nothing }


fadeAwayTime : Time
fadeAwayTime =
    1.5 * Time.second


isDismissed : Model -> Bool
isDismissed model =
    case model.timeSinceDismissal of
        Just time ->
            True

        Nothing ->
            False



-- UPDATE


type Msg
    = SecondsElapsed Float
    | Dismiss


type Effect
    = PlaySound String
    | StartGame


update : Msg -> Update Model Effect
update msg model =
    case msg of
        Dismiss ->
            if isDismissed model then
                Update.returnAlive model
            else
                Update.returnAlive { model | timeSinceDismissal = Just 0 }

        SecondsElapsed dt ->
            case model.timeSinceDismissal of
                Just time ->
                    let
                        newTimeSinceDismissal =
                            time + dt * Time.second
                    in
                        if newTimeSinceDismissal >= fadeAwayTime then
                            Update.returnDead
                                |> Effects.add [ StartGame ]
                        else
                            Update.returnAlive
                                { model
                                    | timeSinceDismissal = Just newTimeSinceDismissal
                                }

                Nothing ->
                    Update.returnAlive model



-- VIEW


draw : Model -> Form
draw model =
    Collage.group
        [ DefaultText.draw 40 "elmsteroids"
            |> Collage.moveY 50
        , DefaultText.draw 16 "github.com/yupferris // 2016"
            |> Collage.moveY -30
        , DefaultText.draw 14 "press enter/return to begin"
            |> Collage.moveY -50
        ]
        |> DrawUtilities.alpha' (currentAlpha model)


currentAlpha : Model -> Float
currentAlpha model =
    Maybe.map (\x -> 1 - (x / fadeAwayTime)) model.timeSinceDismissal
        |> Maybe.withDefault 1
