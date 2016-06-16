module Component.GameOverScreen exposing (Model, Msg(..), Effect(..), init, update, draw)

-- EXTERNAL IMPORTS

import Game.Update as Update exposing (Update)
import Effects exposing (Effects)
import Time exposing (Time)
import Collage exposing (Form)
import Color


-- LOCAL IMPORTS

import DefaultText
import DrawUtilities
import MaybeUtilities
import Bounds


-- MODEL


type alias Model =
    { timeSinceShowing : Time
    , timeSinceDismissal : Maybe Time
    , score : Int
    , sector : Int
    }


type alias InitArgs =
    { score : Int
    , sector : Int
    }


init : InitArgs -> Effects Model Effect
init { score, sector } =
    Effects.return
        { timeSinceShowing = 0
        , timeSinceDismissal = Nothing
        , score = score
        , sector = sector
        }


fadeInTime : Time
fadeInTime =
    1.0 * Time.second


fadeOutTime : Time
fadeOutTime =
    1.5 * Time.second


isDismissed : Model -> Bool
isDismissed model =
    MaybeUtilities.hasValue model.timeSinceDismissal



-- UPDATE


type Msg
    = SecondsElapsed Float
    | Dismiss


type Effect
    = RestartGame


update : Msg -> Update Model Effect
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                newTimeSinceShowing =
                    model.timeSinceShowing + dt * Time.second
            in
                case model.timeSinceDismissal of
                    Just time ->
                        let
                            newTimeSinceDismissal =
                                time + dt * Time.second
                        in
                            if newTimeSinceDismissal >= fadeOutTime then
                                Update.returnDead
                                    |> Effects.add [ RestartGame ]
                            else
                                Update.returnAlive
                                    { model
                                        | timeSinceDismissal = Just newTimeSinceDismissal
                                        , timeSinceShowing = newTimeSinceShowing
                                    }

                    Nothing ->
                        Update.returnAlive
                            { model
                                | timeSinceShowing = newTimeSinceShowing
                            }

        Dismiss ->
            if isDismissed model || model.timeSinceShowing < fadeInTime then
                Update.returnAlive model
            else
                Update.returnAlive { model | timeSinceDismissal = Just 0 }



-- VIEW


draw : Model -> Form
draw model =
    let
        gameOverText =
            Collage.group
                [ DefaultText.draw 36 "GAME OVER" |> Collage.moveY 30
                , DefaultText.draw 18
                    ("sector "
                        ++ toString model.sector
                        ++ " // score: "
                        ++ toString model.score
                    )
                    |> Collage.moveY -30
                ]
                |> DrawUtilities.alpha' (currentTextAlpha model)

        fadeAwayBackground =
            Collage.rect Bounds.width Bounds.height
                |> Collage.filled Color.black
                |> DrawUtilities.alpha' (currentFadeBackgroundAlpha model)
    in
        Collage.group
            [ gameOverText
            , fadeAwayBackground
            ]


currentTextAlpha : Model -> Float
currentTextAlpha model =
    min 1 (model.timeSinceShowing / fadeInTime)


currentFadeBackgroundAlpha : Model -> Float
currentFadeBackgroundAlpha model =
    Maybe.map (\x -> x / fadeOutTime) model.timeSinceDismissal
        |> Maybe.withDefault 0
