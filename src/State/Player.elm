module State.Player exposing (Model(..), Msg(..), Effect(..), init, update, draw)

-- EXTERNAL IMPORTS

import Effects exposing (Effects)
import Time exposing (Time)
import Keyboard.Extra as Keyboard
import Game.Update as Update
import Collage exposing (Form)


-- LOCAL IMPORTS

import Component.Player as Player
import Component.PlayerSpawnAnimation as PlayerSpawnAnimation
import Vector exposing (Vector)
import DrawUtilities


type Model
    = Alive Player.Model
    | Spawning PlayerSpawnAnimation.Model
    | Dead Time


init : Effects Model Effect
init =
    Effects.return (Dead 0)


timeBetweenDeathAndRespawn : Time
timeBetweenDeathAndRespawn =
    1.5 * Time.second



-- UPDATE


type Msg
    = HandleInput Keyboard.Model
    | SecondsElapsed Float
    | RespawnPlayer Vector
    | UpdatePlayerStatus (Maybe Player.Model)


type Effect
    = AliveEffect Player.Effect
    | SpawningEffect PlayerSpawnAnimation.Effect
    | DecrementNumLives


update : Msg -> Model -> Effects Model Effect
update msg model =
    case model of
        Alive player ->
            aliveUpdate msg player

        Spawning playerSpawnAnimation ->
            spawningUpdate msg playerSpawnAnimation

        Dead timeSinceDeath ->
            deadUpdate msg timeSinceDeath


aliveUpdate : Msg -> Player.Model -> Effects Model Effect
aliveUpdate msg player =
    let
        ( maybePlayer, playerEffects ) =
            case msg of
                HandleInput keyboard ->
                    Update.returnAlive player
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowUp keyboard)
                                            (Player.update Player.Accelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowDown keyboard)
                                            (Player.update Player.Decelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowLeft keyboard)
                                            (Player.update Player.RotateLeft)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowRight keyboard)
                                            (Player.update Player.RotateRight)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.Space keyboard)
                                            (Player.update Player.FireBullet)

                SecondsElapsed dt ->
                    Player.update (Player.SecondsElapsed dt) player

                UpdatePlayerStatus maybePlayer ->
                    Update.returnMaybe maybePlayer

                _ ->
                    Update.returnAlive player

        newState =
            case maybePlayer of
                Just player ->
                    Alive player

                Nothing ->
                    Dead 0

        -- Maybe.map Alive maybePlayer
        --     |> Maybe.withDefault (Dead 0)
    in
        Effects.init (Debug.log "state" newState) (List.map AliveEffect playerEffects)


spawningUpdate : Msg -> PlayerSpawnAnimation.Model -> Effects Model Effect
spawningUpdate msg playerSpawnAnimation =
    let
        ( maybePlayerSpawnAnimation, playerSpawnAnimationEffects ) =
            case msg of
                SecondsElapsed dt ->
                    PlayerSpawnAnimation.update (PlayerSpawnAnimation.SecondsElapsed dt) playerSpawnAnimation

                _ ->
                    Update.returnAlive playerSpawnAnimation

        ( newState, playerEffects ) =
            case maybePlayerSpawnAnimation of
                Just updatedPlayerSpawnAnimation ->
                    Effects.return (Spawning updatedPlayerSpawnAnimation)

                Nothing ->
                    Player.init { position = playerSpawnAnimation.position }
                        |> Effects.mapOverValue Alive
    in
        Effects.return newState
            |> Effects.add
                (List.concat
                    [ List.map AliveEffect playerEffects
                    , List.map SpawningEffect playerSpawnAnimationEffects
                    ]
                )


deadUpdate : Msg -> Time -> Effects Model Effect
deadUpdate msg timeSinceDeath =
    case msg of
        SecondsElapsed dt ->
            let
                newTimeSinceDeath =
                    timeSinceDeath + dt * Time.second

                isDone =
                    newTimeSinceDeath > timeBetweenDeathAndRespawn
            in
                Effects.return (Dead newTimeSinceDeath)
                    |> Effects.addIf isDone [ DecrementNumLives ]

        RespawnPlayer position ->
            PlayerSpawnAnimation.init { position = position }
                |> Effects.mapOverValue Spawning
                |> Effects.mapOverEffects SpawningEffect

        _ ->
            Effects.return (Dead timeSinceDeath)



-- VIEW


draw : Model -> Form
draw model =
    case model of
        Alive player ->
            Player.draw player

        Spawning playerSpawnAnimation ->
            PlayerSpawnAnimation.draw playerSpawnAnimation

        Dead _ ->
            DrawUtilities.emptyForm
