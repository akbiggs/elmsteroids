module State.Playing.PlayerAlive exposing (Model, Msg(..), Effect(..), init, update, updatePlayer, draw)

-- EXTERNAL IMPORTS

import Random exposing (Generator)
import Keyboard.Extra as Keyboard


-- LOCAL IMPORTS

import Component.Bullet as Bullet
import Component.SegmentParticle as SegmentParticle
import Component.Player as Player
import Vector exposing (Vector)
import Update
import Effects exposing (Effects)


type alias Model =
    { player : Maybe Player.Model }


type alias InitArgs =
    { playerPosition : Vector }


init : InitArgs -> ( Model, List Effect )
init args =
    let
        ( player, playerEffects ) =
            Player.init args.playerPosition

        initialModel =
          { player = Just player }
    in
        (initialModel, [])
            `Effects.andThen` Effects.process processPlayerEffect playerEffects


type Msg
    = SecondsElapsed Float
    | HandleInput Keyboard.Model


type Effect
    = PlaySound
    | SpawnBullets (List Bullet.Model)
    | GenerateSegmentParticles (Generator (List SegmentParticle.Model))
    | DecrementNumLives


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                ( updatedPlayer, playerEffects ) =
                    Player.update (Player.SecondsElapsed dt) model.player
            in
                updatePlayer updatedPlayer model
                    `Effects.andThen` Effects.process processPlayerEffect playerEffects

        HandleInput keyboard ->
            let
                ( updatedPlayer, playerEffects ) =
                    Update.chain model.player
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowUp model.keyboard) (Player.update Player.Accelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowDown model.keyboard) (Player.update Player.Decelerate)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowLeft model.keyboard) (Player.update Player.RotateLeft)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.ArrowRight model.keyboard) (Player.update Player.RotateRight)
                        `Update.andThen` Update.runIf (Keyboard.isPressed Keyboard.Space model.keyboard) (Player.update Player.FireBullet)
            in
                updatePlayer updatedPlayer model
                    `Effects.andThen` Effects.process processPlayerEffect playerEffects


updatePlayer : Maybe Player -> Model -> (Model, List Effects)
updatePlayer maybePlayer model =
  let
    effects =
      case maybePlayer of
        Just x ->
          []

        Nothing ->
          [DecrementNumLives]

    case maybePlayer of
        Just player ->
            ({ model | player = player }, [])

        Nothing ->
            ({ model | player })

processPlayerEffect : Player.Effect -> Maybe Model -> ( Maybe Model, List Effect )
processPlayerEffect effect model =
    case effect of
        Player.PlaySound filename ->
            ( model, [ PlaySound filename ] )

        Player.SpawnBullet bullet ->
            ( model, [ SpawnBullets [ bullet ] ] )

        Player.GenerateSegmentParticles generator ->
            ( model, [ GenerateSegmentParticles generator ] )
