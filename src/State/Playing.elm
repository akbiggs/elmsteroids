module State.Playing exposing (Model, Msg(..), Effect(..), init, update, draw)

-- EXTERNAL IMPORTS

import Keyboard.Extra as Keyboard
import Random exposing (Generator)


-- LOCAL IMPORTS

import Component.Stats as Stats
import Component.Player as Player
import Component.Bullet as Bullet
import Component.Update as Update
import Component.SegmentParticle as SegmentParticle
import State.Playing.PlayerAlive as PlayerAlive
import State.Playing.PlayerDead as PlayerDead
import State.Playing.PlayerSpawning as PlayerSpawning
import DrawUtilities
import Effects


type State
    = PlayerAlive PlayerAlive.Model
    | PlayerDead PlayerDead.Model
    | PlayerSpawning PlayerSpawning.Model


type alias Model =
    { state : State
    , stats : Stats.Model
    , bullets : List Bullet.Model
    }


init : Model
init =
    let
        ( stats, statsEffects ) =
            Stats.init { numLives = 3 }

        ( playerSpawning, playerSpawningEffects ) =
            PlayerSpawning.init

        initialModel =
            { state = playerSpawning
            , stats = stats
            , bullets = []
            }
    in
        ( initialModel, [] )
            `Effect.andThen` Effect.process processStatsEffect statsEffects
            `Effect.andThen` Effect.process processPlayerSpawningEffect playerSpawningEffects


type Msg
    = SecondsElapsed Float
    | HandleInput Keyboard.Model


type Effect
    = GameOver Stats.Model
    | GenerateSegmentParticles (Generator (List SegmentParticle.Model))
    | SpawnBullets (List Bullet.Model)


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        SecondsElapsed dt ->
            let
                ( updatedBullets, bulletEffects ) =
                    Update.runOnGroup (Bullet.update (Bullet.SecondsElapsed dtSeconds)) model.bullets
                        |> Update.filterAliveObjects

                updatedModel =
                    { model
                        | player = updatedPlayer
                        , bullets = updatedBullets
                    }
            in
                ( updatedModel, [] )
                    `Effects.andThen` Effects.process processBulletEffect bulletEffects
                    `Effects.andThen` Effects.process processPlayerEffect playerEffects


processBulletEffect : Bullet.Effect -> Model -> ( Model, List Effect )
processBulletEffect =
    Effects.ignoreUnused


handleCollisions : Model -> ( Model, List Effect )
handleCollisions model =
    case model.state of
        PlayerAlive state ->
            let
                objects =
                    { player = player
                    , bullets = model.bullets
                    , asteroids = model.asteroids
                    }

                ( updatedObjects, collisionEffects ) =
                    Collisions.handleCollisions objects

                updatedGame =
                    { model
                        | player = updatedObjects.player
                        , bullets = updatedObjects.bullets
                        , asteroids = updatedObjects.asteroids
                    }
            in
                ( updatedGame, [] )
                    `Effects.andThen` Effects.process processCollisionEffect collisionEffects

        _ ->
            ( model, [] )


draw : Model -> Form
draw =
    DrawUtilities.emptyForm
