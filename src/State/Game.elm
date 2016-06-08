module State.Game exposing (Model, Msg(..))

-- <editor-fold> IMPORTS
--EXTERNAL IMPORTS

import Keyboard.Extra as Keyboard
import Random


-- LOCAL IMPORTS

import Component.Bullet as Bullet
import Component.Asteroid as Asteroid
import Component.AsteroidRandom as AsteroidRandom
import Component.SegmentParticle as SegmentParticle
import Component.SegmentParticleRandom as SegmentParticleRandom
import Component.Star as Star
import Component.StarRandom as StarRandom
import State.Title
import State.Playing
import State.GameOver


-- </editor-fold> END IMPORTS
-- <editor-fold> MODEL


type State
    = Title State.Title.Model
    | Playing State.Playing.Model
    | GameOver State.GameOver.Model


type alias Model =
    { state : State
    , asteroids : List Asteroid.Model
    , segmentParticles : List SegmentParticle.Model
    , stars : List Star.Model
    }


init : ( Model, List Effect )
init =
    let
        effects =
            [ GenerateAsteroids AsteroidRandom.asteroidGroup
            , GenerateStars StarRandom.starGroup
            ]

        initialModel =
            { state = Title
            , bullets = []
            , asteroids = []
            , segmentParticles = []
            , stars = []
            }
    in
        ( initialModel, effects )



-- </editor-fold>
-- <editor-fold> UPDATE


type Msg
    = SecondsElapsed Float
    | HandleInput Keyboard.Model
    | SpawnedAsteroids (List Asteroid.Model)
    | SpawnedSegmentParticles (List SegmentParticle.Model)
    | SpawnedStars (List Star.Model)


type Effect
    = PlaySound String
    | GenerateAsteroids (Generator (List Asteroid.Model))
    | GenerateSegmentParticles (Generator (List SegmentParticle.Model))
    | GenerateStars (Generator (List Star.Model))


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    let
        ( updatedState, stateEffects ) =
            updateState msg model.state
    in
        case msg of
            SecondsElapsed dt ->
                let
                    ( updatedAsteroids, asteroidEffects ) =
                        Update.runOnGroup (Asteroid.update (Asteroid.SecondsElapsed dtSeconds)) model.asteroids
                            |> Update.filterAliveObjects

                    ( updatedSegmentParticles, segmentParticleEffects ) =
                        Update.runOnGroup (SegmentParticle.update (SegmentParticle.SecondsElapsed dtSeconds)) model.segmentParticles
                            |> Update.filterAliveObjects

                    ( updatedStars, starEffects ) =
                        Update.runOnGroup (Star.update (Star.SecondsElapsed dtSeconds)) model.stars
                            |> Update.filterAliveObjects
                in
                    Effects.chain
                        { model
                            | asteroids = updatedAsteroids
                            , segmentParticles = updatedSegmentParticles
                            , stars = updatedStars
                        }
                        `Effects.andThen` Effects.process processAsteroidEffect asteroidEffects
                        `Effects.andThen` Effects.process processSegmentParticleEffect segmentParticleEffects
                        `Effects.andThen` Effects.process processStarEffect starEffects
                        `Effects.andThen` resolveCollisions

            HandleInput keyboard ->
                -- nothing needs the input besides the current state of the game
                ( model, [] )

            SpawnAsteroids asteroids ->
                ( { model | asteroids = model.asteroids ++ asteroids }, [] )

            SpawnSegmentParticles segmentParticles ->
                ( { model | segmentParticles = model.segmentParticles ++ segmentParticles }, [] )

            SpawnStars stars ->
                ( { model | stars = model.stars ++ stars }, [] )


processAsteroidEffect : Asteroid.Effect -> Model -> ( Model, Cmd Msg )
processAsteroidEffect effect model =
    case effect of
        Asteroid.SpawnSegmentParticles { velocity, segments } ->
            ( model, [ GenerateSegmentParticles <| SegmentParticleRandom.particles velocity segments ] )

        Asteroid.SpawnSplitAsteroids { parentScale, position } ->
            let
                childScale =
                    parentScale - 1
            in
                ( model, [ GenerateAsteroids <| AsteroidRandom.asteroidGroupWithScaleAt childScale position ] )

        Asteroid.IncreaseScore amount ->
            update (IncreaseScore amount) model


processSegmentParticleEffect : SegmentParticle.Effect -> Model -> ( Model, Cmd Msg )
processSegmentParticleEffect =
    ignoreUnusedEffect


processStarEffect : Star.Effect -> Model -> ( Model, Cmd Msg )
processStarEffect =
    ignoreUnusedEffect



-- </editor-fold>
