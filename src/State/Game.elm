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

type StateEffect
    = TitleEffect State.Title.Effect
    | PlayingEffect State.Playng.Effect
    | GameOverEffect State.GameOver.Effect

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

        ( updatedModel, effects ) =
            case msg of
                SecondsElapsed dt ->
                    let
                        ( updatedAsteroids, asteroidEffects ) =
                            List.map (Asteroid.update (Asteroid.SecondsElapsed dtSeconds)) model.asteroids
                                |> Update.filterAlive
                                |> Effects.batch

                        ( updatedSegmentParticles, segmentParticleEffects ) =
                            List.map (SegmentParticle.update (SegmentParticle.SecondsElapsed dtSeconds)) model.segmentParticles
                                |> Update.filterAlive
                                |> Effects.batch

                        ( updatedStars, starEffects ) =
                            List.map (Star.update (Star.SecondsElapsed dtSeconds)) model.stars
                                |> Update.filterAlive
                                |> Effects.batch
                    in
                        Effects.return
                            { model
                                | asteroids = updatedAsteroids
                                , segmentParticles = updatedSegmentParticles
                                , stars = updatedStars
                            }
                            `Effects.andThen` Effects.handle handleAsteroidEffect asteroidEffects
                            `Effects.andThen` Effects.handle handleSegmentParticleEffect segmentParticleEffects
                            `Effects.andThen` Effects.handle handleStarEffect starEffects
                            `Effects.andThen` resolveCollisions

                HandleInput keyboard ->
                    -- nothing needs the input besides the current state of the game
                    Effects.return model

                SpawnAsteroids asteroids ->
                    Effects.return { model | asteroids = model.asteroids ++ asteroids }

                SpawnSegmentParticles segmentParticles ->
                    Effects.return { model | segmentParticles = model.segmentParticles ++ segmentParticles }

                SpawnStars stars ->
                    Effects.return { model | stars = model.stars ++ stars }
    in
        Effects.init { model | state = updatedState } effects
            `Effects.andThen` Effects.handle handleStateEffect stateEffects


handleAsteroidEffect : Effects.Handler Asteroid.Effect Model Effect
handleAsteroidEffect effect model =
    case effect of
        Asteroid.SpawnSegmentParticles { velocity, segments } ->
            Effects.return model
                |> Effects.add [ GenerateSegmentParticles <| SegmentParticleRandom.particles velocity segments ]

        Asteroid.SpawnSplitAsteroids { parentScale, position } ->
            let
                childScale =
                    parentScale - 1
            in
                Effects.return model
                    |> Effects.add [ GenerateAsteroids <| AsteroidRandom.asteroidGroupWithScaleAt childScale position ]

        Asteroid.IncreaseScore amount ->
            update (IncreaseScore amount) model


handleSegmentParticleEffect : SegmentParticle.Effect -> Model -> ( Model, Cmd Msg )
handleSegmentParticleEffect =
    Effects.ignoreUnused


handleStarEffect : Star.Effect -> Model -> ( Model, Cmd Msg )
handleStarEffect =
    Effects.ignoreUnused

handleStateEffect :



-- </editor-fold>
