import Html exposing (Html)
import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Text exposing (fromString, style, link)
import Collage exposing (Form, collage, group, rect, filled, text, moveY, scale, alpha, scale)
import Element
import Color exposing (..)
import State exposing (..)
import DefaultText exposing (..)
import Bounds exposing (..)
import Stars exposing (Star)
import Player exposing (Player)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)
import KeyStates exposing (KeyStates)
import Ship
import Collisions exposing (..)
import Hud

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type Model
  = Uninitialized
  | Title TitleState
  | PreGame PreGameState
  | Game GameState

type alias TitleState =
  { stars : List Star
  , asteroids : List Asteroid
  , randomSeed : Seed
  }

type alias PreGameState =
  { sector : Int
  , score : Int
  , lives : Int
  , stars : List Star
  , asteroids : List Asteroid
  , bullets : List Bullet
  , segmentParticles : List SegmentParticle
  , randomSeed : Seed
  , stateTime : Float
  }

preGameLength : Float
preGameLength = 3

type alias GameState =
  { sector : Int
  , score : Int
  , lives : Int
  , stars : List Star
  , player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  , segmentParticles : List SegmentParticle
  , keys : KeyStates
  , randomSeed : Seed
  , fireTime : Float
  , stateTime : Float
  }

-- TODO: Better name?
invincibleLength : Float
invincibleLength = 3

init : (Model, Cmd Msg)
init = (Uninitialized, Cmd.none)

type Msg
  = Init Time
  | Tick Time
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (case model of
     Uninitialized ->
       case msg of
         Init time -> Title (initTitle time)
         _ -> model

     Title titleState ->
       (case msg of
          Tick timeDelta -> Title (tickTitle (inSeconds timeDelta) titleState)

          KeyPressed key ->
            let enter = 13
            in
              if key == enter then
                PreGame (initPreGame 1 0 3 titleState.stars titleState.asteroids [] [] titleState.randomSeed)
              else model

          _ -> model)

     PreGame preGameState ->
       (case msg of
          Tick timeDelta -> tickPreGame (inSeconds timeDelta) preGameState
          _ -> model)

     Game gameState ->
       (case msg of
          Init _ -> model
          Tick timeDelta -> tickGame (inSeconds timeDelta) gameState

          KeyPressed key -> Game { gameState | keys = KeyStates.pressed key gameState.keys }
          KeyReleased key -> Game { gameState | keys = KeyStates.released key gameState.keys })

  , Cmd.none)

initTitle : Time -> TitleState
initTitle time =
  let
    ms = inMilliseconds time |> floor
    ((stars, asteroids), randomSeed) =
      initialSeed ms |> initStarsAndAsteroids

  in
    { stars = stars
    , asteroids = asteroids
    , randomSeed = randomSeed
    }

initStarsAndAsteroids : State Seed (List Star, List Asteroid)
initStarsAndAsteroids =
  Stars.init >>= \stars ->
    Asteroids.init >>= \asteroids ->
      return (stars, asteroids)

tickTitle : Float -> TitleState -> TitleState
tickTitle timeDelta titleState =
  { titleState
    | stars = Stars.tick timeDelta titleState.stars
    , asteroids = Asteroids.tick timeDelta titleState.asteroids
  }

initPreGame : Int -> Int -> Int -> List Star -> List Asteroid -> List Bullet -> List SegmentParticle -> Seed -> PreGameState
initPreGame sector score lives stars asteroids bullets segmentParticles randomSeed =
  { sector = sector
  , score = score
  , stars = stars
  , lives = lives
  , asteroids = asteroids
  , bullets = bullets
  , segmentParticles = segmentParticles
  , randomSeed = randomSeed
  , stateTime = 0
  }

tickPreGame : Float -> PreGameState -> Model
tickPreGame timeDelta preGameState =
  let
    stars = Stars.tick timeDelta preGameState.stars
    asteroids = Asteroids.tick timeDelta preGameState.asteroids
    bullets = Bullets.tick timeDelta preGameState.bullets

    ((asteroids', bullets', segmentParticles, _, hitPlayer), randomSeed) =
      collide
        Nothing
        asteroids
        bullets
        preGameState.randomSeed

    segmentParticles' = SegmentParticles.tick timeDelta preGameState.segmentParticles ++ segmentParticles
  in
    if preGameState.stateTime >= preGameLength then
      Game
        (initGame
           preGameState.sector
           preGameState.score
           preGameState.lives
           stars
           asteroids'
           bullets'
           segmentParticles'
           randomSeed)
    else
      PreGame
        { preGameState
          | stars = stars
          , asteroids = asteroids'
          , bullets = bullets'
          , segmentParticles = segmentParticles'
          , randomSeed = randomSeed
          , stateTime = preGameState.stateTime + timeDelta
        }

initGame : Int -> Int -> Int -> List Star -> List Asteroid -> List Bullet -> List SegmentParticle -> Seed -> GameState
initGame sector score lives stars asteroids bullets segmentParticles randomSeed =
  { sector = sector
  , score = score
  , lives = lives
  , stars = stars
  , player =
      { position = (0, 0)
      , velocity = (0, 0)
      , rotation = 0
      }
  , asteroids = asteroids
  , bullets = bullets
  , segmentParticles = segmentParticles
  , keys =
      { left = False
      , right = False
      , up = False
      , down = False
      , space = False
      }
  , randomSeed = randomSeed
  , fireTime = 0
  , stateTime = 0
  }

tickGame : Float -> GameState -> Model
tickGame timeDelta gameState =
  let
    stars = Stars.tick timeDelta gameState.stars
    player = Player.tick timeDelta gameState.keys gameState.player
    asteroids = Asteroids.tick timeDelta gameState.asteroids
    bullets = Bullets.tick timeDelta gameState.bullets
    (bullets', fireTime) =
      if gameState.keys.space && gameState.fireTime >= 0 then
        (Bullets.fire gameState.player bullets, -0.5)
      else (bullets, gameState.fireTime + timeDelta)

    ((asteroids', bullets'', segmentParticles, score, hitPlayer), randomSeed) =
      collide
        (if gameState.stateTime < invincibleLength then Nothing else Just player)
        asteroids
        bullets'
        gameState.randomSeed

    score' = gameState.score + score
    segmentParticles' = SegmentParticles.tick timeDelta gameState.segmentParticles ++ segmentParticles
  in
    if hitPlayer then
      PreGame
        (initPreGame
           gameState.sector
           score'
           (gameState.lives - 1) -- TODO: Game over
           stars
           asteroids'
           bullets''
           segmentParticles'
           randomSeed)
    else
      case asteroids' of
        [] ->
          let
            ((stars', asteroids''), randomSeed') =
              initStarsAndAsteroids randomSeed
          in
            PreGame
              (initPreGame
                 (gameState.sector + 1)
                 score'
                 gameState.lives
                 stars'
                 asteroids''
                 []
                 []
                 randomSeed')
        _ ->
          Game
            { gameState
              | score = score'
              , stars = stars
              , player = player
              , asteroids = asteroids'
              , bullets = bullets''
              , segmentParticles = segmentParticles'
              , randomSeed = randomSeed
              , fireTime = fireTime
              , stateTime = gameState.stateTime + timeDelta
            }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Uninitialized -> times Init
    Title _ ->
      Sub.batch
           [ diffs Tick

           , downs KeyPressed
           ]
    PreGame _ -> diffs Tick
    Game _ ->
      Sub.batch
           [ diffs Tick

           , downs KeyPressed
           , ups KeyReleased
           ]

view : Model -> Html Msg
view model =
  case model of
    Uninitialized -> Html.text "Initializing..."

    Title titleState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Stars.draw titleState.stars
        , Asteroids.draw titleState.asteroids
        , group
            [ defaultText 40 "elmsteroids" |> moveY 50
            , defaultText 16 "github.com/yupferris // 2016" |> moveY -30
            , defaultText 14 "press enter/return to begin" |> moveY -50
            ]
        ]
        |> Element.toHtml

    PreGame preGameState ->
      let
        animAmt = preGameState.stateTime / preGameLength
        animAmt' = 1 - animAmt
      in
        collage
          (floor width) (floor height)
          [ rect width height |> filled black
          , Stars.draw preGameState.stars
          , Asteroids.draw preGameState.asteroids
          , Ship.draw (0, 0) ((animAmt' ^ 3) * 8) |> scale (1 + (animAmt' ^ 2) * 2) |> alpha animAmt
          , Bullets.draw preGameState.bullets
          , SegmentParticles.draw preGameState.segmentParticles
          , group
              [ defaultText 26 ("warping to sector " ++ toString preGameState.sector) |> moveY 50
              , defaultText 18 ("score: " ++ toString preGameState.score ++ " // " ++ Hud.livesText preGameState.lives) |> moveY -30
              ]
              |> alpha (min preGameState.stateTime (preGameLength - preGameState.stateTime |> min 1 |> max 0))
              |> scale (1 + (animAmt' * 0.2))
          ]
          |> Element.toHtml

    Game gameState ->
      collage
        (floor width) (floor height)
        [ rect width height |> filled black
        , Stars.draw gameState.stars
        , Asteroids.draw gameState.asteroids
        , let
            a =
              if gameState.stateTime < invincibleLength then
                cos (gameState.stateTime * 50) * 0.4 + 0.6
              else 1
          in Player.draw gameState.player |> alpha a
        , Bullets.draw gameState.bullets
        , SegmentParticles.draw gameState.segmentParticles
        , Hud.draw gameState.sector gameState.score gameState.lives |> alpha (min gameState.stateTime 1)
        ]
        |> Element.toHtml
