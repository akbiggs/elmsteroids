import Html.App
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Collage exposing (..)
import Element
import Color exposing (..)
import Bounds exposing (..)
import Player exposing (Player)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)
import KeyStates exposing (KeyStates)

main =
  Html.App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model =
  { player : Player
  , asteroids : List Asteroid
  , bullets : List Bullet
  , keys : KeyStates
  , randomSeed : Seed
  }

init =
  let
    -- TODO: Different seed each time
    randomSeed = initialSeed 12345
    (asteroids, randomSeed') = Asteroids.init randomSeed

  in
    ({ player =
         { position = (0, 0)
         , velocity = (0, 0)
         , rotation = 0
         }
     , asteroids = asteroids
     , bullets = []
     , keys =
         { left = False
         , right = False
         , up = False
         , down = False
         , spaceTapped = False
         }
     , randomSeed = randomSeed'
     }, Cmd.none)

type Msg
  = Tick Float -- Time value is always in seconds
  | KeyPressed KeyCode
  | KeyReleased KeyCode

update msg model =
  (case msg of
     Tick timeDelta ->
       { model
       | player = Player.tick timeDelta model.keys model.player
       , asteroids = Asteroids.tick timeDelta model.asteroids
       , bullets = Bullets.tick timeDelta model.keys model.player model.bullets
       , keys = KeyStates.tick model.keys
       }

     KeyPressed key -> { model | keys = KeyStates.pressed key model.keys }
     KeyReleased key -> { model | keys = KeyStates.released key model.keys }
  , Cmd.none)

subscriptions _ =
  Sub.batch
    [ diffs (inSeconds >> Tick)

    , downs KeyPressed
    , ups KeyReleased
    ]

view model =
  collage
    width height
    [ rect width height |> filled black
    , Player.draw model.player
    , Asteroids.draw model.asteroids
    , Bullets.draw model.bullets
    ]
  |> Element.toHtml
