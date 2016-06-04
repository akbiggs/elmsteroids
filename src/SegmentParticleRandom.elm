module SegmentParticleRandom exposing (particles)

-- <editor-fold> IMPORTS

-- EXTERNAL IMPORTS

import List
import Random exposing (Generator)
import Random.Extra as RandomExtra

-- LOCAL IMPORTS

import SegmentParticle
import Vector exposing (Vector)
import Segment exposing (Segment)

-- </editor-fold> END IMPORTS

-- <editor-fold> GENERATORS

particles : Vector -> List Segment -> Generator (List SegmentParticle.Model)
particles initialVelocity segments =
  List.map (particle initialVelocity) segments
    |> RandomExtra.together

particle : Vector -> Segment -> Generator (SegmentParticle.Model)
particle initialVelocity segment =
  let
    position =
      Segment.center segment

    relativeSegment =
      Segment.init
        (Vector.sub segment.a position)
        (Vector.sub segment.b position)
  in
    Random.map4 (\velDir velMagnitude rotationVel lifetime ->
      { position = position
      , velocity =
          Vector.rotate velDir (0, velMagnitude)
            |> Vector.add initialVelocity
      , rotation = 0
      , rotationVelocity = rotationVel
      , segment = relativeSegment
      , timeUntilDeath = lifetime
      }
    ) velocityDirection velocityMagnitude rotationVelocity timeUntilDeath

velocityDirection : Generator Float
velocityDirection =
  angle

velocityMagnitude : Generator Float
velocityMagnitude =
  Random.float 0 40

rotationVelocity : Generator Float
rotationVelocity =
  Random.float -1 1

timeUntilDeath : Generator Float
timeUntilDeath =
  Random.float 1 3

angle : Generator Float
angle =
  Random.float 0 (pi * 2)

-- </editor-fold> END GENERATORS
