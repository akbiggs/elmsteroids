module Component.SegmentParticleRandom exposing (particles)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import List
import Random exposing (Generator)
import Random.Extra as RandomExtra
import Time exposing (Time)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Component.SegmentParticle as SegmentParticle
import Vector exposing (Vector)
import Segment exposing (Segment)


-- </editor-fold> END IMPORTS
-- <editor-fold> GENERATORS


particles : Vector -> List Segment -> Generator (Effects (List SegmentParticle.Model) SegmentParticle.Effect)
particles initialVelocity segments =
    List.map (particle initialVelocity) segments
        |> RandomExtra.together
        |> Random.map Effects.batch


particle : Vector -> Segment -> Generator (Effects SegmentParticle.Model SegmentParticle.Effect)
particle initialVelocity segment =
    let
        position =
            Segment.center segment

        relativeSegment =
            Segment.init (Vector.sub segment.a position)
                (Vector.sub segment.b position)
    in
        Random.map4
            (\velDir velMagnitude rotationVel lifetime ->
                SegmentParticle.init
                    { position = position
                    , velocity =
                        Vector.rotate velDir ( 0, velMagnitude )
                            |> Vector.add initialVelocity
                    , rotationVelocity = rotationVel
                    , segment = relativeSegment
                    , timeUntilDeath = lifetime
                    }
            )
            velocityDirection
            velocityMagnitude
            rotationVelocity
            timeUntilDeath


velocityDirection : Generator Float
velocityDirection =
    angle


velocityMagnitude : Generator Float
velocityMagnitude =
    Random.float 0 40


rotationVelocity : Generator Float
rotationVelocity =
    Random.float -1 1


timeUntilDeath : Generator Time
timeUntilDeath =
    Random.map ((*) Time.second) (Random.float 1 3)


angle : Generator Float
angle =
    Random.float 0 (pi * 2)



-- </editor-fold> END GENERATORS
