module Component.AsteroidRandom exposing (asteroidGroup, asteroidGroupWithScaleAt)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Random exposing (Generator, andThen)
import Random.Extra
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Component.Asteroid as Asteroid exposing (AsteroidSize)
import Bounds
import Vector exposing (Vector)


-- </editor-fold>


numAsteroids : Generator Int
numAsteroids =
    Random.int 2 3


asteroidGroup : Generator (Effects (List Asteroid.Model) Asteroid.Effect)
asteroidGroup =
    numAsteroids
        `andThen` \n ->
                    Random.map Effects.batch (Random.list n asteroid)


asteroidGroupWithScaleAt : Int -> Vector -> Generator (Effects (List Asteroid.Model) Asteroid.Effect)
asteroidGroupWithScaleAt scale position =
    numAsteroids
        `andThen` \n ->
                    size scale scale
                        `andThen` \s ->
                                    Random.map Effects.batch (Random.list n (asteroidWithSizeAt s position))


asteroid : Generator (Effects Asteroid.Model Asteroid.Effect)
asteroid =
    size 4 5
        `andThen` asteroidWithSize


asteroidWithSize : AsteroidSize -> Generator (Effects Asteroid.Model Asteroid.Effect)
asteroidWithSize ({ radius } as size) =
    positionInSafeZone radius
        `andThen` asteroidWithSizeAt size


asteroidWithSizeAt : AsteroidSize -> Vector -> Generator (Effects Asteroid.Model Asteroid.Effect)
asteroidWithSizeAt { scale, radius } position =
    Random.map4
        (\velocity rotation rotationVel points ->
            Asteroid.init
                { position = position
                , velocity = velocity
                , rotation = rotation
                , rotationVelocity = rotationVel
                , scale = scale
                , points = points
                }
        )
        (velocity scale)
        angle
        rotationVelocity
        (asteroidPoints radius)


position : Generator Vector
position =
    let
        xGen =
            Random.float Bounds.left Bounds.right

        yGen =
            Random.float Bounds.bottom Bounds.top
    in
        Random.map2 Vector.init xGen yGen


velocity : Int -> Generator Vector
velocity scale =
    Random.map2
        (\velDir velMagnitude ->
            Vector.rotate velDir ( 0, velMagnitude )
        )
        angle
        (Random.float 60 (180 / toFloat (scale ^ 2)))


positionInSafeZone : Float -> Generator Vector
positionInSafeZone objRadius =
    let
        minOffsetFromCenter =
            Bounds.safeZoneSize + objRadius

        pushIntoSafeZone p =
            if Vector.length p < minOffsetFromCenter then
                p |> Vector.normalize |> Vector.mulS minOffsetFromCenter
            else
                p
    in
        Random.map pushIntoSafeZone position


angle : Generator Float
angle =
    Random.float 0 (pi * 2)


rotationVelocity : Generator Float
rotationVelocity =
    Random.float -0.5 0.5


size : Int -> Int -> Generator AsteroidSize
size minScale maxScale =
    Random.int minScale maxScale
        `andThen` sizeWithScale


sizeWithScale : Int -> Generator AsteroidSize
sizeWithScale scale =
    let
        idealRadius =
            toFloat scale * 16.0

        minRadius =
            idealRadius * 0.95

        maxRadius =
            idealRadius * 1.05
    in
        Random.map
            (\radius ->
                { scale = scale
                , radius = radius
                }
            )
            (Random.float minRadius maxRadius)


asteroidPoints : Float -> Generator (List Vector)
asteroidPoints radius =
    let
        minRadius =
            radius * 0.8

        maxRadius =
            radius * 1.2
    in
        Random.int 10 16
            `andThen` \n ->
                        asteroidPoints' (pi * 2.0 / (toFloat n)) minRadius maxRadius n


asteroidPoints' : Float -> Float -> Float -> Int -> Generator (List Vector)
asteroidPoints' segAngleDelta minRadius maxRadius n =
    -- coded using explicit recursion because each point is dependent on the
    -- current value of n to determine its angle offset
    if n == 0 then
        Random.Extra.constant []
    else
        let
            angleOffset =
                toFloat n * segAngleDelta

            initPoint angle radius =
                ( cos angle * radius, sin angle * radius )

            minAngle =
                -segAngleDelta * 0.3

            maxAngle =
                segAngleDelta * 0.3
        in
            Random.pair (Random.float minAngle maxAngle) (Random.float minRadius maxRadius)
                `andThen` \( angle, radius ) ->
                            Random.map ((::) (initPoint (angle + angleOffset) radius))
                                (asteroidPoints' segAngleDelta minRadius maxRadius (n - 1))
