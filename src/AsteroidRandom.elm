module AsteroidRandom exposing (asteroidGroup, asteroidGroupWithScaleAt)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Random exposing (Generator, andThen)
import Random.Extra


-- LOCAL IMPORTS

import Asteroid exposing (AsteroidSize)
import Bounds
import Vector exposing (Vector)


-- </editor-fold>


numAsteroids : Generator Int
numAsteroids =
    Random.int 2 3


asteroidGroup : Generator (List Asteroid.Model)
asteroidGroup =
    numAsteroids
        `andThen` \n ->
                    Random.list n asteroid


asteroidGroupWithScaleAt : Int -> Vector -> Generator (List Asteroid.Model)
asteroidGroupWithScaleAt scale position =
    numAsteroids
        `andThen` \n ->
                    size scale scale
                        `andThen` \s ->
                                    Random.list n (asteroidWithSizeAt s position)


asteroid : Generator Asteroid.Model
asteroid =
    size 4 5
        `andThen` asteroidWithSize


asteroidWithSize : AsteroidSize -> Generator Asteroid.Model
asteroidWithSize (( scale, radius ) as size) =
    positionInSafeZone radius
        `andThen` asteroidWithSizeAt size


asteroidWithSizeAt : AsteroidSize -> Vector -> Generator Asteroid.Model
asteroidWithSizeAt ( scale, radius ) position =
    Random.map5
        (\velDir rotation velMagnitude rotationVel points ->
            { position = position
            , velocity = Vector.rotate velDir ( 0, velMagnitude )
            , rotation = rotation
            , rotationVelocity = rotationVel
            , scale = scale
            , points = points
            }
        )
        angle
        angle
        (velocityMagnitude scale)
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


velocityMagnitude : Int -> Generator Float
velocityMagnitude objScale =
    Random.float 60 (180 / toFloat (objScale ^ 2))


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
        Random.map (\radius -> ( scale, radius )) (Random.float minRadius maxRadius)


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
