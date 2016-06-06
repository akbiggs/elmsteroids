module StarRandom exposing (starGroup)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Random exposing (Generator, andThen)


-- LOCAL IMPORTS

import Star
import Bounds
import Vector exposing (Vector)


-- </editor-fold> END IMPORTS
-- <editor-fold> GENERATORS


starGroup : Generator (List Star.Model)
starGroup =
    Random.int 80 100
        `andThen` \n ->
                    Random.list n star


star : Generator Star.Model
star =
    Random.map3
        (\posValue phaseValue freqValue ->
            { position = posValue
            , blinkPhase = phaseValue
            , blinkFrequency = freqValue
            }
        )
        position
        phase
        frequency


position : Generator Vector
position =
    Random.pair (Random.float Bounds.left Bounds.right)
        (Random.float Bounds.bottom Bounds.top)


phase : Generator Float
phase =
    Random.float 0 (pi * 2)


frequency : Generator Float
frequency =
    Random.float 0 2



-- </editor-fold> END GENERATORS
