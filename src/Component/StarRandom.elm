module Component.StarRandom exposing (starGroup)

-- <editor-fold> IMPORTS
-- EXTERNAL IMPORTS

import Random exposing (Generator, andThen)
import Effects exposing (Effects)


-- LOCAL IMPORTS

import Component.Star as Star
import Bounds
import Vector exposing (Vector)


-- </editor-fold> END IMPORTS
-- <editor-fold> GENERATORS


starGroup : Generator (Effects (List Star.Model) Star.Effect)
starGroup =
    Random.int 80 100
        `andThen` \n ->
                    Random.map Effects.batch (Random.list n star)


star : Generator (Effects Star.Model Star.Effect)
star =
    Random.map3
        (\posValue phaseValue freqValue ->
            Star.init
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
