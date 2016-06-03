module ComponentUtils exposing (updateMaybe, updateIf, updateGroup, mapUpdate, foldlUpdates, drawMaybe, drawGroup)

-- <editor-fold> IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Collage exposing (Form, group)
import Element
import Tuple2

-- </editor-fold> END IMPORTS

-- <editor-fold> FUNCTIONS

updateMaybe : (a -> (Maybe a, List effect)) -> Maybe a -> (Maybe a, List effect)
updateMaybe updateFn maybeObj =
  case maybeObj of
    Just obj ->
      updateFn obj

    Nothing ->
      (Nothing, [])

updateIf : Bool -> (a -> (Maybe a, List effect)) -> a -> (Maybe a, List effect)
updateIf cond updateFn obj =
  if cond then
    updateFn obj
  else
    (Just obj, [])

mapUpdate : (a -> (Maybe a, List effect)) -> (Maybe a, List effect) -> (Maybe a, List effect)
mapUpdate updateFn (maybeObj, effects) =
  let
    (updatedObj, newEffects) =
      updateMaybe updateFn maybeObj
  in
    (updatedObj, effects ++ newEffects)

foldlUpdates : List (a -> (Maybe a, List effect)) -> a -> (Maybe a, List effect)
foldlUpdates updateFns obj =
  List.foldl mapUpdate (Just obj, []) updateFns

updateGroup : (a -> (b, List effect)) -> List a -> (List b, List effect)
updateGroup updateFn xs =
  List.map updateFn xs
    |> List.unzip
    |> Tuple2.mapSnd List.concat

drawMaybe : (a -> Form) -> Maybe a -> Form
drawMaybe drawFn maybeObj =
  case maybeObj of
    Just obj ->
      drawFn obj

    Nothing ->
      Collage.toForm Element.empty

drawGroup : (a -> Form) -> List a -> Form
drawGroup drawFn =
  map drawFn >> group

-- </editor-fold> END FUNCTIONS
