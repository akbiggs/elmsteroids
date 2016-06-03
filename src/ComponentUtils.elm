module ComponentUtils exposing (updateMaybe, updateIf, updateGroup, mapUpdate, foldlUpdates, drawMaybe, drawGroup)

-- <editor-fold> IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Collage exposing (Form, group)
import Element

-- </editor-fold>

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
  List.foldl
    mapUpdate
    (Just obj, [])
    updateFns

updateGroup : (a -> (Maybe a, List effect)) -> List a -> (List a, List effect)
updateGroup updateFn xs =
  let
    (updatedObjects, effectLists) =
      map updateFn xs |> unzip

    aliveObjects =
      filterMap identity updatedObjects

    effects =
      List.foldl (++) [] effectLists
  in
    (aliveObjects, effects)

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

-- </editor-fold>
