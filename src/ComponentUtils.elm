module ComponentUtils exposing (updateMaybe, updateIf, updateGroup, mapUpdate, foldlUpdates, drawMaybe, drawGroup)

-- <editor-fold> IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Collage exposing (Form, group)
import Element

-- </editor-fold>

-- <editor-fold> FUNCTIONS

updateMaybe : (a -> (Maybe a, Cmd b)) -> Maybe a -> (Maybe a, Cmd b)
updateMaybe updateFn maybeObj =
  case maybeObj of
    Just obj ->
      updateFn obj

    Nothing ->
      Nothing ! []

updateIf : Bool -> (a -> (Maybe a, Cmd b)) -> a -> (Maybe a, Cmd b)
updateIf b updateFn obj =
  if b then
    updateFn obj
  else
    Just obj ! []

mapUpdate : (a -> (Maybe a, Cmd b)) -> (Maybe a, Cmd b) -> (Maybe a, Cmd b)
mapUpdate updateFn (maybeObj, cmd) =
  let
    (obj', nextCmd) =
      updateMaybe updateFn maybeObj
  in
    obj' ! [cmd, nextCmd]

foldlUpdates : List (a -> (Maybe a, Cmd b)) -> a -> (Maybe a, Cmd b)
foldlUpdates updateFns obj =
  List.foldl
    mapUpdate
    (Just obj ! [])
    updateFns

updateGroup : (a -> (Maybe a, Cmd b)) -> List a -> (List a, Cmd b)
updateGroup updateFn xs =
  let
    (updatedObjects, effects) = map updateFn xs |> unzip
    aliveObjects = filterMap identity updatedObjects
    batchedEffect = Cmd.batch effects
  in
    (aliveObjects, batchedEffect)

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
