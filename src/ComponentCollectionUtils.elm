module ComponentCollectionUtils exposing (updateGroup, drawGroup)

-- <editor-fold> IMPORTS

import List exposing (map, filter, filterMap, unzip)
import Collage exposing (Form, group)
import ExternalMsg exposing (ExternalMsg(..))

-- </editor-fold>

-- <editor-fold> FUNCTIONS

updateGroup : (a -> (Maybe a, Cmd ExternalMsg)) -> List a -> (List a, Cmd ExternalMsg)
updateGroup updateFn xs =
  let
    (updatedObjects, effects) = map updateFn xs |> unzip
    aliveObjects = filterMap identity updatedObjects
    batchedEffect = Cmd.batch effects
  in
    (aliveObjects, batchedEffect)

drawGroup : (a -> Form) -> List a -> Form
drawGroup drawFn =
  map drawFn >> group

-- </editor-fold>
