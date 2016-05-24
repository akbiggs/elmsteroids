module ExternalMsg exposing (..)

-- <editor-fold> IMPORTS

import Time exposing (Time)
import Keyboard.Extra as Keyboard

-- </editor-fold>

-- <editor-fold> TYPES

type ExternalMsg
  = Init Time
  | Tick Time
  | KeyboardUpdate Keyboard.Msg
  | PlaySound String

-- </editor-fold>
