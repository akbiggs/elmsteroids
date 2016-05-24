module ExternalMsg exposing (..)

-- <editor-fold> IMPORTS

import Time exposing (Time)
import Keyboard exposing (KeyCode)

-- </editor-fold>

-- <editor-fold> TYPES

type ExternalMsg
  = Tick Time
  | KeyPressed KeyCode
  | KeyDown KeyCode
  | KeyReleased KeyCode
  | KeyUp KeyCode
  | PlaySound String

-- </editor-fold>
