module Example.Shapes where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (arc, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  strokePath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  setFillStyle ctx "#0F0"

  setStrokeStyle ctx "#F00"

  strokePath ctx $ do
    arc ctx $ translate 200.0 200.0
      { x: 300.0
      , y: 300.0
      , radius: 50.0
      , start: 0.0
      , end: Math.tau * 2.0 / 3.0
      }
    closePath ctx

  setFillStyle ctx "#F00"
    
  fillPath ctx $ do
    moveTo ctx 500.0 300.0
    lineTo ctx 550.0 300.0
    arc ctx 
      { x: 500.0
      , y: 300.0
      , radius: 50.0
      , start: 0.0
      , end: Math.tau * 2.0 / 3.0
      }
    
    closePath ctx  
    

  strokePath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx
