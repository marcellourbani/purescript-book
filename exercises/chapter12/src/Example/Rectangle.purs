module Example.Rectangle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, fillPath, getCanvasElementById, getContext2D, rect, rotate, setFillStyle, translate)
import Math as Math
import Partial.Unsafe (unsafePartial)

rotateAround :: Context2D -> Number -> Number -> Number ->  Effect Unit
rotateAround ctx x y angle = do
  translate ctx {translateX : x, translateY : y}
  rotate ctx angle
  translate ctx {translateX : -x, translateY : -y}

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"

  rotateAround ctx 200.0 200.0 $ Math.tau / 3.0
  fillPath ctx $ do 
    rect ctx
      { x: 250.0
      , y: 250.0
      , width: 100.0
      , height: 100.0
      }
    rect ctx
      { x: 352.0
      , y: 250.0
      , width: 100.0
      , height: 100.0
      }

