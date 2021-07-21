module Example.Plot where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, getCanvasElementById, getContext2D, lineTo, setFillStyle, strokePath)
import Partial.Unsafe (unsafePartial)

type Point = { x :: Number, y :: Number }

f :: Number -> Point
f n = { x : n, y : n * n / 1000.0 }

renderPath
  :: Context2D
  -> Array Point
  -> Effect Unit
renderPath ctx points = strokePath ctx $ for_ points \{x:x,y:y} -> lineTo ctx x y

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"
  let points = f <$> (100*_) >>> toNumber <$> 0 .. 5
  renderPath ctx points
