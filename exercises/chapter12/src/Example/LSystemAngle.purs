module Example.LSystemAngle where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)


lsystemProduce :: forall a. 
         Array a
         -> (a -> Array a)
         -> Int
         -> Array a
lsystemProduce init prod n = go init n
  where
  go:: Array a -> Int -> Array a
  go s 0 = s
  go s i = go (concatMap prod s) (i - 1)

lsystemInterpret :: forall a m s. Monad m => (s -> a -> m s) -> s -> Array a -> m s
lsystemInterpret interpret state sentence = foldM interpret state sentence

type Angle = Number
basenum = 8.0 :: Number
angle = Math.tau / basenum :: Angle 
scale = 0.8 * 6.0 / basenum :: Number

data Letter = L Angle | R Angle | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F, R angle, R angle, F, R angle, R angle, F, R angle, R angle, F, R angle, R angle]

productions :: Letter -> Sentence
productions (L n)= [L n]
productions (R n) = [R n]
productions F = [F, L angle, F , R angle, R angle, F, L angle, F]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Letter -> Effect State
    interpret state (L a )= pure $ state { theta = state.theta - a }
    interpret state (R a )= pure $ state { theta = state.theta + a }
    interpret state F = do
      let x = state.x + Math.cos state.theta * scale
          y = state.y + Math.sin state.theta * scale
      lineTo ctx x y
      pure { x, y, theta: state.theta }
    
  setStrokeStyle ctx "#000"
  setFillStyle ctx "#00F"

  fillPath ctx $ do 
    setShadowOffsetX ctx 10.0
    setShadowOffsetY ctx 20.0
    setShadowBlur ctx 20.0
    setShadowColor ctx "#004"
    moveTo ctx initialState.x initialState.y
    let sentence = lsystemProduce initial productions 5
    _<- lsystemInterpret interpret initialState sentence
    closePath ctx
  