module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, setStrokeStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s i = go (concatMap prod s) (i - 1)

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

data Letter = L | R | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
    interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
    interpret state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      -- moveTo ctx state.x state.y
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
  