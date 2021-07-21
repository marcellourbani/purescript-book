module Example.LSystemM where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console(log)
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

data Letter = L | R | F Boolean
instance showLetter :: Show Letter where
  show L = "L"
  show R = "R"
  show (F b) = "F " <> show b

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F false]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
productions (F true) = [F true,L,F false,L,F true,R,F false,R,F true,R,F false,R,F true,L,F false,L,F true]
productions (F false) = [F false,R,F true,R,F false,L,F true,L,F false,L,F true,L,F false,R,F true,R,F false]

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
    interpret state _ = do
      let x = state.x + Math.cos state.theta * 4.0
          y = state.y + Math.sin state.theta * 4.0
      -- moveTo ctx state.x state.y
      lineTo ctx x y
      pure { x, y, theta: state.theta }
    
    interpretC :: State -> Letter -> Effect State
    interpretC state l = do 
      log $ show l <> show state
      pure case l of 
            L -> state { theta = state.theta - Math.tau / 6.0 }
            R -> state { theta = state.theta + Math.tau / 6.0 }
            _ -> state {
              x = state.x + Math.cos state.theta * 4.0,
              y = state.y + Math.sin state.theta * 4.0
            }
      

  setStrokeStyle ctx "#000"
  setFillStyle ctx "#00F"

  fillPath ctx $ do 
    let sentence = lsystemProduce initial productions 3
    _<- lsystemInterpret interpretC initialState sentence
    setShadowOffsetX ctx 10.0
    setShadowOffsetY ctx 20.0
    setShadowBlur ctx 20.0
    setShadowColor ctx "#004"
    moveTo ctx initialState.x initialState.y
    _<- lsystemInterpret interpret initialState sentence
    closePath ctx
  