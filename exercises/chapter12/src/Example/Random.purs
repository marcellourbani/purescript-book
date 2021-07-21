module Example.Random where

import Prelude

import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int (floor, hexadecimal, toStringAs)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

strokeAndFill::forall a. Context2D → Effect a → Effect a
strokeAndFill ctx path = do
    _<-fillPath ctx path
    strokePath ctx path

-- randomCircle :: Context2D -> EventListener
-- randomCircle :: forall t8. Context2D -> t8 -> Effect Unit
randomCircle :: Context2D -> Effect EventListener
randomCircle ctx = eventListener \_ -> do
    x <- random
    y <- random
    r <- random
    fill <- random 
    setFillStyle ctx $ "#" <> toStringAs hexadecimal ( floor $ fill * 4096.0)
    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , radius: r * 50.0
         , start : 0.0
         , end   : Math.tau
         }
    strokeAndFill ctx path

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc
  ctx <- getContext2D canvas

  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"

  for_ (1 .. 100) \_ -> do
    x <- random
    y <- random
    r <- random

    let path = arc ctx
         { x     : x * 600.0
         , y     : y * 600.0
         , radius: r * 50.0
         , start : 0.0
         , end   : Math.tau
         }

    strokeAndFill ctx path
  rc<-randomCircle ctx
  addEventListener (EventType "click") rc true (toEventTarget node)