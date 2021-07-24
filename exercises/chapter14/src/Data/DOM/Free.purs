module Data.DOM.Free
  ( Attribute
  , Content
  , AttributeKey
  , Href(..)
  , Name
  , class IsValue
  , toValue

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , name

  , attribute, (:=)
  , text
  , comment
  , newName
  , runName
  , isMobile

  , render
  ) where

import Prelude

import Control.Monad.Free (Free, runFreeM, liftF)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Writer (WriterT, execWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Content Unit)
  }

data ContentF a
  = TextContent String a
  | ElementContent Element a
  | Comment String a
  | NewName (Name -> a)
  | IsMobile (Boolean -> a)

instance functorContentF :: Functor ContentF where
  map f (TextContent s x) = TextContent s (f x)
  map f (ElementContent e x) = ElementContent e (f x)
  map f (Comment s x) = Comment s (f x)
  map f (NewName k) = NewName (f <<< k)
  map f (IsMobile x) = IsMobile (f <<< x)

type FreeContent = Free ContentF
newtype Content a = Content (FreeContent a)

derive newtype instance functorContent :: Functor (Content)
derive newtype instance applyContent :: Apply (Content)
derive newtype instance applicativeContent :: Applicative (Content)
derive newtype instance bindContent :: Bind (Content)
derive newtype instance monadContent :: Monad (Content)

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype Name = Name String

runName :: Name -> String
runName (Name n) = n

instance nameIsValue :: IsValue Name where
  toValue (Name n) = n

data Href
  = URLHref String
  | AnchorHref Name

instance hrefIsValue :: IsValue Href where
  toValue (URLHref url) = url
  toValue (AnchorHref (Name nm)) = "#" <> nm

newtype AttributeKey :: forall k. k -> Type
newtype AttributeKey a = AttributeKey String

element :: String -> Array Attribute -> Maybe (Content Unit) -> Element
element name' attribs content = Element
  { name:      name'
  , attribs:   attribs
  , content:   content
  }

newName :: Content Name
newName = Content $ liftF $ NewName identity

href :: AttributeKey Href
href = AttributeKey "href"

name :: AttributeKey Name
name = AttributeKey "name"

text :: String -> Content Unit
text s = Content $ liftF $ TextContent s unit

elem :: Element -> Content Unit
elem e = Content $ liftF $ ElementContent e unit

comment :: String -> Content Unit
comment e = Content $ liftF $ Comment e unit

isMobile :: Content Boolean
isMobile = Content $ liftF $ IsMobile identity

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

attribute :: forall a. IsValue a => AttributeKey a -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a :: Array Attribute -> Content Unit -> Content Unit
a attribs content = elem $ element "a" attribs (Just content)

p :: Array Attribute -> Content Unit -> Content Unit
p attribs content = elem $  element "p" attribs (Just content)

img :: Array Attribute -> Content Unit
img attribs = elem $ element "img" attribs Nothing



_class :: AttributeKey String
_class = AttributeKey "class"

src :: AttributeKey String
src = AttributeKey "src"

width :: AttributeKey Int
width = AttributeKey "width"

height :: AttributeKey Int
height = AttributeKey "height"
  
type Interp = ReaderT Boolean (WriterT String (State Int))

onMobile :: Boolean -- configuration
onMobile = true

render :: Content Unit -> String
render (Content c) = evalState (execWriterT (runReaderT (renderContent c) onMobile)) 0 where
  renderContent :: FreeContent Unit -> Interp Unit
  renderContent content = runFreeM renderContentItem content where
    renderElement:: Element -> Interp Unit
    renderElement (Element e)  = do 
      tell $ "<" <> e.name
      for_ e.attribs $ \x -> do
          tell " "
          renderAttribute x
      renderInnerContent e.name e.content
    renderContentItem :: forall a. ContentF (FreeContent a) -> Interp (FreeContent a)
    renderContentItem (TextContent s rest) = do
      tell s
      pure rest
    renderContentItem (ElementContent e' rest) = do
      renderElement e'
      pure rest
    renderContentItem (Comment s rest) = do
      tell $ "<!--" <> s <> "-->"
      pure rest
    renderContentItem (NewName k) = do
      n<-get
      let fresh = Name $ "name" <> show n
      put $ n + 1
      pure $ k fresh
    renderContentItem (IsMobile f) = do 
      b <- ask
      pure $ f b      
    renderAttribute :: Attribute -> Interp Unit
    renderAttribute (Attribute x) = tell $ x.key <> "=\"" <> x.value <> "\""
    renderInnerContent :: String -> Maybe (Content Unit) -> Interp Unit
    renderInnerContent _ Nothing = tell " />"
    renderInnerContent elname (Just (Content content')) = do
      tell ">"
      renderContent content'
      tell $ "</" <> elname <> ">"