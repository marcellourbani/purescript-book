module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , Percent(..)
  , True
  , class IsValue
  , class Dimension
  , toValue

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , checked
  , disabled

  , attribute, (:=)
  , text
  , elem

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array Attribute
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

data Attribute = Attribute
  { key          :: String
  , value        :: String
  }
  | SimpleAttribute String
data True
data False

newtype AttributeKey :: forall k1 k2. k1 -> k2 -> Type
newtype AttributeKey a b = AttributeKey String

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

class IsValue a where
  toValue :: a -> String

class Dimension :: forall k. k -> Constraint
class Dimension a

instance intDimension :: Dimension Int

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

newtype Percent = Percent Int

instance percDimension :: Dimension Percent

instance percIsValue :: IsValue Percent where
  toValue (Percent x) = show x <> "%"

attribute :: forall a. IsValue a => AttributeKey a True -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey String True
href = AttributeKey "href"

_class :: AttributeKey String True
_class = AttributeKey "class"

src :: AttributeKey String True
src = AttributeKey "src"

width ::forall d. Dimension d => AttributeKey d True
width = AttributeKey "width"

height ::forall d. Dimension d => AttributeKey d True
height = AttributeKey "height"

disabled :: Attribute 
disabled = SimpleAttribute "disabled"

checked :: Attribute
checked = SimpleAttribute "checked"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""
    renderAttribute (SimpleAttribute x) = x

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
