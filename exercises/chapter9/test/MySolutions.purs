module Test.MySolutions where

import Prelude

import Control.Monad.Cont (lift)
import Control.Parallel (parTraverse, sequential)
import Data.Array (concat, filter, snoc)
import Data.Either (Either(..))
import Data.Foldable (fold, foldM)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.String.Regex (parseFlags, regex, replace)
import Data.Traversable (oneOf, traverse)
import Effect.Aff (Aff, Error, Milliseconds(..), attempt, delay, parallel)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path
import Test.HTTP (getUrl)

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles i1 i2 o = do
  a <- readTextFile UTF8 i1
  b <- readTextFile UTF8 i2
  writeTextFile UTF8 o (a <> b)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany a o = do 
  fs <- traverse (readTextFile UTF8) a
  writeTextFile UTF8 o $ fold fs

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters p = do
  s <- attempt $ readTextFile UTF8 p
  pure $ length <$> s

writeGet :: String -> String -> Aff Unit
writeGet url path = do
  d <- getUrl url
  writeTextFile UTF8 path d

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel a o = do 
  fs <- parTraverse (readTextFile UTF8) a
  writeTextFile UTF8 o $ fold fs

getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url = sequential $ oneOf $ parallel <$> [
   Just <$> getUrl url
  ,delay (Milliseconds ms) $> Nothing ] 

recurseFiles :: FilePath -> Aff (Array String)
recurseFiles root = do
  let folder = Path.dirname root
  content <- readTextFile UTF8 root
  let files = filter (\s->s /= "") $ split (Pattern "\n") content
  let cleanFiles = Path.concat <$> ( snoc [folder] <$> files)
  
  allFiles <- traverse recurseFiles cleanFiles 
  pure $[root] <> concat allFiles
