{-
Copyright (c) 2017 Lars Krueger

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

module Main
where

import Text.Pandoc.JSON as J
import Text.Pandoc.Walk
import Data.Map.Lazy as M

import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Utf8
import qualified Data.ByteString.Lazy as B

newtype WhoIdentifier = WhoIdentifier String
newtype WhenIdentifier = WhenIdentifier String
newtype WhereIdentifier = WhereIdentifier String

data StoryEvent = StoryEvent
  { evWho :: WhoIdentifier
  , evWhen :: WhenIdentifier
  , evWhere :: WhereIdentifier
  }

type StoryEvents = [StoryEvent]

data WhoProperties = WhoProperties
  { whoName :: String
  , whoColor :: String
  }

type WhoMap = M.Map WhoIdentifier WhoProperties

data WhenProperties = WhenProperties
  { whenName :: String
  }

type WhenMap = M.Map WhenIdentifier WhenProperties

data WhereProperties = WhereProperties
  { whereName :: String
  }

type WhereMap = M.Map WhereIdentifier WhereProperties

type PlotProperties = M.Map String String

renderPlot :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> B.ByteString
renderPlot pProp whoMap whenMap whereMap events = renderSvg $ toPlotSvg pProp whoMap whenMap whereMap events

toPlotSvg :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> S.Svg
toPlotSvg pProp whoMap whenMap whereMap events =
  S.docTypeSvg S.! A.version (S.toValue "1.1") S.! A.width (S.toValue "150") S.! A.height (S.toValue "100") $ do
    S.path S.! A.d makeSimplePath

makeSimplePath :: S.AttributeValue
makeSimplePath =  S.mkPath $ do
  S.l 2 3
  S.m 4 5

main :: IO ()
main = putStrLn "Hello, Haskell!"
