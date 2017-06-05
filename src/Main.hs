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
import qualified Data.Map.Lazy as M

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as B
import Data.List
import Control.Monad (forM_)
import qualified Data.Text as T

newtype WhoIdentifier = WhoIdentifier String
  deriving (Eq, Ord, Show)
newtype WhenIdentifier = WhenIdentifier String
  deriving (Eq, Ord, Show)
newtype WhereIdentifier = WhereIdentifier String
  deriving (Eq, Ord, Show)

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
toPlotSvg pProp whoMap' whenMap' whereMap' events =
  S.docTypeSvg
    ! A.version (S.toValue "1.1")
    ! A.width (S.toValue $ left + whoNameSkip + gridW * (nWhen - 1) + right)
    ! A.height (S.toValue $ top + gridH * (nWhere - 1) + bottom)
    $ do
      S.defs $ do
        S.marker ! A.id_ (S.toValue "arrowTip")
          ! A.markerwidth (S.toValue (10 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.fill (S.toValue "black") ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10
              S.l 10 5
              S.z)
        S.marker ! A.id_ (S.toValue "arrowMark")
          ! A.markerwidth (S.toValue (1 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.stroke (S.toValue "black") ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10)
        S.marker ! A.id_ (S.toValue "eventMark")
          ! A.markerwidth (S.toValue whoStep)
          ! A.markerheight (S.toValue whoStep)
          ! A.markerunits (S.toValue "userSpaceOnUse")
          ! A.refx (S.toValue (whoStep `div` 2))
          ! A.refy (S.toValue (whoStep `div` 2)) $
            S.circle ! A.stroke (S.toValue "black")
            ! A.fill (S.toValue "white")
            ! A.strokeWidth (S.toValue $ whoStep - lineWidth)
            ! A.cx (S.toValue (whoStep `div` 2))
            ! A.cy (S.toValue (whoStep `div` 2))
            ! A.r (S.toValue (whoStep `div` 2))

      S.path ! A.stroke (S.toValue "black")
        ! A.markerEnd (S.toValue "url(#arrowTip)")
        ! A.markerMid (S.toValue "url(#arrowMark)")
        ! A.markerStart (S.toValue "url(#arrowMark)")
        ! A.d (S.mkPath $ do
          S.m (left + whoNameSkip) (top + nWhere * gridH)
          forM_ [0..(nWhen-1)] $ \i ->
            S.l (left + whoNameSkip + i * gridW) (top + nWhere * gridH)
          S.l (left + whoNameSkip + (nWhen - 1) * gridW + arrowGap) (top + nWhere * gridH)
          )
      forM_ (zip whenLabels [0..(nWhen-1)]) $ \(label,i) -> do
        let x = left + whoNameSkip + i * gridW
            y = top + nWhere * gridH + textOffs
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill (S.toValue "black")
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor (S.toValue "end")
          ! A.transform ( S.rotateAround (-45) x y)
          $ S.text (T.pack label)
      forM_ (zip whereLabels [0..(nWhere-1)]) $ \(label,i) -> do
        let x = left - textOffs
            y = top + i * gridH
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill (S.toValue "black")
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor (S.toValue "end")
          ! A.transform ( S.rotateAround (-45) x y)
          $ S.text (T.pack label)
      forM_ (zip whoKeys [0..(nWho-1)]) $ \(kWho,iWho) -> do
        let kEvents = filter ((== kWho) . evWho) events
            xEv = map ((whenPos M.!) . evWhen) kEvents
            yEv = map ((wherePos M.!) . evWhere) kEvents
        case sortOn fst $ zip xEv yEv of
             [] -> S.text (T.pack "")
             allXys@((x1,y1):xys) -> do
               let firstX = left + x1 * gridW
                   firstY = top + y1 * gridH + whoStep * iWho
               S.path ! A.stroke (S.toValue $ whoColor $ whoMap M.! kWho)
                 ! A.markerEnd (S.toValue "url(#eventMark)")
                 ! A.markerMid (S.toValue "url(#eventMark)")
                 ! A.strokeWidth (S.toValue lineWidth)
                 ! A.fill (S.toValue "none")
                 ! A.d (S.mkPath $ do
                   S.m firstX firstY
                   S.s (left + whoNameSkip + x1 * gridW - gridW `div` 2) (top + y1 * gridH + whoStep * iWho)
                       (left + whoNameSkip + x1 * gridW) (top + y1 * gridH + whoStep * iWho)
                   forM_ (zip allXys xys) $ \((xl,yl),(xi,yi)) ->
                     S.s (left + whoNameSkip + xi * gridW - gridW `div` 2) (top + yi * gridH + whoStep * iWho)
                         (left + whoNameSkip + xi * gridW) (top + yi * gridH + whoStep * iWho)
                   )
               S.rect
                 ! A.x (S.toValue $ firstX + 1)
                 ! A.y (S.toValue $ firstY - whoNameHeight `div` 2)
                 ! A.width (S.toValue whoNameLen)
                 ! A.height (S.toValue whoNameHeight)
                 ! A.stroke (S.toValue "none")
                 ! A.fill (S.toValue "white")
               S.text_
                 ! A.x (S.toValue $ firstX + 2)
                 ! A.y (S.toValue firstY)
                 ! A.fill (S.toValue "black")
                 ! A.fontSize (S.toValue $ show whoNameHeight ++ "px")
                 ! A.dominantBaseline (S.toValue "middle")
                 $ S.text (T.pack $ whoName $ whoMap M.! kWho)
  where
  top = 20
  bottom = 100
  left = 100
  right = 50
  arrowGap = 10
  gridW = 50
  whoNameHeight = 6
  lineWidth = whoNameHeight + 2
  whoNameLen = 30
  whoNameSkip = whoNameLen + 10
  whoStep = lineWidth + 2
  gridH = whoStep * (nWho + 2)
  textOffs = 20
  nWho = M.size whoMap
  nWhen = M.size whenMap
  nWhere = M.size whereMap
  whoMap = foldl' (\m who -> M.alter (alterWho who) who m) whoMap' $ map evWho events
  whenMap = foldl' (\m when -> M.alter (alterWhen when) when m) whenMap' $ map evWhen events
  whereMap = foldl' (\m here -> M.alter (alterWhere here) here m) whereMap' $ map evWhere events

  whenPos = M.fromList $ zip (M.keys whenMap) [0::Int ..]
  wherePos = M.fromList $ zip (M.keys whereMap) [0::Int ..]
  whoKeys = M.keys whoMap
  whenLabels = map (\k -> whenName $ whenMap M.! k) $ M.keys whenMap
  whereLabels = map (\k -> whereName $ whereMap M.! k) $ M.keys whereMap


alterWho :: WhoIdentifier -> Maybe WhoProperties -> Maybe WhoProperties
alterWho (WhoIdentifier who) Nothing = Just WhoProperties { whoName = who, whoColor = "black" }
alterWho _ x = x

alterWhen :: WhenIdentifier -> Maybe WhenProperties -> Maybe WhenProperties
alterWhen (WhenIdentifier when) Nothing = Just WhenProperties { whenName = when}
alterWhen _ x = x

alterWhere :: WhereIdentifier -> Maybe WhereProperties -> Maybe WhereProperties
alterWhere (WhereIdentifier here) Nothing = Just WhereProperties { whereName = here}
alterWhere _ x = x


makeSimplePath :: S.AttributeValue
makeSimplePath =  S.mkPath $ do
  S.m 0 0
  S.l 20 30

main :: IO ()
main = do
  let pProp = M.empty
      whoMap = M.fromList [(WhoIdentifier "hero", WhoProperties { whoName = "Hero", whoColor = "blue"})
                          ,(WhoIdentifier "villain", WhoProperties { whoName = "Villain", whoColor = "red"})
                          ]
      whenMap = M.fromList [(WhenIdentifier "01beginning", WhenProperties { whenName = "Beginning"})
                           ,(WhenIdentifier "02kickoff", WhenProperties { whenName = "Kick-off"})
                           ,(WhenIdentifier "03journey", WhenProperties { whenName = "The Journey"})
                           ,(WhenIdentifier "04climax", WhenProperties { whenName = "Climax"})
                           ,(WhenIdentifier "05ending", WhenProperties { whenName = "Fin"})
                           ]
      whereMap = M.fromList [(WhereIdentifier "01home", WhereProperties { whereName = "Home"})
                            ,(WhereIdentifier "03street", WhereProperties { whereName = "A Dirty Street"})
                            ,(WhereIdentifier "02road", WhereProperties { whereName = "A Lonely Road"})
                            ,(WhereIdentifier "05valley", WhereProperties { whereName = "The Valley"})
                            ,(WhereIdentifier "06fortess", WhereProperties { whereName = "Evil Fortress"})
                            ]
      events =
        [ StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "01beginning"
          , evWhere = WhereIdentifier "01home"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "05ending"
          , evWhere = WhereIdentifier "06fortess"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "01beginning"
          , evWhere = WhereIdentifier "06fortess"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "02kickoff"
          , evWhere = WhereIdentifier "03street"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "02kickoff"
          , evWhere = WhereIdentifier "03street"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "03journey"
          , evWhere = WhereIdentifier "02road"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "04climax"
          , evWhere = WhereIdentifier "05valley"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "04climax"
          , evWhere = WhereIdentifier "05valley"
          }
        ]
  B.writeFile "plot.svg" $ renderPlot pProp whoMap whenMap whereMap events
