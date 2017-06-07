{-# LANGUAGE OverloadedStrings #-}
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
import Text.Pandoc.Definition
import qualified Data.Map.Lazy as M

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m, z)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Control.Monad (forM_)
import qualified Data.Text as T
import Data.Default
import Data.IORef
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype WhoIdentifier = WhoIdentifier String
  deriving (Eq, Ord, Show)
newtype WhenIdentifier = WhenIdentifier String
  deriving (Eq, Ord, Show)
newtype WhereIdentifier = WhereIdentifier String
  deriving (Eq, Ord, Show)

instance FromJSON WhoIdentifier where
  parseJSON (Y.String s) = WhoIdentifier <$> pure (T.unpack s)

instance FromJSON WhenIdentifier where
  parseJSON (Y.String s) = WhenIdentifier <$> pure (T.unpack s)

instance FromJSON WhereIdentifier where
  parseJSON (Y.String s) = WhereIdentifier <$> pure (T.unpack s)

data StoryEvent = StoryEvent
  { evWho :: WhoIdentifier
  , evWhen :: WhenIdentifier
  , evWhere :: WhereIdentifier
  }

instance FromJSON StoryEvent where
  parseJSON (Object v) = StoryEvent
    <$> v .: "who"
    <*> v .: "when"
    <*> v .: "where"
  parseJSON invalid    = typeMismatch "StoryEvent" invalid

type StoryEvents = [StoryEvent]

data WhoProperties = WhoProperties
  { whoName :: String
  , whoColor :: String
  , whoKey :: Int
  }

type WhoMap = M.Map WhoIdentifier WhoProperties

data WhenProperties = WhenProperties
  { whenName :: String
  , whenKey :: Int
  }

type WhenMap = M.Map WhenIdentifier WhenProperties

data WhereProperties = WhereProperties
  { whereName :: String
  , whereKey :: Int
  }

type WhereMap = M.Map WhereIdentifier WhereProperties

type PlotProperties = M.Map String String

data Global = Global
  { gWhoMap :: WhoMap
  , gWhenMap :: WhenMap
  , gWhereMap :: WhereMap
  , gEvents :: StoryEvents
  }

instance Default Global where
  def = Global
        { gWhoMap = M.empty 
        , gWhenMap = M.empty
        , gWhereMap = M.empty
        , gEvents = []
        }

globalAddEvent :: StoryEvent -> Global -> Global
globalAddEvent e g = g { gEvents = e : gEvents g }

renderPlot :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> B.ByteString
renderPlot pProp whoMap whenMap whereMap events = renderSvg $ toPlotSvg pProp whoMap whenMap whereMap events

toPlotSvg :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> S.Svg
toPlotSvg pProp whoMap' whenMap' whereMap' events =
  S.docTypeSvg
    ! A.version "1.1"
    ! A.width (S.toValue $ left + whoNameSkip + gridW * (nWhen - 1) + right)
    ! A.height (S.toValue $ top + gridH * (nWhere - 1) + bottom)
    $ do
      S.defs $ do
        S.marker ! A.id_ "arrowTip"
          ! A.markerwidth (S.toValue (10 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.fill "black" ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10
              S.l 10 5
              S.z)
        S.marker ! A.id_ "arrowMark"
          ! A.markerwidth (S.toValue (1 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.stroke "black" ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10)
        S.marker ! A.id_ "eventMark"
          ! A.markerwidth (S.toValue whoStep)
          ! A.markerheight (S.toValue whoStep)
          ! A.markerunits "userSpaceOnUse"
          ! A.refx (S.toValue (whoStep `div` 2))
          ! A.refy (S.toValue (whoStep `div` 2)) $
            S.circle ! A.stroke "black"
            ! A.fill "white"
            ! A.strokeWidth (S.toValue $ whoStep - lineWidth)
            ! A.cx (S.toValue (whoStep `div` 2))
            ! A.cy (S.toValue (whoStep `div` 2))
            ! A.r (S.toValue (whoStep `div` 2))

      S.path ! A.stroke "black"
        ! A.markerEnd "url(#arrowTip)"
        ! A.markerMid "url(#arrowMark)"
        ! A.markerStart "url(#arrowMark)"
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
          ! A.fill "black"
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
          ! A.transform (S.rotateAround (-45) x y)
          $ S.text (T.pack label)
      forM_ (zip whereLabels [0..(nWhere-1)]) $ \(label,i) -> do
        let x = left - textOffs
            y = top + i * gridH
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill "black"
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
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
                 ! A.markerEnd "url(#eventMark)"
                 ! A.markerMid "url(#eventMark)"
                 ! A.strokeWidth (S.toValue lineWidth)
                 ! A.fill "none"
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
                 ! A.stroke "none"
                 ! A.fill "white"
               S.text_
                 ! A.x (S.toValue $ firstX + 2)
                 ! A.y (S.toValue firstY)
                 ! A.fill "black"
                 ! A.fontSize (S.toValue (show whoNameHeight ++ "px"))
                 ! A.dominantBaseline "middle"
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

  whoKeys = map snd $ sortOn fst $ map (\(k,v) -> (whoKey v,k)) $ M.assocs whoMap
  whenKeys = map snd $ sortOn fst $ map (\(k,v) -> (whenKey v,k)) $ M.assocs whenMap
  whereKeys = map snd $ sortOn fst $ map (\(k,v) -> (whereKey v,k)) $ M.assocs whereMap

  whenPos = M.fromList $ zip whenKeys [0::Int ..]
  wherePos = M.fromList $ zip whereKeys [0::Int ..]
  whenLabels = map (\k -> whenName $ whenMap M.! k) whenKeys
  whereLabels = map (\k -> whereName $ whereMap M.! k) whereKeys


alterWho :: WhoIdentifier -> Maybe WhoProperties -> Maybe WhoProperties
alterWho (WhoIdentifier who) Nothing = Just WhoProperties { whoName = who, whoColor = "black", whoKey = 1 }
alterWho _ x = x

alterWhen :: WhenIdentifier -> Maybe WhenProperties -> Maybe WhenProperties
alterWhen (WhenIdentifier when) Nothing = Just WhenProperties { whenName = when, whenKey = 1}
alterWhen _ x = x

alterWhere :: WhereIdentifier -> Maybe WhereProperties -> Maybe WhereProperties
alterWhere (WhereIdentifier here) Nothing = Just WhereProperties { whereName = here, whereKey = 1}
alterWhere _ x = x

processEvents :: IORef Global -> Block -> IO Block
processEvents globalRef b@(CodeBlock (_,["narcha-event"],_) cont) = do
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (e@StoryEvent{}) -> do
         modifyIORef' globalRef (globalAddEvent e)
         return Text.Pandoc.Definition.Null
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processEvents _ b = return b

processPlots :: IORef Global -> Block -> IO Block
processPlots globalRef b@(CodeBlock (_,["narcha-plot"],_) cont) = do
  global <- readIORef globalRef
  let svg = renderPlot M.empty (gWhoMap global) (gWhenMap global) (gWhereMap global) (gEvents global)
  return $ RawBlock "HTML" $ C.unpack svg
processPlots _ b = return b

main :: IO ()
main = do
  let pProp = M.empty
      whoMap = M.fromList [(WhoIdentifier "hero", WhoProperties { whoName = "Hero", whoColor = "blue", whoKey = 1})
                          ,(WhoIdentifier "villain", WhoProperties { whoName = "Villain", whoColor = "red", whoKey = 2})
                          ]
      whenMap = M.fromList [(WhenIdentifier "beginning", WhenProperties { whenName = "Beginning", whenKey = 1})
                           ,(WhenIdentifier "kickoff", WhenProperties { whenName = "Kick-off", whenKey = 2})
                           ,(WhenIdentifier "journey", WhenProperties { whenName = "The Journey", whenKey = 3})
                           ,(WhenIdentifier "climax", WhenProperties { whenName = "Climax", whenKey = 4})
                           ,(WhenIdentifier "ending", WhenProperties { whenName = "Fin", whenKey = 5})
                           ]
      whereMap = M.fromList [(WhereIdentifier "home", WhereProperties { whereName = "Home", whereKey = 1})
                            ,(WhereIdentifier "street", WhereProperties { whereName = "A Dirty Street", whereKey = 3})
                            ,(WhereIdentifier "road", WhereProperties { whereName = "A Lonely Road", whereKey = 2})
                            ,(WhereIdentifier "valley", WhereProperties { whereName = "The Valley", whereKey = 5})
                            ,(WhereIdentifier "fortress", WhereProperties { whereName = "Evil Fortress", whereKey = 6})
                            ]
      events =
        [ StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "beginning"
          , evWhere = WhereIdentifier "home"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "ending"
          , evWhere = WhereIdentifier "fortress"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "beginning"
          , evWhere = WhereIdentifier "fortress"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "kickoff"
          , evWhere = WhereIdentifier "street"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "kickoff"
          , evWhere = WhereIdentifier "street"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "journey"
          , evWhere = WhereIdentifier "road"
          }
        , StoryEvent { evWho = WhoIdentifier "hero"
          , evWhen = WhenIdentifier "climax"
          , evWhere = WhereIdentifier "valley"
          }
        , StoryEvent { evWho = WhoIdentifier "villain"
          , evWhen = WhenIdentifier "climax"
          , evWhere = WhereIdentifier "valley"
          }
        ]
  -- B.writeFile "plot.svg" $ renderPlot pProp whoMap whenMap whereMap events
  txt <- B.getContents
  let input = (either error id $ eitherDecode' txt) :: Pandoc
  globalRef <- newIORef def
  noEvents <- walkM (processEvents globalRef) input
  withPlots <- walkM (processPlots globalRef) noEvents
  B.putStr $ encode withPlots
