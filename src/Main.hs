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
import qualified Text.Blaze.Svg.Renderer.String as R
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import Control.Monad (forM_)
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
  , whoKey :: String
  }

type WhoMap = M.Map WhoIdentifier WhoProperties

data WhoIdProp = WhoIdProp WhoIdentifier WhoProperties

mkWhoIdProp :: T.Text -> T.Text -> T.Text -> T.Text -> WhoIdProp
mkWhoIdProp ident name color key =
  WhoIdProp (WhoIdentifier $ T.unpack ident)
            (WhoProperties (T.unpack name) (T.unpack color) (T.unpack key))

instance FromJSON WhoIdProp where
  parseJSON (Object v) = mkWhoIdProp
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "color"
    <*> v .: "key"
  parseJSON invalid    = typeMismatch "WhoIdProp" invalid


data WhenProperties = WhenProperties
  { whenName :: String
  , whenKey :: String
  }

type WhenMap = M.Map WhenIdentifier WhenProperties

data WhenIdProp = WhenIdProp WhenIdentifier WhenProperties

mkWhenIdProp :: T.Text -> T.Text -> T.Text -> WhenIdProp
mkWhenIdProp ident name key =
  WhenIdProp (WhenIdentifier $ T.unpack ident)
            (WhenProperties (T.unpack name) (T.unpack key))

instance FromJSON WhenIdProp where
  parseJSON (Object v) = mkWhenIdProp
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "key"
  parseJSON invalid    = typeMismatch "WhenIdProp" invalid


data WhereProperties = WhereProperties
  { whereName :: String
  , whereKey :: String
  }

type WhereMap = M.Map WhereIdentifier WhereProperties

data WhereIdProp = WhereIdProp WhereIdentifier WhereProperties

mkWhereIdProp :: T.Text -> T.Text -> T.Text -> WhereIdProp
mkWhereIdProp ident name key = WhereIdProp (WhereIdentifier $ T.unpack ident) (WhereProperties (T.unpack name) (T.unpack key))

instance FromJSON WhereIdProp where
  parseJSON (Object v) = mkWhereIdProp
    <$> v .: "id"
    <*> v .: "name"
    <*> v .: "key"
  parseJSON invalid    = typeMismatch "WhereIdProp" invalid

data PlotProperties = PlotProperties
  { ppAxisColor :: String
  , ppTop :: Int
  , ppBottom :: Int
  , ppLeft :: Int
  , ppRight :: Int
  , ppArrowGap :: Int
  , ppGridW :: Int
  , ppNameHeight :: Int
  , ppNameEdge :: Int
  , ppNameLen :: Int
  , ppNameGap :: Int
  }

instance FromJSON PlotProperties where
  parseJSON (Object v) = PlotProperties
    <$> v .:? "axisColor" .!= "black"
    <*> v .:? "top" .!= 20
    <*> v .:? "bottom" .!= 100
    <*> v .:? "left" .!= 100
    <*> v .:? "right" .!= 50
    <*> v .:? "arrowGap" .!= 10
    <*> v .:? "eventGrid" .!= 50
    <*> v .:? "nameHeight" .!= 10
    <*> v .:? "nameEdge" .!= 1
    <*> v .:? "nameLen" .!= 50
    <*> v .:? "nameGap" .!= 1
  parseJSON invalid    = typeMismatch "PlotProperties" invalid

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

globalAddWho :: WhoIdentifier -> WhoProperties -> Global -> Global
globalAddWho ident prop g = g { gWhoMap = M.insert ident prop $ gWhoMap g }

globalAddWhen :: WhenIdentifier -> WhenProperties -> Global -> Global
globalAddWhen ident prop g = g { gWhenMap = M.insert ident prop $ gWhenMap g }

globalAddWhere :: WhereIdentifier -> WhereProperties -> Global -> Global
globalAddWhere ident prop g = g { gWhereMap = M.insert ident prop $ gWhereMap g }

globalAddEvent :: StoryEvent -> Global -> Global
globalAddEvent e g = g { gEvents = e : gEvents g }

renderPlot :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> String
renderPlot pProp whoMap whenMap whereMap events = R.renderSvg $ toPlotSvg pProp whoMap whenMap whereMap events

toPlotSvg :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> StoryEvents -> S.Svg
toPlotSvg pProp whoMap' whenMap' whereMap' events =
  S.svg
    ! A.width (S.toValue $ left + whoNameSkip + gridW * (nWhen - 1) + right)
    ! A.height (S.toValue $ top + gridH * nWhere + bottom)
    $ do
      S.defs $ do
        S.marker ! A.id_ "arrowTip"
          ! A.markerwidth (S.toValue (10 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.fill (S.toValue $ ppAxisColor pProp) ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10
              S.l 10 5
              S.z)
        S.marker ! A.id_ "arrowMark"
          ! A.markerwidth (S.toValue (1 :: Int))
          ! A.markerheight (S.toValue (10 :: Int))
          ! A.refx (S.toValue (0 :: Int))
          ! A.refy (S.toValue (5 :: Int)) $
            S.path ! A.stroke (S.toValue $ ppAxisColor pProp) ! A.d (S.mkPath $ do
              S.m 0 0
              S.l 0 10)
        S.marker ! A.id_ "eventMark"
          ! A.markerwidth (S.toValue whoStep)
          ! A.markerheight (S.toValue whoStep)
          ! A.markerunits "userSpaceOnUse"
          ! A.refx (S.toValue (whoStep `div` 2))
          ! A.refy (S.toValue (whoStep `div` 2)) $
            S.circle ! A.stroke (S.toValue $ ppAxisColor pProp)
            ! A.fill "white"
            ! A.strokeWidth (S.toValue $ whoStep - lineWidth)
            ! A.cx (S.toValue (whoStep `div` 2))
            ! A.cy (S.toValue (whoStep `div` 2))
            ! A.r (S.toValue (whoStep `div` 2))

      S.path ! A.stroke (S.toValue $ ppAxisColor pProp)
        ! A.markerEnd "url(#arrowTip)"
        ! A.markerMid "url(#arrowMark)"
        ! A.markerStart "url(#arrowMark)"
        ! A.d (S.mkPath $ do
          S.m (left + whoNameSkip) (top + nWhere * gridH)
          forM_ [0..(nWhen-1)] $ \i ->
            S.l (left + whoNameSkip + i * gridW) (top + nWhere * gridH)
          S.l (left + whoNameSkip + (nWhen - 1) * gridW + arrowGap) (top + nWhere * gridH)
          )
      forM_ whenKeys $ \kWhen -> do
        let x = left + whoNameSkip + (whenPos M.! kWhen) * gridW
            y = top + nWhere * gridH + textOffs
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill (S.toValue $ ppAxisColor pProp)
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
          ! A.dominantBaseline "middle"
          ! A.transform (S.rotateAround (-45) x y)
          $ S.string (whenName $ whenMap M.! kWhen)
      forM_ whereKeys $ \kWhere -> do
        let x = left - textOffs
            y = top + (wherePos M.! kWhere) * gridH
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill (S.toValue $ ppAxisColor pProp)
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
          ! A.dominantBaseline "middle"
          ! A.transform ( S.rotateAround (-45) x y)
          $ S.string (whereName $ whereMap M.! kWhere)
      forM_ whoKeys $ \kWho -> do
        let whoEvents = filter ((== kWho) . evWho) events
            iWho = whoPos M.! kWho
            xWho = map ((whenPos M.!) . evWhen) whoEvents
            yWho = map ((wherePos M.!) . evWhere) whoEvents
        case sortOn fst $ zip xWho yWho of
             [] -> S.string ""
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
                 ! A.x (S.toValue $ firstX + whoNameGap)
                 ! A.y (S.toValue $ firstY - whoNameHeight `div` 2)
                 ! A.width (S.toValue whoNameLen)
                 ! A.height (S.toValue whoNameHeight)
                 ! A.stroke "none"
                 ! A.fill "white"
               S.text_
                 ! A.x (S.toValue $ firstX + whoNameGap + whoNameLen `div` 2)
                 ! A.y (S.toValue firstY)
                 ! A.fill (S.toValue $ ppAxisColor pProp)
                 ! A.fontSize (S.toValue (show whoNameHeight ++ "px"))
                 ! A.dominantBaseline "middle"
                 ! A.textAnchor "middle"
                 $ S.string (whoName $ whoMap M.! kWho)
  where
  top = 0 `max` ppTop pProp
  bottom = 0 `max` ppBottom pProp
  left = 0 `max` ppLeft pProp
  right = 0 `max` ppRight pProp
  arrowGap = 1 `max` ppArrowGap pProp
  gridW = 10 `max` ppGridW pProp
  whoNameHeight = 5 `max` ppNameHeight pProp
  whoNameEdge1 = 1 `max` ppNameEdge pProp
  whoNameLen = 10 `max` ppNameLen pProp
  whoNameGap = 1 `max` ppNameGap pProp
  whoNameEdge = 2 * whoNameEdge1
  whoNameSkip = whoNameLen + 2 * whoNameGap + lineWidth
  lineWidth = whoNameHeight + whoNameEdge
  whoStep = lineWidth + 2
  gridH = whoStep * (nWho + 2)
  textOffs = lineWidth `div` 2
  nWho = M.size whoMap
  nWhen = M.size whenMap
  nWhere = M.size whereMap
  whoMap = foldl' (\m who -> M.alter (alterWho who) who m) whoMap' $ map evWho events
  whenMap = foldl' (\m when -> M.alter (alterWhen when) when m) whenMap' $ map evWhen events
  whereMap = foldl' (\m here -> M.alter (alterWhere here) here m) whereMap' $ map evWhere events

  whoKeys = map fst $ sortOn (whoKey . snd) $ M.assocs whoMap
  whenKeys = map fst $ sortOn (whenKey . snd) $ M.assocs whenMap
  whereKeys = map fst $ sortOn (whereKey . snd) $ M.assocs whereMap

  whoPos = M.fromList $ zip whoKeys [0::Int ..]
  whenPos = M.fromList $ zip whenKeys [0::Int ..]
  wherePos = M.fromList $ zip whereKeys [0::Int ..]


alterWho :: WhoIdentifier -> Maybe WhoProperties -> Maybe WhoProperties
alterWho (WhoIdentifier who) Nothing = Just WhoProperties { whoName = who, whoColor = "black", whoKey = who }
alterWho _ x = x

alterWhen :: WhenIdentifier -> Maybe WhenProperties -> Maybe WhenProperties
alterWhen (WhenIdentifier when) Nothing = Just WhenProperties { whenName = when, whenKey = when}
alterWhen _ x = x

alterWhere :: WhereIdentifier -> Maybe WhereProperties -> Maybe WhereProperties
alterWhere (WhereIdentifier here) Nothing = Just WhereProperties { whereName = here, whereKey = here}
alterWhere _ x = x

processEvents :: IORef Global -> Block -> IO Block
processEvents globalRef b@(CodeBlock (_,["narcha-event"],_) cont) =
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (e@StoryEvent{}) -> do
         modifyIORef' globalRef (globalAddEvent e)
         return Text.Pandoc.Definition.Null
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processEvents globalRef b@(CodeBlock (_,["narcha-where"],_) cont) =
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (WhereIdProp id prop) -> do
         modifyIORef' globalRef (globalAddWhere id prop)
         return Text.Pandoc.Definition.Null
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processEvents globalRef b@(CodeBlock (_,["narcha-who"],_) cont) =
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (WhoIdProp id prop) -> do
         modifyIORef' globalRef (globalAddWho id prop)
         return Text.Pandoc.Definition.Null
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processEvents globalRef b@(CodeBlock (_,["narcha-when"],_) cont) =
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (WhenIdProp id prop) -> do
         modifyIORef' globalRef (globalAddWhen id prop)
         return Text.Pandoc.Definition.Null
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processEvents _ b = return b

processPlots :: IORef Global -> Block -> IO Block
processPlots globalRef b@(CodeBlock (_,["narcha-plot"],_) cont) =
  case Y.decodeEither' $ T.encodeUtf8 $ T.pack cont of
       Right (pp@PlotProperties{}) -> do
         global <- readIORef globalRef
         let svg = renderPlot pp (gWhoMap global) (gWhenMap global) (gWhereMap global) (gEvents global)
         return $ RawBlock "HTML" svg
       Left err -> return $ CodeBlock nullAttr $ "ERROR: " ++ Y.prettyPrintParseException err
processPlots _ b = return b

main :: IO ()
main = do
  let pProp = M.empty
  txt <- B.getContents
  let input = (either error id $ eitherDecode' txt) :: Pandoc
  globalRef <- newIORef def
  noEvents <- walkM (processEvents globalRef) input
  withPlots <- walkM (processPlots globalRef) noEvents
  B.putStr $ encode withPlots
