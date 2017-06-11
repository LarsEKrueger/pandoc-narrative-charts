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
import Control.Monad (forM_, forM)
import Data.Default
import Data.IORef
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad.ST

newtype WhoIdentifier = WhoIdentifier String
  deriving (Eq, Ord, Show)
newtype WhenIdentifier = WhenIdentifier String
  deriving (Eq, Ord, Show)
newtype WhereIdentifier = WhereIdentifier String
  deriving (Eq, Ord, Show)

instance FromJSON WhoIdentifier where
  parseJSON (Y.String s) = WhoIdentifier <$> pure (T.unpack s)
  parseJSON invalid    = typeMismatch "WhoIdentifier" invalid

instance FromJSON WhenIdentifier where
  parseJSON (Y.String s) = WhenIdentifier <$> pure (T.unpack s)
  parseJSON invalid    = typeMismatch "WhenIdentifier" invalid

instance FromJSON WhereIdentifier where
  parseJSON (Y.String s) = WhereIdentifier <$> pure (T.unpack s)
  parseJSON invalid    = typeMismatch "WhereIdentifier" invalid

data EventType = VisibleEvent
               | InvisibleEvent
               | FrontEvent
  deriving (Eq)

data StoryEvent = StoryEvent
  { evWho :: WhoIdentifier
  , evWhen :: WhenIdentifier
  , evWhere :: WhereIdentifier
  }
  deriving (Eq)

instance FromJSON StoryEvent where
  parseJSON (Object v) = StoryEvent
    <$> v .: "who"
    <*> v .: "when"
    <*> v .: "where"
  parseJSON invalid    = typeMismatch "StoryEvent" invalid

type EventMap = M.Map (WhoIdentifier,WhenIdentifier) WhereIdentifier

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
  , gEvents :: EventMap
  }

instance Default Global where
  def = Global
        { gWhoMap = M.empty
        , gWhenMap = M.empty
        , gWhereMap = M.empty
        , gEvents = M.empty
        }

globalAddWho :: WhoIdentifier -> WhoProperties -> Global -> Global
globalAddWho ident prop g = g { gWhoMap = M.insert ident prop $ gWhoMap g }

globalAddWhen :: WhenIdentifier -> WhenProperties -> Global -> Global
globalAddWhen ident prop g = g { gWhenMap = M.insert ident prop $ gWhenMap g }

globalAddWhere :: WhereIdentifier -> WhereProperties -> Global -> Global
globalAddWhere ident prop g = g { gWhereMap = M.insert ident prop $ gWhereMap g }

globalAddEvent :: StoryEvent -> Global -> Global
globalAddEvent e g = g { gEvents = M.insert (evWho e,evWhen e) (evWhere e) $ gEvents g }

renderPlot :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> EventMap -> String
renderPlot pProp whoMap whenMap whereMap events = R.renderSvg $ toPlotSvg pProp whoMap whenMap whereMap events

toPlotSvg :: PlotProperties -> WhoMap -> WhenMap -> WhereMap -> EventMap -> S.Svg
toPlotSvg pProp whoMap' whenMap' whereMap' events' =
  S.svg
    ! A.width (S.toValue $ left + whoNameSkip + gridW * (nWhen - 1) + right)
    ! A.height (S.toValue $ top + whereTotal * whoStep + bottom)
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

      -- Arrow of time
      S.path
        ! A.stroke (S.toValue $ ppAxisColor pProp)
        ! A.markerEnd "url(#arrowTip)"
        ! A.markerMid "url(#arrowMark)"
        ! A.markerStart "url(#arrowMark)"
        ! A.d (S.mkPath $ do
          let y = top + whereTotal * whoStep
          S.m (left + whoNameSkip) y
          forM_ [0..(nWhen-1)] $ \i ->
            S.l (left + whoNameSkip + i * gridW) y
          S.l (left + whoNameSkip + (nWhen - 1) * gridW + arrowGap) y
          )
      -- Labels for arrow of time
      forM_ whenKeys $ \kWhen -> do
        let x = left + whoNameSkip + (whenPos M.! kWhen) * gridW
            y = top + whereTotal * whoStep + textOffs
        S.text_
          ! A.x (S.toValue x)
          ! A.y (S.toValue y)
          ! A.fill (S.toValue $ ppAxisColor pProp)
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
          ! A.dominantBaseline "middle"
          ! A.transform (S.rotateAround (-45) x y)
          $ S.string (whenName $ whenMap M.! kWhen)

      -- Swim lanes and place labels
      forM_ (zip4 whereKeys whereStart whereHeight $ cycle ["#ccc", "#eee"])$ \(kWhere,sWhere,hWhere,zebra) -> do
        let x = left - textOffs
            xt = x - lineWidth `div` 2
            yt = top + sWhere * whoStep + whoStep `div` 2
        S.rect
          ! A.x (S.toValue x)
          ! A.y (S.toValue $ top + sWhere * whoStep - whoStep)
          ! A.width (S.toValue $ left + whoNameSkip + gridW * (nWhen - 1))
          ! A.height (S.toValue $ hWhere * whoStep)
          ! A.stroke "none"
          ! A.fill zebra
        S.text_
          ! A.x (S.toValue xt)
          ! A.y (S.toValue yt)
          ! A.fill (S.toValue $ ppAxisColor pProp)
          ! A.fontSize (S.toValue $ show lineWidth ++ "px")
          ! A.textAnchor "end"
          ! A.dominantBaseline "middle"
          ! A.transform ( S.rotateAround (-45) xt yt)
          $ S.string (whereName $ whereMap M.! kWhere)

      -- Event lines
      forM_ eventPos $ \whoEvPos ->
        case whoEvPos of
             Nothing -> return ()
             Just (kWho, evPos) -> do
               case filter (\(_,_,_,t) -> t /= FrontEvent) evPos of
                   [] -> S.string ""
                   ((iWho1,iWhen1,iWhere1,_):evPoss) -> do
                     let firstX = left + gridW * iWhen1
                         firstY = top + whoStep * (iWho1 + whereStart !! iWhere1)
                         nx0 = firstX + whoNameSkip - 1
                         ny0 = firstY
                         nx1 = firstX + whoNameSkip
                         ny1 = firstY
                     S.path ! A.stroke (S.toValue $ whoColor $ whoMap M.! kWho)
                       ! A.strokeWidth (S.toValue lineWidth)
                       ! A.fill "none"
                       ! A.d (S.mkPath $ do
                         S.m nx1 ny1
                         S.s nx0 ny0 nx1 ny1
                         forM_ evPoss $ \(iWho,iWhen,iWhere,_) -> do
                           let x1 = left + whoNameSkip + gridW * iWhen
                               y1 = top + whoStep * (iWho + whereStart !! iWhere)
                               x0 = x1 - gridW `div` 2
                               y0 = y1
                           S.s x0 y0 x1 y1
                         )
               case filter (\(_,_,_,t) -> t == VisibleEvent) evPos of
                   [] -> S.string ""
                   ((iWho1,iWhen1,iWhere1,_):evPoss) -> do
                     let firstX = left + gridW * iWhen1
                         firstY = top + whoStep * (iWho1 + whereStart !! iWhere1)
                         nx0 = firstX + whoNameSkip - 1
                         ny0 = firstY
                         nx1 = firstX + whoNameSkip
                         ny1 = firstY
                     S.path ! A.stroke "none"
                       ! A.markerEnd "url(#eventMark)"
                       ! A.markerMid "url(#eventMark)"
                       ! A.fill "none"
                       ! A.d (S.mkPath $ do
                         S.m nx1 ny1
                         S.s nx0 ny0 nx1 ny1
                         forM_ evPoss $ \(iWho,iWhen,iWhere,_) -> do
                           let x1 = left + whoNameSkip + gridW * iWhen
                               y1 = top + whoStep * (iWho + whereStart !! iWhere)
                               x0 = x1 - gridW `div` 2
                               y0 = y1
                           S.s x0 y0 x1 y1
                         )
               case takeUntil (\(_,_,_,t) -> t /= FrontEvent) evPos of
                   [] -> S.string ""
                   ((iWho1,iWhen1,iWhere1,_):evPoss) -> do
                     let firstX = left + gridW * iWhen1
                         firstY = top + whoStep * (iWho1 + whereStart !! iWhere1)
                         nx0 = firstX + whoNameSkip - 1
                         ny0 = firstY
                         nx1 = firstX + whoNameSkip
                         ny1 = firstY
                     S.path ! A.stroke (S.toValue $ whoColor $ whoMap M.! kWho)
                       ! A.strokeWidth (S.toValue frontLineWidth)
                       ! A.fill "none"
                       ! A.d (S.mkPath $ do
                         S.m firstX firstY
                         S.s nx0 ny0 nx1 ny1
                         forM_ evPoss $ \(iWho,iWhen,iWhere,_) -> do
                           let x1 = left + whoNameSkip + gridW * iWhen
                               y1 = top + whoStep * (iWho + whereStart !! iWhere)
                               x0 = x1 - gridW `div` 2
                               y0 = y1
                           S.s x0 y0 x1 y1
                         )
      -- Event markers
      forM_ eventPos $ \whoEvPos ->
        case whoEvPos of
             Nothing -> return ()
             Just (kWho, evPos) ->
               case evPos of
                   [] -> S.string ""
                   ((iWho1,iWhen1,iWhere1,_):_) -> do
                     let firstX = left + gridW * iWhen1
                         firstY = top + whoStep * (iWho1 + whereStart !! iWhere1)
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
  textOffs = lineWidth `div` 2
  frontLineWidth = 2 :: Int

  -- Unique events
  events = map (\((who,when),here) -> StoryEvent who when here) $ M.assocs events'

  -- Maps updated with additional keys from events
  whoMap = foldl' (\m who -> M.alter (alterWho who) who m) whoMap' $ map evWho events
  whenMap = foldl' (\m when -> M.alter (alterWhen when) when m) whenMap' $ map evWhen events
  whereMap = foldl' (\m here -> M.alter (alterWhere here) here m) whereMap' $ map evWhere events

  -- Number of items along each dimension
  nWho = M.size whoMap
  nWhen = M.size whenMap
  nWhere = M.size whereMap

  -- Ids sorted on keys
  whoKeys = map fst $ sortOn (whoKey . snd) $ M.assocs whoMap
  whenKeys = map fst $ sortOn (whenKey . snd) $ M.assocs whenMap
  whereKeys = map fst $ sortOn (whereKey . snd) $ M.assocs whereMap

  -- Map the key-sorted ids to positions along their dimensions
  whoPos = M.fromList $ zip whoKeys [0::Int ..]
  whenPos = M.fromList $ zip whenKeys [0::Int ..]
  wherePos = M.fromList $ zip whereKeys [0::Int ..]

  -- For each who, created a list of where over when. Pad missing items.
  whoStory = map whereOverWhen whoKeys

  whereOverWhen kWho =
    let evThis =  filter ((== kWho) . evWho) events
        whenThis = map ((whenPos M.!) . evWhen) evThis
        evSort = map snd $ sortOn fst $ zip whenThis evThis
    in addFrontEvents evSort whenKeys


  addFrontEvents :: StoryEvents -> [WhenIdentifier] -> StoryEvents
  addFrontEvents _ [] = []
  addFrontEvents [] _ = []
  addFrontEvents evl@(ev:evs) (when:whens)
    | evWhen ev == when = ev : addMiddleEvents (evWhere ev) evs whens
    | otherwise         = StoryEvent (evWho ev) when (evWhere ev) FrontEvent : addFrontEvents evl whens

  addMiddleEvents :: WhereIdentifier -> StoryEvents -> [WhenIdentifier] -> StoryEvents
  addMiddleEvents _ _ [] = []
  addMiddleEvents _ [] _ = []
  addMiddleEvents here evl@(ev:evs) (when:whens)
    | evWhen ev == when = ev : addMiddleEvents (evWhere ev) evs whens
    | otherwise         = StoryEvent (evWho ev) when here InvisibleEvent : addMiddleEvents here evl whens





  whereStart = 0 : unfoldr accuInt (0,whereHeight)
  whereTotal = sum whereHeight

  (eventPos,whereHeight) = runST $ do
    whoPosNow <- MV.replicate (nWhen*nWhere) (0::Int)
    evP <- forM whoKeys $ \kWho -> do
      let evThis = filter ((== kWho) . evWho) padEvents
          whenThis = map ((whenPos M.!) . evWhen) evThis
          whereThis = map ((wherePos M.!) . evWhere) evThis
          typeThis = map evType evThis
      case sortOn (\(x,_,_)->x) $ zip3 whenThis whereThis typeThis of
            [] -> return Nothing
            ((iWhen1,iWhere1,iType1):iwws) -> do
              iWho1 <- readInc whoPosNow (iWhen1 + iWhere1 * nWhen)
              oneSeg <- forM iwws $ \(iWhen,iWhere,iType) -> do
                iWho <- readInc whoPosNow (iWhen + iWhere * nWhen)
                return
                  ( iWho
                  , iWhen
                  , iWhere
                  , iType
                  )
              return $ Just
                ( kWho
                , ( iWho1
                  , iWhen1
                  , iWhere1
                  , iType1
                  ) : oneSeg
                )
    whereH <- forM [0 .. (nWhere-1)] $ \iWhere -> do
      cntWhere <- forM [0 .. (nWhen-1)] $ \ iWhen ->
        MV.read whoPosNow (iWhen + iWhere * nWhen)
      return $ 1 + maximum cntWhere
    return (evP,whereH)

  readInc vec offs = do
    old <- MV.read vec offs
    MV.modify vec (1+) offs
    return old

accuInt :: (Int,[Int]) -> Maybe (Int,(Int,[Int]))
accuInt (_,[]) = Nothing
accuInt (s,a:as) = Just (s+a,(s+a,as))

computeWhereHeight :: StoryEvents -> WhereIdentifier -> Int
computeWhereHeight events here =
  let whereEvents = filter ((== here) . evWhere) events
      sortedWhE = sort $ map evWhen whereEvents
      cntWhE = map ((1 +). length) $ group sortedWhE
  in maximum cntWhE

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

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (a:as) =
  if p a
     then [a]
     else a : takeUntil p as

main :: IO ()
main = do
  let pProp = M.empty
  txt <- B.getContents
  let input = (either error id $ eitherDecode' txt) :: Pandoc
  globalRef <- newIORef def
  noEvents <- walkM (processEvents globalRef) input
  withPlots <- walkM (processPlots globalRef) noEvents
  B.putStr $ encode withPlots
