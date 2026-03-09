{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- |
-- Module      : TUI
-- Description : Brick-based terminal UI for the GEMM.
--
-- Provides an interactive interface for computing homology/cohomology
-- of Eilenberg-MacLane spaces and generating Lean 4 proof certificates.
module TUI (runTUI) where

import Control.Monad (void, when)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO

import Brick
import Brick.BChan (newBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Lens.Micro ((^.), (.~), (&))
import Lens.Micro.Mtl ((.=), (%=))
import Lens.Micro.TH (makeLenses)

import GEMM.Types
  ( GradedGroup(..), groupInDegree, showGroup, truncateGraded )
import GEMM.GradedGroups (universalCoefficients)
import GEMM.EilenbergMacLane
  ( emHomologyPWithGenerators, emHomologyZWithGenerators )
import GEMM.LaTeX (renderDocumentP, renderDocumentZ)
import GEMM.JSON (renderJsonP, renderJsonZ)
import GEMM.Certificate (computeCertificateP, certificateToLean)

-- ============================================================
-- Types
-- ============================================================

data Screen
  = MenuScreen
  | InputScreen
  | ResultScreen
  | PreviewScreen   -- ^ Shows generated content before export
  deriving (Eq, Show)

data SpaceType = SpacePF | SpaceZ
  deriving (Eq, Show)

-- | Resource names for viewports.
data Name = ResultVP | PreviewVP
  deriving (Eq, Ord, Show)

data AppState' = AS
  { _asScreen        :: Screen
  , _asSpaceType     :: SpaceType
  , _asMenuItem      :: Int           -- 0 = K(Z/p^f,n), 1 = K(Z,n)
  , _asParamP        :: String
  , _asParamF        :: String
  , _asParamN        :: String
  , _asParamR        :: String
  , _asActiveField   :: Int
  , _asHomology      :: Maybe GradedGroup
  , _asCohomology    :: Maybe GradedGroup
  , _asShowCohom     :: Bool
  , _asStatusMsg     :: String
  , _asComputing     :: Bool
  , _asRange         :: Int
  , _asP             :: Int
  , _asF             :: Int
  , _asN             :: Int
  -- Preview/export state
  , _asPreviewTitle  :: String        -- ^ Title shown above the preview
  , _asPreviewLines  :: [String]      -- ^ Content to display
  , _asPreviewRaw    :: String        -- ^ Full raw content to write
  , _asExportPath    :: String        -- ^ Target file path
  , _asEditingPath   :: Bool          -- ^ Whether the path is being edited
  , _asPreviewIsLazy :: Bool          -- ^ True if _asPreviewRaw is LaTeX (lazy Text)
  , _asPreviewLazy   :: TL.Text       -- ^ Lazy text for LaTeX export
  }

makeLenses ''AppState'

data AppEvent
  = ComputationDone GradedGroup GradedGroup
  | ComputationFailed String

-- ============================================================
-- Initial state
-- ============================================================

initialState :: AppState'
initialState = AS
  { _asScreen       = MenuScreen
  , _asSpaceType    = SpacePF
  , _asMenuItem     = 0
  , _asParamP       = "2"
  , _asParamF       = "1"
  , _asParamN       = "2"
  , _asParamR       = "10"
  , _asActiveField  = 0
  , _asHomology     = Nothing
  , _asCohomology   = Nothing
  , _asShowCohom    = False
  , _asStatusMsg    = ""
  , _asComputing    = False
  , _asRange        = 0
  , _asP            = 0
  , _asF            = 0
  , _asN            = 0
  , _asPreviewTitle = ""
  , _asPreviewLines = []
  , _asPreviewRaw   = ""
  , _asExportPath   = ""
  , _asEditingPath  = False
  , _asPreviewIsLazy = False
  , _asPreviewLazy  = TL.empty
  }

-- ============================================================
-- Drawing
-- ============================================================

drawUI :: AppState' -> [Widget Name]
drawUI s = [ui]
  where
    ui = vBox
      [ withBorderStyle unicode $
          borderWithLabel (str " GEMM — Generalized Eilenberg-MacLane Machine ") $
          padAll 1 body
      , statusBar s
      ]
    body = case s ^. asScreen of
      MenuScreen    -> drawMenu s
      InputScreen   -> drawInput s
      ResultScreen  -> drawResult s
      PreviewScreen -> drawPreview s

-- | Fixed status bar at the bottom of the screen.
statusBar :: AppState' -> Widget Name
statusBar s = vBox
  [ msgLine
  , withAttr (attrName "statusbar") $ hBox [padRight Max keysW]
  ]
  where
    msgLine = case s ^. asStatusMsg of
      "" -> emptyWidget
      m  -> withAttr (attrName "statusmsg") $ str $ " " ++ m
    keysW = str $ " " ++ keys
    keys = case s ^. asScreen of
      MenuScreen    -> "j/Down: select  k/Up: select  Enter: confirm  q: quit"
      InputScreen   ->
        "j/k: field  0-9: input  Backspace: delete  Enter: compute  Esc: back"
      ResultScreen  ->
        let base = "j/k: scroll  g/G: top/bottom  c: cohomology  L: LaTeX  J: JSON"
            cert = if s ^. asSpaceType == SpacePF then "  T: certificate" else ""
        in base ++ cert ++ "  n: new  Esc: back  q: quit"
      PreviewScreen
        | s ^. asEditingPath ->
          "Type path, Enter: confirm  Esc: cancel"
        | otherwise ->
          "j/k/PgUp/PgDn: scroll  g/G: top/bottom  w: write to ["
            ++ s ^. asExportPath ++ "]  e: change path  Esc: back"

drawMenu :: AppState' -> Widget Name
drawMenu s = vBox
  [ str ""
  , hCenter $ str "The Generalized Eilenberg-MacLane Machine"
  , hCenter $ str "Version 4.1"
  , str ""
  , hCenter $ str "Computes the integral homology and cohomology"
  , hCenter $ str "of Eilenberg-MacLane spaces K(G, n)."
  , str ""
  , str ""
  , padLeft (Pad 4) $ vBox
    [ menuItem s 0 "Compute H*(K(Z/p^f, n); Z)"
    , menuItem s 1 "Compute H*(K(Z, n); Z)"
    ]
  , fill ' '
  ]

menuItem :: AppState' -> Int -> String -> Widget Name
menuItem s idx label =
  let active = s ^. asMenuItem == idx
      prefix = if active then " > " else "   "
      w = str (prefix ++ label)
  in if active then withAttr (attrName "menusel") w else w

drawInput :: AppState' -> Widget Name
drawInput s = vBox
  [ str ""
  , hCenter $ str title
  , str ""
  , padLeft (Pad 4) $ vBox fields
  , fill ' '
  ]
  where
    title = case s ^. asSpaceType of
      SpacePF -> "K(Z/p^f, n) — Enter parameters"
      SpaceZ  -> "K(Z, n) — Enter parameters"
    fields = case s ^. asSpaceType of
      SpacePF ->
        [ drawField s 0 "p (prime)    " (s ^. asParamP)
        , drawField s 1 "f (exponent) " (s ^. asParamF)
        , drawField s 2 "n (dimension)" (s ^. asParamN)
        , drawField s 3 "range        " (s ^. asParamR)
        ]
      SpaceZ ->
        [ drawField s 2 "n (dimension)" (s ^. asParamN)
        , drawField s 3 "range        " (s ^. asParamR)
        ]

drawField :: AppState' -> Int -> String -> String -> Widget Name
drawField s idx label val =
  let active = s ^. asActiveField == idx
      prefix = if active then " > " else "   "
      valStr = if active then val ++ "_" else val
      w = str (prefix ++ label ++ ": " ++ valStr)
  in if active then withAttr (attrName "active") w else w

drawResult :: AppState' -> Widget Name
drawResult s = vBox
  [ str ""
  , hCenter $ withAttr (attrName "header") $ str header
  , str ""
  , viewport ResultVP Vertical $ padLeft (Pad 2) $ vBox groupLines
  ]
  where
    rng = s ^. asRange
    header = case s ^. asSpaceType of
      SpacePF -> (if s ^. asShowCohom then "H^*" else "H_*")
                    ++ "(K(Z/" ++ show (s ^. asP) ++ "^" ++ show (s ^. asF)
                    ++ ", " ++ show (s ^. asN) ++ "); Z)  range " ++ show rng
      SpaceZ  -> (if s ^. asShowCohom then "H^*" else "H_*")
                    ++ "(K(Z, " ++ show (s ^. asN) ++ "); Z)  range " ++ show rng
    gg = if s ^. asShowCohom
         then fromMaybe (GradedGroup mempty) (s ^. asCohomology)
         else fromMaybe (GradedGroup mempty) (s ^. asHomology)
    groupLines =
      [ let g = groupInDegree deg gg
            prefix = if s ^. asShowCohom then "  H^" else "  H_"
            pad = if deg < 10 then " " else ""
        in str (prefix ++ pad ++ show deg ++ " = " ++ showGroup g)
      | deg <- [0..rng]
      ]

drawPreview :: AppState' -> Widget Name
drawPreview s = vBox
  [ str ""
  , hCenter $ withAttr (attrName "header") $ str (s ^. asPreviewTitle)
  , str ""
  , viewport PreviewVP Vertical $ vBox contentLines
  , pathLine
  ]
  where
    contentLines = map strWrap (s ^. asPreviewLines)
    pathLine
      | s ^. asEditingPath =
          str " Save to: " <+>
          withAttr (attrName "active") (str $ s ^. asExportPath ++ "_")
      | otherwise = emptyWidget

-- ============================================================
-- Event handling
-- ============================================================

handleEvent :: BrickEvent Name AppEvent -> EventM Name AppState' ()
handleEvent (AppEvent (ComputationDone h c)) = do
  asHomology  .= Just h
  asCohomology .= Just c
  asComputing .= False
  asScreen    .= ResultScreen
  asStatusMsg .= "Computation complete."
handleEvent (AppEvent (ComputationFailed msg)) = do
  asComputing .= False
  asStatusMsg .= "Error: " ++ msg

-- Esc
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen    -> halt
    InputScreen   -> asScreen .= MenuScreen >> asStatusMsg .= ""
    ResultScreen  -> asScreen .= InputScreen >> asStatusMsg .= ""
    PreviewScreen
      | s ^. asEditingPath -> asEditingPath .= False
      | otherwise -> do
          asScreen .= ResultScreen
          asStatusMsg .= ""
          vScrollToBeginning (viewportScroll PreviewVP)

-- Quit
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen   -> halt
    ResultScreen -> halt
    InputScreen  -> return ()
    PreviewScreen
      | s ^. asEditingPath -> asExportPath %= (++ "q")
      | otherwise -> return ()

-- Enter
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen    -> menuSelect
    InputScreen   -> startComputation
    PreviewScreen
      | s ^. asEditingPath -> asEditingPath .= False
      | otherwise -> return ()
    _ -> return ()

-- Arrow keys
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen    -> asMenuItem %= \i -> min 1 (i + 1)
    InputScreen   -> moveField 1
    ResultScreen  -> vScrollBy (viewportScroll ResultVP) 1
    PreviewScreen -> vScrollBy (viewportScroll PreviewVP) 1
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen    -> asMenuItem %= \i -> max 0 (i - 1)
    InputScreen   -> moveField (-1)
    ResultScreen  -> vScrollBy (viewportScroll ResultVP) (-1)
    PreviewScreen -> vScrollBy (viewportScroll PreviewVP) (-1)

-- Page Up/Down
handleEvent (VtyEvent (V.EvKey V.KPageDown [])) = do
  s <- get
  case s ^. asScreen of
    ResultScreen  -> vScrollBy (viewportScroll ResultVP) 20
    PreviewScreen -> vScrollBy (viewportScroll PreviewVP) 20
    _             -> return ()
handleEvent (VtyEvent (V.EvKey V.KPageUp [])) = do
  s <- get
  case s ^. asScreen of
    ResultScreen  -> vScrollBy (viewportScroll ResultVP) (-20)
    PreviewScreen -> vScrollBy (viewportScroll PreviewVP) (-20)
    _             -> return ()

-- Character input
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) = do
  s <- get
  case s ^. asScreen of
    MenuScreen    -> handleMenuChar c
    InputScreen   -> handleInputKey c
    ResultScreen  -> handleResultKey c
    PreviewScreen -> handlePreviewKey c

-- Backspace
handleEvent (VtyEvent (V.EvKey V.KBS [])) = do
  s <- get
  case s ^. asScreen of
    InputScreen   -> handleBackspace
    PreviewScreen | s ^. asEditingPath ->
      asExportPath %= \p -> if null p then p else init p
    _ -> return ()

handleEvent _ = return ()

-- ============================================================
-- Menu handling
-- ============================================================

handleMenuChar :: Char -> EventM Name AppState' ()
handleMenuChar '1' = do
  asMenuItem .= 0
  menuSelect
handleMenuChar '2' = do
  asMenuItem .= 1
  menuSelect
handleMenuChar 'j' = asMenuItem %= \i -> min 1 (i + 1)
handleMenuChar 'k' = asMenuItem %= \i -> max 0 (i - 1)
handleMenuChar _ = return ()

menuSelect :: EventM Name AppState' ()
menuSelect = do
  s <- get
  case s ^. asMenuItem of
    0 -> do
      asSpaceType .= SpacePF
      asActiveField .= 0
      asScreen .= InputScreen
      asStatusMsg .= ""
    1 -> do
      asSpaceType .= SpaceZ
      asActiveField .= 2
      asScreen .= InputScreen
      asStatusMsg .= ""
    _ -> return ()

-- ============================================================
-- Input handling
-- ============================================================

handleInputKey :: Char -> EventM Name AppState' ()
handleInputKey 'j' = moveField 1
handleInputKey 'k' = moveField (-1)
handleInputKey c
  | c >= '0' && c <= '9' = do
      s <- get
      let fld = s ^. asActiveField
      case fld of
        0 -> asParamP %= (++ [c])
        1 -> asParamF %= (++ [c])
        2 -> asParamN %= (++ [c])
        3 -> asParamR %= (++ [c])
        _ -> return ()
  | otherwise = return ()

handleBackspace :: EventM Name AppState' ()
handleBackspace = do
  s <- get
  let fld = s ^. asActiveField
  case fld of
    0 -> asParamP %= safeInit
    1 -> asParamF %= safeInit
    2 -> asParamN %= safeInit
    3 -> asParamR %= safeInit
    _ -> return ()
  where safeInit [] = []; safeInit xs = init xs

moveField :: Int -> EventM Name AppState' ()
moveField delta = do
  s <- get
  let maxF = 3
      minF = case s ^. asSpaceType of { SpacePF -> 0; SpaceZ -> 2 }
      cur = s ^. asActiveField
      new = max minF (min maxF (cur + delta))
  asActiveField .= new

startComputation :: EventM Name AppState' ()
startComputation = do
  s <- get
  let readField fld = case reads fld of { [(v, "")] -> Just v; _ -> Nothing }
  case s ^. asSpaceType of
    SpacePF -> do
      let mp = readField (s ^. asParamP) :: Maybe Int
          mf = readField (s ^. asParamF) :: Maybe Int
          mn = readField (s ^. asParamN) :: Maybe Int
          mr = readField (s ^. asParamR) :: Maybe Int
      case (mp, mf, mn, mr) of
        (Just p, Just f, Just n, Just r)
          | p >= 2 && f >= 1 && n >= 1 && r >= n -> do
              asP .= p; asF .= f; asN .= n; asRange .= r
              asStatusMsg .= "Computing..."
              asScreen .= ResultScreen
              let (homFull, _gens) = emHomologyPWithGenerators p f n r
                  hom = truncateGraded r homFull
                  coh = universalCoefficients hom
              asHomology .= Just hom
              asCohomology .= Just coh
              asStatusMsg .= ""
        _ -> asStatusMsg .= "Invalid parameters. p>=2, f>=1, n>=1, range>=n."
    SpaceZ -> do
      let mn = readField (s ^. asParamN) :: Maybe Int
          mr = readField (s ^. asParamR) :: Maybe Int
      case (mn, mr) of
        (Just n, Just r)
          | n >= 1 && r >= n -> do
              asN .= n; asRange .= r
              asStatusMsg .= "Computing..."
              asScreen .= ResultScreen
              let (homFull, _gens) = emHomologyZWithGenerators n r
                  hom = truncateGraded r homFull
                  coh = universalCoefficients hom
              asHomology .= Just hom
              asCohomology .= Just coh
              asStatusMsg .= ""
        _ -> asStatusMsg .= "Invalid parameters. n>=1, range>=n."

-- ============================================================
-- Result handling
-- ============================================================

handleResultKey :: Char -> EventM Name AppState' ()
handleResultKey 'c' = asShowCohom %= not >> asStatusMsg .= ""
handleResultKey 'n' = do
  asScreen .= MenuScreen
  asHomology .= Nothing
  asCohomology .= Nothing
  asShowCohom .= False
  asStatusMsg .= ""
  vScrollToBeginning (viewportScroll ResultVP)
handleResultKey 'L' = previewLaTeX
handleResultKey 'J' = previewJSON
handleResultKey 'T' = previewCert
handleResultKey 'j' = vScrollBy (viewportScroll ResultVP) 1
handleResultKey 'k' = vScrollBy (viewportScroll ResultVP) (-1)
handleResultKey 'g' = vScrollToBeginning (viewportScroll ResultVP)
handleResultKey 'G' = vScrollToEnd (viewportScroll ResultVP)
handleResultKey _ = return ()

-- | Generate LaTeX content and switch to preview screen.
previewLaTeX :: EventM Name AppState' ()
previewLaTeX = do
  s <- get
  let content = case s ^. asSpaceType of
        SpacePF ->
          let p = s ^. asP; f = s ^. asF; n = s ^. asN; r = s ^. asRange
              (homFull, gens) = emHomologyPWithGenerators p f n r
              hom = truncateGraded r homFull
              coh = universalCoefficients hom
          in renderDocumentP p f n hom coh gens
        SpaceZ ->
          let n = s ^. asN; r = s ^. asRange
              (homFull, gens) = emHomologyZWithGenerators n r
              hom = truncateGraded r homFull
              coh = universalCoefficients hom
          in renderDocumentZ n hom coh gens
      lns = lines (TL.unpack content)
      shown = if length lns > 500
              then take 500 lns ++ ["", "... (" ++ show (length lns) ++ " lines total)"]
              else lns
  asPreviewTitle  .= "LaTeX preview"
  asPreviewLines  .= shown
  asPreviewRaw    .= ""
  asPreviewIsLazy .= True
  asPreviewLazy   .= content
  asExportPath    .= "output.tex"
  asEditingPath   .= False
  asStatusMsg     .= ""
  asScreen        .= PreviewScreen
  vScrollToBeginning (viewportScroll PreviewVP)

-- | Generate JSON content and switch to preview screen.
previewJSON :: EventM Name AppState' ()
previewJSON = do
  s <- get
  let content = case s ^. asSpaceType of
        SpacePF ->
          let p = s ^. asP; f = s ^. asF; n = s ^. asN; r = s ^. asRange
              (homFull, gens) = emHomologyPWithGenerators p f n r
              hom = truncateGraded r homFull
              coh = universalCoefficients hom
          in renderJsonP p f n hom coh gens
        SpaceZ ->
          let n = s ^. asN; r = s ^. asRange
              (homFull, gens) = emHomologyZWithGenerators n r
              hom = truncateGraded r homFull
              coh = universalCoefficients hom
          in renderJsonZ n hom coh gens
  asPreviewTitle  .= "JSON preview"
  asPreviewLines  .= lines content
  asPreviewRaw    .= content
  asPreviewIsLazy .= False
  asPreviewLazy   .= TL.empty
  asExportPath    .= "output.json"
  asEditingPath   .= False
  asStatusMsg     .= ""
  asScreen        .= PreviewScreen
  vScrollToBeginning (viewportScroll PreviewVP)

-- | Generate Lean certificate and switch to preview screen.
previewCert :: EventM Name AppState' ()
previewCert = do
  s <- get
  when (s ^. asSpaceType == SpacePF) $ do
    let p = s ^. asP; f = s ^. asF; n = s ^. asN; r = s ^. asRange
        cert = computeCertificateP p f n r
        content = certificateToLean cert
    asPreviewTitle  .= "Lean 4 certificate preview"
    asPreviewLines  .= lines content
    asPreviewRaw    .= content
    asPreviewIsLazy .= False
    asPreviewLazy   .= TL.empty
    asExportPath    .= "cert.lean"
    asEditingPath   .= False
    asStatusMsg     .= ""
    asScreen        .= PreviewScreen
    vScrollToBeginning (viewportScroll PreviewVP)

-- ============================================================
-- Preview handling
-- ============================================================

handlePreviewKey :: Char -> EventM Name AppState' ()
handlePreviewKey c = do
  s <- get
  if s ^. asEditingPath
    then asExportPath %= (++ [c])
    else case c of
      'w' -> writeExport
      'e' -> asEditingPath .= True
      'j' -> vScrollBy (viewportScroll PreviewVP) 1
      'k' -> vScrollBy (viewportScroll PreviewVP) (-1)
      'g' -> vScrollToBeginning (viewportScroll PreviewVP)
      'G' -> vScrollToEnd (viewportScroll PreviewVP)
      _   -> return ()

-- | Write the preview content to the chosen path.
writeExport :: EventM Name AppState' ()
writeExport = do
  s <- get
  let path = s ^. asExportPath
  when (not (null path)) $ do
    suspendAndResume $ do
      if s ^. asPreviewIsLazy
        then TLIO.writeFile path (s ^. asPreviewLazy)
        else writeFile path (s ^. asPreviewRaw)
      return $ s & asStatusMsg .~ ("Wrote " ++ path)

-- ============================================================
-- Attributes
-- ============================================================

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (attrName "active",    fg V.cyan `V.withStyle` V.bold)
  , (attrName "menusel",   fg V.cyan `V.withStyle` V.bold)
  , (attrName "header",    fg V.white `V.withStyle` V.bold)
  , (attrName "statusbar", V.defAttr `V.withForeColor` V.black
                                     `V.withBackColor` V.white)
  , (attrName "statusmsg", fg V.yellow)
  ]

-- ============================================================
-- App definition
-- ============================================================

app :: App AppState' AppEvent Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const theMap
  }

-- | Launch the TUI.
runTUI :: IO ()
runTUI = do
  chan <- newBChan 10
  -- defaultConfig does not enable mouse mode, so terminal-native
  -- text selection and copy-paste works out of the box.
  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app initialState
