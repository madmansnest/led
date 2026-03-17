module LedVi.Types
  ( -- * Display Types
    DisplayZone(..)
  , DisplayLine(..)
  , LineStyle(..)
  , HighlightStyle(..)
  , emptyDisplayZone
  , mkDisplayZone

    -- * Display Line Constructors
  , displayLine
  , multiDocDisplayLine
  , newContentLine

    -- * Visual State
  , VisualState(..)
  , initialVisualState
  , updateVisualInput
  , updateVisualParse
  , updateVisualScroll

    -- * Scroll Control
  , ScrollDirection(..)
  , applyScroll
  , canScroll
  , centerOnLine
  , scrollToShowRange
  , isRangeVisible
  , logicalViewTop
  , isNearWindowEdge

    -- * Async Preview
  , AsyncPreview(..)
  , AsyncResult(..)
  , newAsyncPreview
  , pollAsyncPreview
  , cancelAsyncPreview

    -- * Result Types
  , VisualModeResult(..)
  , InputResult(..)
  , ResultDisplay(..)

    -- * Continuation Types
  , ContinuationContext(..)
  , ContinuationType(..)
  , GlobalContInfo(..)
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception (try)
import qualified Data.Vector as V
import qualified Graphics.Vty as Vty

import LedParse (DocRange(..), LineRange(..))
import LedVi.Parse (PartialParse(..))
import qualified LedUndo


-- ---------------------------------------------------------------------------
-- Display Types
-- ---------------------------------------------------------------------------

data DisplayLine = DisplayLine
  { dlLineNum    :: !(Maybe Int)
    -- ^ Line number for gutter (Nothing for headers/separators/continuation lines)
  , dlSourceLine :: !(Maybe Int)
    -- ^ Logical source line number (preserved across wrapping for mouse selection)
  , dlDocIdx     :: !Int
    -- ^ Document index (0 for doc list, 1+ for documents)
  , dlText       :: !Text
    -- ^ Line content
  , dlStyle      :: !LineStyle
    -- ^ Visual style for the line
  , dlHighlights :: ![(Int, Int, HighlightStyle)]
    -- ^ Highlighted spans: (start, end, style)
  } deriving stock (Eq, Show)


data LineStyle
  = StyleNormal
    -- ^ Normal text
  | StyleAdded
    -- ^ Newly added/changed line (green)
  | StyleDeleted
    -- ^ Deleted/replaced line (grey italic)
  | StyleHeader
    -- ^ Document header line (bold on blue)
  | StyleSelected
    -- ^ Currently selected line (inverse video)
  deriving stock (Eq, Show)


data HighlightStyle
  = HLMatch
    -- ^ Regex match highlight (yellow background)
  | HLReplacement
    -- ^ Replacement text highlight (cyan background)
  | HLSelection
    -- ^ Selection highlight (inverse video)
  deriving stock (Eq, Show)


data DisplayZone = DisplayZone
  { dzLines       :: !(V.Vector DisplayLine)
    -- ^ Lines to display. In windowed mode, only visible lines + buffer.
  , dzScrollTop   :: !Int
    -- ^ Index of first visible line in dzLines (visual, 0-based)
  , dzTotalLines  :: !Int
    -- ^ Total logical lines in the source (for scroll calculations)
  , dzWindowStart :: !Int
    -- ^ First logical line in dzLines (1-based). For non-windowed, this is 1.
    -- Used to translate between visual indices and logical line numbers.
  , dzHeight      :: !Int
    -- ^ Number of visible rows (terminal height - 1 for input line)
  , dzWidth       :: !Int
    -- ^ Terminal width
  , dzTargetRange :: !(Maybe (Int, Int))
    -- ^ Target range that should be visible (0-based indices into dzLines).
    -- Used to determine if scrolling is needed.
  } deriving stock (Eq, Show)


emptyDisplayZone :: Int -> Int -> DisplayZone
emptyDisplayZone width height = DisplayZone
  { dzLines = V.empty
  , dzScrollTop = 0
  , dzTotalLines = 0
  , dzWindowStart = 1
  , dzHeight = height
  , dzWidth = width
  , dzTargetRange = Nothing
  }


mkDisplayZone :: V.Vector DisplayLine -> Int -> Int -> DisplayZone
mkDisplayZone lns width height = DisplayZone
  { dzLines = lns
  , dzScrollTop = 0
  , dzTotalLines = V.length lns
  , dzWindowStart = 1
  , dzHeight = height
  , dzWidth = width
  , dzTargetRange = Nothing
  }


-- ---------------------------------------------------------------------------
-- Display Line Constructors
-- ---------------------------------------------------------------------------

displayLine :: Int -> Maybe Int -> Text -> LineStyle -> DisplayLine
displayLine docIdx mLineNum txt style = DisplayLine
  { dlLineNum = mLineNum
  , dlSourceLine = mLineNum
  , dlDocIdx = docIdx
  , dlText = txt
  , dlStyle = style
  , dlHighlights = []
  }

multiDocDisplayLine :: Int -> Int -> Text -> LineStyle -> DisplayLine
multiDocDisplayLine docIdx lineNum txt style = DisplayLine
  { dlLineNum = Just lineNum
  , dlSourceLine = Just lineNum
  , dlDocIdx = docIdx
  , dlText = txt
  , dlStyle = style
  , dlHighlights = []
  }

newContentLine :: Text -> LineStyle -> DisplayLine
newContentLine txt style = DisplayLine
  { dlLineNum = Nothing
  , dlSourceLine = Nothing
  , dlDocIdx = 0
  , dlText = txt
  , dlStyle = style
  , dlHighlights = []
  }


-- ---------------------------------------------------------------------------
-- Visual State
-- ---------------------------------------------------------------------------

data VisualState = VisualState
  { vsVty           :: !Vty.Vty
    -- ^ Vty terminal handle
  , vsInputBuffer   :: !Text
    -- ^ Current input text (for display sync)
  , vsScrollOffset  :: !Int
    -- ^ Current scroll position
  , vsLastParse     :: !PartialParse
    -- ^ Most recent parse result
  , vsLastDisplay   :: !DisplayZone
    -- ^ Cached display (for error recovery)
  , vsManualScroll  :: !Bool
    -- ^ True if user manually scrolled (disables auto-center)
  }


initialVisualState :: Vty.Vty -> IO VisualState
initialVisualState vty = do
  (width, height) <- Vty.displayBounds (Vty.outputIface vty)
  return VisualState
    { vsVty = vty
    , vsInputBuffer = ""
    , vsScrollOffset = 0
    , vsLastParse = PPEmpty
    , vsLastDisplay = emptyDisplayZone width (height - 1)
    , vsManualScroll = False
    }


updateVisualInput :: Text -> VisualState -> VisualState
updateVisualInput input vs = vs
  { vsInputBuffer = input
  , vsManualScroll = False  -- Reset manual scroll on input change
  }


updateVisualParse :: PartialParse -> VisualState -> VisualState
updateVisualParse pp vs = vs { vsLastParse = pp }


updateVisualScroll :: Int -> VisualState -> VisualState
updateVisualScroll offset vs = vs
  { vsScrollOffset = offset
  , vsManualScroll = True
  }


-- ---------------------------------------------------------------------------
-- Scroll Control
-- ---------------------------------------------------------------------------

data ScrollDirection
  = ScrollPageUp
  | ScrollPageDown
  | ScrollLineUp
  | ScrollLineDown
  deriving stock (Eq, Show)


-- Page scroll moves by (height - 1) so the edge line becomes visible on the other edge.
applyScroll :: ScrollDirection -> DisplayZone -> DisplayZone
applyScroll dir dz =
  let totalLines = V.length (dzLines dz)
      maxScroll = max 0 (totalLines - dzHeight dz)
      pageSize = max 1 (dzHeight dz - 1)  -- One line overlap for context
      newTop = case dir of
        ScrollPageUp   -> max 0 (dzScrollTop dz - pageSize)
        ScrollPageDown -> min maxScroll (dzScrollTop dz + pageSize)
        ScrollLineUp   -> max 0 (dzScrollTop dz - 1)
        ScrollLineDown -> min maxScroll (dzScrollTop dz + 1)
  in dz { dzScrollTop = newTop }


canScroll :: DisplayZone -> Bool
canScroll dz = V.length (dzLines dz) > dzHeight dz


centerOnLine :: Int -> Int -> Int -> Int
centerOnLine lineIdx totalLines viewHeight =
  let halfView = viewHeight `div` 2
      maxScroll = max 0 (totalLines - viewHeight)
  in max 0 (min maxScroll (lineIdx - halfView))


-- Returns True if all lines from rangeStart to rangeEnd are visible.
isRangeVisible :: Int -> Int -> Int -> Int -> Bool
isRangeVisible rangeStart rangeEnd scrollPos viewHeight =
  rangeStart >= scrollPos && rangeEnd < scrollPos + viewHeight


-- If the range fits in the viewport, scroll to make all lines visible.
-- If the range is bigger than the viewport, show the first line at the top.
-- Line indices are 0-based.
scrollToShowRange :: Int -> Int -> Int -> Int -> Int
scrollToShowRange rangeStart rangeEnd totalLines viewHeight =
  let rangeSize = rangeEnd - rangeStart + 1
      maxScroll = max 0 (totalLines - viewHeight)
  in if rangeSize >= viewHeight
       -- Range is bigger than viewport: show first line at top
       then max 0 (min maxScroll rangeStart)
       else
         -- Range fits: try to show all of it
         -- If range is already visible at current position, we'd keep it
         -- But since we don't have current position, scroll to show range
         -- with some context above if possible
             -- If showing from rangeStart would cut off the end, adjust
         let scrollIfFromTop = rangeStart
             scrollIfFromBottom = max 0 (rangeEnd - viewHeight + 1)
         in if rangeEnd - scrollIfFromTop < viewHeight
              -- Can show full range starting from rangeStart
              then max 0 (min maxScroll scrollIfFromTop)
              -- Need to scroll so rangeEnd is visible
              else max 0 (min maxScroll scrollIfFromBottom)


-- This is used to track scroll position in logical terms for large files.
logicalViewTop :: DisplayZone -> Int
logicalViewTop dz = dzWindowStart dz + dzScrollTop dz


-- Returns True if we're within one screen height of the buffer edge.
-- This indicates that the window should be rebuilt around the new position.
isNearWindowEdge :: DisplayZone -> Bool
isNearWindowEdge dz =
  let bufferSize = V.length (dzLines dz)
      threshold = dzHeight dz  -- One screen height from edge
  in dzScrollTop dz < threshold ||
     dzScrollTop dz > bufferSize - dzHeight dz - threshold


-- ---------------------------------------------------------------------------
-- Async Preview
-- ---------------------------------------------------------------------------

data AsyncResult
  = AsyncMatches ![Int]  -- ^ List of matching line numbers
  | AsyncError !Text     -- ^ Error message
  deriving stock (Eq, Show)

data AsyncPreview = AsyncPreview
  { apThreadId :: !ThreadId
    -- ^ Thread performing the computation
  , apResult   :: !(MVar.MVar AsyncResult)
    -- ^ Result when ready
  , apPattern  :: !Text
    -- ^ Pattern being searched (for cache validation)
  , apDocIdx   :: !Int
    -- ^ Document index being searched
  }

-- Takes an IO action that returns matching line numbers.
newAsyncPreview :: Text -> Int -> IO [Int] -> IO AsyncPreview
newAsyncPreview pat docIdx computation = do
  resultVar <- MVar.newEmptyMVar
  tid <- forkIO $ do
    result <- try computation :: IO (Either SomeException [Int])
    case result of
      Left e -> MVar.putMVar resultVar (AsyncError (show e))
      Right matches -> MVar.putMVar resultVar (AsyncMatches matches)
  return AsyncPreview
    { apThreadId = tid
    , apResult = resultVar
    , apPattern = pat
    , apDocIdx = docIdx
    }

pollAsyncPreview :: AsyncPreview -> IO (Maybe AsyncResult)
pollAsyncPreview ap = MVar.tryTakeMVar (apResult ap)

cancelAsyncPreview :: AsyncPreview -> IO ()
cancelAsyncPreview ap = killThread (apThreadId ap)


-- ---------------------------------------------------------------------------
-- Result Types (from LedVi.hs)
-- ---------------------------------------------------------------------------

data VisualModeResult
  = VisualContinue    -- ^ Continue in normal REPL
  | VisualQuit        -- ^ Quit editor
  deriving stock (Eq, Show)

data InputResult
  = InputLine !Text      -- ^ Normal input line
  | InputEOF             -- ^ EOF (Ctrl-D on empty)
  | InputInterrupt       -- ^ Interrupt (Ctrl-C)
  | InputExitVisual      -- ^ Exit visual mode (double ESC)
  deriving stock (Eq, Show)

data ResultDisplay
  = ResultText ![Text]                      -- ^ Plain text output (p command, errors, etc.)
  | ResultAffected ![LedUndo.AffectedRange] !Int  -- ^ Affected ranges, current doc index in range list
  | ResultEmpty                             -- ^ Nothing to show (show current position)
  | ResultInitial                           -- ^ Initial entry to visual mode (blank screen)
  deriving stock (Eq, Show)


-- ---------------------------------------------------------------------------
-- Continuation Types (used by visual mode for interactive text input)
-- ---------------------------------------------------------------------------

data ContinuationType
  = ContAppend   -- ^ Append text after line (a command)
  | ContInsert   -- ^ Insert text before line (i command)
  | ContChange   -- ^ Change/replace line range (c command)
  | ContNone     -- ^ No continuation
  deriving stock (Eq, Show)

data GlobalContInfo = GlobalContInfo
  { gciPattern :: !Text    -- ^ Global regex pattern
  , gciMatches :: ![Int]   -- ^ Matching line numbers (cached)
  , gciInvert  :: !Bool    -- ^ True for v (invert match)
  } deriving stock (Eq, Show)

data ContinuationContext = ContinuationContext
  { ccType       :: !ContinuationType
  , ccRange      :: !(DocRange, LineRange)  -- ^ Range being edited
  , ccLines      :: ![Text]                 -- ^ Lines accumulated so far (in reverse order)
  , ccGlobalInfo :: !(Maybe GlobalContInfo) -- ^ Info for global commands
  } deriving stock (Eq, Show)
