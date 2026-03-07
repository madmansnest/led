module LedViState
  ( VisualState(..)
  , initialVisualState
  , updateVisualInput
  , updateVisualParse
  , updateVisualScroll

  , PartialParse(..)
  , isParseError
  , isParseComplete

  , DisplayZone(..)
  , DisplayLine(..)
  , LineStyle(..)
  , HighlightStyle(..)
  , emptyDisplayZone

  , ScrollDirection(..)
  , applyScroll
  , canScroll
  , centerOnLine
  , scrollToShowRange
  , isRangeVisible
  ) where

import qualified Data.Vector as V
import qualified Graphics.Vty as Vty

import LedViParse (PartialParse(..), isParseError, isParseComplete)


-- Display Types
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
    -- ^ All lines to display (may be larger than visible area)
  , dzScrollTop   :: !Int
    -- ^ Index of first visible line
  , dzHeight      :: !Int
    -- ^ Number of visible rows (terminal height - 1 for input line)
  , dzWidth       :: !Int
    -- ^ Terminal width
  , dzTargetRange :: !(Maybe (Int, Int))
    -- ^ Target range that should be visible (0-based start, end indices).
    -- Used to determine if scrolling is needed.
  } deriving stock (Eq, Show)


emptyDisplayZone :: Int -> Int -> DisplayZone
emptyDisplayZone width height = DisplayZone
  { dzLines = V.empty
  , dzScrollTop = 0
  , dzHeight = height
  , dzWidth = width
  , dzTargetRange = Nothing
  }


-- Visual State
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


-- Scroll Control
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
