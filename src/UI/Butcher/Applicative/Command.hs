module UI.Butcher.Applicative.Command
  ( addCmd
  , addCmdHidden
  , peekCmdDesc
  , reorderStart
  , reorderStop
  , withReorder
  , traverseBarbie
    -- * Low-level part functions
  , addCmdPart
  , addCmdPartMany
  , addCmdPartInp
  , addCmdPartManyInp
  )
where

#include "prelude.inc"

import           UI.Butcher.Internal.ApplicativeTypes
import           UI.Butcher.Internal.Applicative



-- | Safe wrapper around 'reorderStart'/'reorderStop' for cases where reducing
-- to a single binding is possible/preferable.
withReorder :: CmdParser out a -> CmdParser out a
withReorder x = reorderStart *> x <* reorderStop

