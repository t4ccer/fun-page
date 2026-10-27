-- | Hydration keys: the @fp-id@ values that tie rendered elements to hydration.
module FunPage.Internal.HydrationKey (
  HydrationKey,
  firstKey,
  nextKey,
  keyToInt,
  keyText,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text

-- | The value of an @fp-id@ attribute.
type HydrationKey :: Type
newtype HydrationKey = HydrationKey Int
  deriving newtype (Eq, Ord, Show)

firstKey :: HydrationKey
firstKey = HydrationKey 0

nextKey :: HydrationKey -> HydrationKey
nextKey (HydrationKey n) = HydrationKey (n + 1)

-- | The key as a number.
keyToInt :: HydrationKey -> Int
keyToInt (HydrationKey n) = n

-- | The key as written in the @fp-id@ attribute.
keyText :: HydrationKey -> Text
keyText (HydrationKey n) = Text.pack (show n)
