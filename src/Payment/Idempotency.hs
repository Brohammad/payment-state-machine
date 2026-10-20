module Payment.Idempotency
  ( IdempotencyKey (..)
  , Store
  , emptyStore
  , applyIdempotent
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Payment.Transition (transition)
import Payment.Types
  ( Event
  , Payment
  , TransitionError (IdempotencyKeyConflict)
  )

-- IdempotencyKey identifies one logical attempt by the caller.
newtype IdempotencyKey = IdempotencyKey Text
  deriving (Eq, Ord, Show)

data CachedResult = CachedResult Event (Either TransitionError Payment)
  deriving (Eq, Show)

-- Store remembers the event and result already produced for each key.
type Store = Map IdempotencyKey CachedResult

-- An empty store has not processed any idempotency keys.
emptyStore :: Store
emptyStore = Map.empty

-- Reusing a key returns its original result; a new key applies the event once.
applyIdempotent
  :: Store
  -> Payment
  -> IdempotencyKey
  -> Event
  -> (Store, Either TransitionError Payment)
applyIdempotent store payment key event =
  case Map.lookup key store of
    Just (CachedResult originalEvent result)
      | originalEvent == event -> (store, result)
      | otherwise ->
          (store, Left (IdempotencyKeyConflict originalEvent event))
    Nothing ->
      let result = transition payment event
          updatedStore = Map.insert key (CachedResult event result) store
       in (updatedStore, result)
