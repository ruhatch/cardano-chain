{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies          #-}
module Ledger.Delegation where

import Control.Lens
import Control.State.Transition
import Ledger.Signatures
import Ledger.Core
import Numeric.Natural (Natural)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------
-- Abstract types
--------------------------------------------------------------------------------

-- | A genesis key is a specialisation of a generic VKey.
newtype VKeyGen = VKeyGen VKey
  deriving (Eq, Ord, Show)


data DCert = DCert
  { -- | Body of the delegation certificate
    _dbody :: (VKey, Epoch)
    -- | Witness for the delegation cerfiticate
  , _dwit :: Sig VKeyGen
    -- | Who delegates to whom
  , _dwho :: (VKeyGen, VKey)
    -- | Certificate epoch
  , _depoch :: Epoch
  }

makeLenses ''DCert

--------------------------------------------------------------------------------
-- Derived types
--------------------------------------------------------------------------------

-- | Delegation scheduling environment
data DSEnv = DSEnv
  { _dseAllowedDelegators :: Set VKeyGen
  , _dseEpoch :: Epoch
  , _dseSlot :: Slot
  , _dseLiveness :: SlotCount
  }

makeLenses ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dssScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dssKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeLenses ''DSState

-- | Delegation state
data DState = DState
  { _dsDelegationMap :: Map VKeyGen VKey
    -- | When was the last time each genesis key delegated.
  , _dsLastDelegation :: Map VKeyGen Slot
  }

makeLenses ''DState

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------

-- | Delegation schdeuling rules
data SDELEG

instance STS SDELEG where
  type State SDELEG = DSState
  type Signal SDELEG = DCert
  type Environment SDELEG = DSEnv

  data PredicateFailure SDELEG
    = IsNotGenesisKey
    | IsPastEpoch
    | HasAlreadyDelegated
    | IsAlreadyScheduled
    deriving (Eq, Show)

  rules =
    [ Rule
      [ Predicate $ \(_, _, cert) -> verify cert
      , Predicate $ \(_, st, cert) -> notAlreadyDelegated st cert
      , Predicate $ \(env, _, cert) -> isGenesisKey env cert
      , Predicate $ \(env, _, cert) -> isFutureEpoch env cert
      ]
      ( Extension . Transition $
        \(env, st, cert) -> st
          & dssScheduledDelegations <>~ [( ((env ^. dseSlot) `addSlot` (env ^. dseLiveness))
                                         , cert ^. dwho
                                        )]
          & dssKeyEpochDelegations %~ (Set.insert (env ^. dseEpoch, cert ^. dwho . _1))
      )
    ]
    where
      verify :: DCert -> PredicateResult SDELEG
      verify = const Passed
      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> PredicateResult SDELEG
      notAlreadyDelegated st cert =
        if Set.member (cert ^. depoch, cert ^. dwho . _1) (st ^. dssKeyEpochDelegations)
        then Failed HasAlreadyDelegated
        else Passed
      -- Check that there is not already a scheduled delegation from this key
      notAlreadyScheduled :: DSEnv -> DSState -> DCert -> PredicateResult SDELEG
      notAlreadyScheduled env st cert =
        if List.elem
            (((env ^. dseSlot) `addSlot` (env ^. dseLiveness)), cert ^. dwho ^. _1)
            (st ^. dssScheduledDelegations . to (fmap $ fmap fst))
        then Failed IsAlreadyScheduled
        else Passed
      -- Verify that the delegator is allowed to do so by virtue of being a
      -- genesis key.
      isGenesisKey :: DSEnv -> DCert -> PredicateResult SDELEG
      isGenesisKey env cert =
        if Set.member (cert ^. dwho . _1) (env ^. dseAllowedDelegators)
        then Passed
        else Failed IsNotGenesisKey
      -- Check that the delegation is for a future epoch
      isFutureEpoch :: DSEnv -> DCert -> PredicateResult SDELEG
      isFutureEpoch env cert =
        if env ^. dseEpoch <= cert ^. depoch
        then Passed
        else Failed IsPastEpoch
