{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  { _dSEnvAllowedDelegators :: Set VKeyGen
  , _dSEnvEpoch :: Epoch
  , _dSEnvSlot :: Slot
  , _dSEnvLiveness :: SlotCount
  }

makeFields ''DSEnv

-- | Delegation scheduling state
data DSState = DSState
  { _dSStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dSStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DSState

-- | Delegation state
data DState = DState
  { _dSStateDelegationMap :: Map VKeyGen VKey
    -- | When was the last time each genesis key delegated.
  , _dSStateLastDelegation :: Map VKeyGen Slot
  }

makeFields ''DState

data DIEnv = DIEnv
  { _dIEnvAllowedDelegators :: Set VKeyGen
  , _dIEnvEpoch :: Epoch
  , _dIEnvSlot :: Slot
  , _dIEnvLiveness :: SlotCount

  }

makeFields ''DIEnv

data DIState = DIState
  { _dIStateDelegationMap :: Map VKeyGen VKey
  , _dIStateLastDelegation :: Map VKeyGen Slot
  , _dIStateScheduledDelegations :: [(Slot, (VKeyGen, VKey))]
  , _dIStateKeyEpochDelegations :: Set (Epoch, VKeyGen)
  }

makeFields ''DIState

--------------------------------------------------------------------------------
-- Transition systems
--------------------------------------------------------------------------------

-- | Delegation scheduling rules
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
          & scheduledDelegations <>~ [( ((env ^. slot) `addSlot` (env ^. liveness))
                                         , cert ^. dwho
                                        )]
          & keyEpochDelegations %~ (Set.insert (env ^. epoch, cert ^. dwho . _1))
      )
    ]
    where
      verify :: DCert -> PredicateResult SDELEG
      verify = const Passed
      -- Check that this delegator hasn't already delegated this epoch
      notAlreadyDelegated :: DSState -> DCert -> PredicateResult SDELEG
      notAlreadyDelegated st cert =
        if Set.member (cert ^. depoch, cert ^. dwho . _1) (st ^. keyEpochDelegations)
        then Failed HasAlreadyDelegated
        else Passed
      -- Check that there is not already a scheduled delegation from this key
      notAlreadyScheduled :: DSEnv -> DSState -> DCert -> PredicateResult SDELEG
      notAlreadyScheduled env st cert =
        if List.elem
            (((env ^. slot) `addSlot` (env ^. liveness)), cert ^. dwho ^. _1)
            (st ^. scheduledDelegations . to (fmap $ fmap fst))
        then Failed IsAlreadyScheduled
        else Passed
      -- Verify that the delegator is allowed to do so by virtue of being a
      -- genesis key.
      isGenesisKey :: DSEnv -> DCert -> PredicateResult SDELEG
      isGenesisKey env cert =
        if Set.member (cert ^. dwho . _1) (env ^. allowedDelegators)
        then Passed
        else Failed IsNotGenesisKey
      -- Check that the delegation is for a future epoch
      isFutureEpoch :: DSEnv -> DCert -> PredicateResult SDELEG
      isFutureEpoch env cert =
        if env ^. epoch <= cert ^. depoch
        then Passed
        else Failed IsPastEpoch

-- | Delegation rules
data ADELEG

instance STS ADELEG where
  type State ADELEG = DState
  type Signal ADELEG = (Slot, (VKeyGen, VKey))
  type Environment ADELEG = ()

  data PredicateFailure ADELEG
    = Foo
    deriving (Eq, Show)

  rules = []
