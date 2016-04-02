{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Flux where

import Control.Lens

import qualified Data.Map as M

newtype VoterId = VoterId Int deriving (Show,Eq,Ord,Enum)
newtype BillId = BillId Int deriving (Show,Eq,Ord,Enum)
newtype VoteCount = VoteCount Int deriving (Show,Eq,Ord,Num,Enum,Integral,Real)
newtype LiquidityTokens = LiquidityTokens Int deriving (Show,Eq,Ord,Num)

data Vote = InFavour | Against | Abstained

data VoterState = VoterState {
  _votesPerBill :: VoteCount,
  _liquidityTokens :: LiquidityTokens
}
                    
data State = State {
  _voters :: M.Map VoterId VoterState,
  _bills :: M.Map BillId BillState
  }

data VoterBillState = VoterBillState {
  _availableVotes :: VoteCount,
  _vote :: Vote
}

data BillState = BillState {
  _votes :: M.Map VoterId VoterBillState
  }

data VoteResult = VoteResult {
  inFavour :: VoteCount,
  against :: VoteCount,
  abstained :: VoteCount
}

makeLenses ''VoterState
makeLenses ''State
makeLenses ''VoterBillState
makeLenses ''BillState

initialState :: State
initialState = State M.empty M.empty

newVoter :: State -> (State,VoterId)
newVoter state = (state',vid)
  where
    state' = over voters (M.insert vid details) state
    vid = nextKey (view voters state)
    details = VoterState 1 0

newBill :: State -> (State,BillId)
newBill state = (state,bid)
  where
    state' = over bills (M.insert bid details) state
    bid = nextKey (view bills state)
    details = BillState (M.map newVotorBillState (view voters state))
    newVotorBillState vd = VoterBillState (view votesPerBill vd) Abstained

endBill :: BillId -> State -> (State,VoteResult)
endBill bid state  = (state',result)
  where
    billState = view (bills . melem bid) state
    state' = over bills (M.delete bid) state
    result = VoteResult votesFor votesAgainst votesAbstained
    allVotes = M.elems (view votes billState)
    votesFor = sum [votes | (VoterBillState votes InFavour) <- allVotes]
    votesAgainst = sum [votes | (VoterBillState votes Against) <- allVotes]
    votesAbstained = sum [votes | (VoterBillState votes Abstained) <- allVotes]

swapVote :: VoterId -> VoterId -> BillId -> VoteCount -> LiquidityTokens -> State -> State
swapVote vid1 vid2 bid voteCount tokens
    = over (bills . melem bid . votes. melem vid1 . availableVotes) (\v -> checkGEZero (v+voteCount))
    . over (bills . melem bid . votes . melem vid2 . availableVotes) (\v -> checkGEZero (v-voteCount))
    . over (voters . melem vid1 . liquidityTokens) (\v -> checkGEZero (v+tokens))
    . over (voters . melem vid2 . liquidityTokens) (\v -> checkGEZero (v-tokens))

distributeLiquidity :: LiquidityTokens -> State -> State
distributeLiquidity  toShare state = over voters (M.map updateVoter) state
  where
    nTotalVotes = sum [view votesPerBill v | v <- M.elems (view voters state)]
    updateVoter vs = over liquidityTokens (addTokens (view votesPerBill vs) nTotalVotes toShare) vs

    addTokens (VoteCount num) (VoteCount denom) (LiquidityTokens toShare) (LiquidityTokens existing)
      = LiquidityTokens (existing + (toShare * num) `div` denom)

setVote :: VoterId -> BillId -> Vote -> State -> State
setVote vid bid v = set (bills . melem bid . votes . melem vid . vote) v

----------------------------------------------------------------------
-- Helper functions

nextKey :: (Enum k) => M.Map k a -> k
nextKey m | M.null m = toEnum 0
          | otherwise = let (k,_) = M.findMax m in succ k

checkGEZero :: (Num a, Ord a) => a -> a
checkGEZero a | a < 0 = error "balance may not be negative"
              | otherwise = a

melem :: (Ord k) => k -> Lens' (M.Map k v) v
melem k = lens get set
  where
    get m = case M.lookup k m of
      Nothing -> error "missing map key"
      (Just v) -> v
    set m v = M.insert k v m

    
