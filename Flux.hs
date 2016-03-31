{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flux where

import qualified Data.Map as M

newtype VoterId = VoterId Int deriving (Show,Eq,Ord,Enum)
newtype BillId = BillId Int deriving (Show,Eq,Ord,Enum)
newtype VoteCount = VoteCount Int deriving (Show,Eq,Ord,Num)
newtype LiquidityTokens = LiquidityTokens Int deriving (Show,Eq,Ord,Num)

data Vote = InFavour | Against | Abstained

data VoterState = VoterState {
  votesPerBill :: VoteCount,
  liquidityTokens :: LiquidityTokens
}
                    
data State = State {
  voters :: M.Map VoterId VoterState,
  bills :: M.Map BillId BillState
  }

data VoterBillState = VoterBillState {
  availableVotes :: VoteCount,
  vote :: Vote
}

data BillState = BillState {
  votes :: M.Map VoterId VoterBillState
  }

data VoteResult = VoteResult {
  inFavour :: VoteCount,
  against :: VoteCount,
  abstained :: VoteCount
}

initialState :: State
initialState = State M.empty M.empty

newVoter :: State -> (State,VoterId)
newVoter state = (state',vid)
  where
    state' = state{voters=M.insert vid details (voters state)}
    vid = nextKey (voters state)
    details = VoterState 1 0

newBill :: State -> (State,BillId)
newBill state = (state,bid)
  where
    state' = state{bills=M.insert bid details (bills state)}
    bid = nextKey (bills state)
    details = BillState (M.map newVotorBillState (voters state))
    newVotorBillState vd = VoterBillState (votesPerBill vd) Abstained

endBill :: BillId -> State -> (State,VoteResult)
endBill bid state  = (state',result)
  where
    billState = lookupMap bid (bills state)
    state' = state{bills=M.delete bid (bills state)}
    result = VoteResult votesFor votesAgainst votesAbstained
    votesFor = sum [votes | (VoterBillState votes InFavour) <- M.elems (votes billState)]
    votesAgainst = sum [votes | (VoterBillState votes Against) <- M.elems (votes billState)]
    votesAbstained = sum [votes | (VoterBillState votes Abstained) <- M.elems (votes billState)]

swapVote :: VoterId -> VoterId -> BillId -> VoteCount -> LiquidityTokens -> State -> State
swapVote vid1 vid2 bid voteCount tokens
    = (updateBills $ updateMap bid $ updateVotes $ updateMap vid1 $ updateAvailableVotes (\v -> checkGEZero (v+voteCount)))
    . (updateBills $ updateMap bid $ updateVotes $ updateMap vid2 $ updateAvailableVotes (\v -> checkGEZero (v-voteCount)))
    . (updateVoters $ updateMap vid1 $ updateLiquidityTokens (\v -> checkGEZero (v+tokens)))
    . (updateVoters $ updateMap vid2 $ updateLiquidityTokens (\v -> checkGEZero (v-tokens)))

setVote :: VoterId -> BillId -> Vote -> State -> State
setVote vid bid = updateBills . updateMap bid . updateVotes . updateMap vid . updateVote . const

----------------------------------------------------------------------
-- Helper functions

nextKey :: (Enum k) => M.Map k a -> k
nextKey m | M.null m = toEnum 0
          | otherwise = let (k,_) = M.findMax m in succ k

updateBills :: (M.Map BillId BillState -> M.Map BillId BillState) -> State -> State
updateBills f s = s{bills=f (bills s)}

updateVoters :: (M.Map VoterId VoterState -> M.Map VoterId VoterState) -> State -> State
updateVoters f s = s{voters=f (voters s)}

updateLiquidityTokens :: (LiquidityTokens -> LiquidityTokens) -> VoterState -> VoterState
updateLiquidityTokens f s = s{liquidityTokens=f (liquidityTokens s)}

updateVotes :: (M.Map VoterId VoterBillState -> M.Map VoterId VoterBillState) -> BillState -> BillState
updateVotes f s = s{votes=f (votes s)}

updateVote :: (Vote -> Vote) -> VoterBillState -> VoterBillState
updateVote f s = s{vote=f (vote s)}

updateAvailableVotes :: (VoteCount -> VoteCount) -> VoterBillState -> VoterBillState
updateAvailableVotes f s = s{availableVotes=f (availableVotes s)}

updateMap :: (Ord k) => k -> (v->v) -> M.Map k v -> M.Map k v
updateMap k updatev m = case M.lookup k m of
  Nothing -> error "missing key"
  (Just v) -> M.insert k (updatev v) m

lookupMap :: (Ord k) => k -> M.Map k v -> v
lookupMap k m = case M.lookup k m of
  Nothing -> error "missing key"
  (Just v) -> v

checkGEZero :: (Num a, Ord a) => a -> a
checkGEZero a | a < 0 = error "balance may not be negative"
              | otherwise = a
