{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Flux where

import Control.Lens

import qualified Data.Map as M

newtype VoterId = VoterId Int deriving (Show,Eq,Ord,Enum)
newtype IssueId = IssueId Int deriving (Show,Eq,Ord,Enum)
newtype VoteCount = VoteCount Int deriving (Show,Eq,Ord,Num,Enum,Integral,Real)
newtype LiquidityTokens = LiquidityTokens Int deriving (Show,Eq,Ord,Num)

data Vote = InFavour | Against | Abstained

data VoterState = VoterState {
  _votesPerIssue :: VoteCount,
  _liquidityTokens :: LiquidityTokens
}
                    
data State = State {
  _voters :: M.Map VoterId VoterState,
  _issues :: M.Map IssueId IssueState
  }

data VoterIssueState = VoterIssueState {
  _availableVotes :: VoteCount,
  _vote :: Vote
}

data IssueState = IssueState {
  _votes :: M.Map VoterId VoterIssueState
  }

data VoteResult = VoteResult {
  inFavour :: VoteCount,
  against :: VoteCount,
  abstained :: VoteCount
}

makeLenses ''VoterState
makeLenses ''State
makeLenses ''VoterIssueState
makeLenses ''IssueState

initialState :: State
initialState = State M.empty M.empty

newVoter :: State -> (State,VoterId)
newVoter state = (state',vid)
  where
    state' = over voters (M.insert vid details) state
    vid = nextKey (view voters state)
    details = VoterState 1 0

newIssue :: State -> (State,IssueId)
newIssue state = (state,bid)
  where
    state' = over issues (M.insert bid details) state
    bid = nextKey (view issues state)
    details = IssueState (M.map newVotorIssueState (view voters state))
    newVotorIssueState vd = VoterIssueState (view votesPerIssue vd) Abstained

endIssue :: IssueId -> State -> (State,VoteResult)
endIssue bid state  = (state',result)
  where
    issueState = view (issues . melem bid) state
    state' = over issues (M.delete bid) state
    result = VoteResult votesFor votesAgainst votesAbstained
    allVotes = M.elems (view votes issueState)
    votesFor = sum [votes | (VoterIssueState votes InFavour) <- allVotes]
    votesAgainst = sum [votes | (VoterIssueState votes Against) <- allVotes]
    votesAbstained = sum [votes | (VoterIssueState votes Abstained) <- allVotes]

swapVote :: VoterId -> VoterId -> IssueId -> VoteCount -> LiquidityTokens -> State -> State
swapVote vid1 vid2 bid voteCount tokens
    = over (issues . melem bid . votes. melem vid1 . availableVotes) (\v -> checkGEZero (v+voteCount))
    . over (issues . melem bid . votes . melem vid2 . availableVotes) (\v -> checkGEZero (v-voteCount))
    . over (voters . melem vid1 . liquidityTokens) (\v -> checkGEZero (v+tokens))
    . over (voters . melem vid2 . liquidityTokens) (\v -> checkGEZero (v-tokens))

distributeLiquidity :: LiquidityTokens -> State -> State
distributeLiquidity  toShare state = over voters (M.map updateVoter) state
  where
    nTotalVotes = sum [view votesPerIssue v | v <- M.elems (view voters state)]
    updateVoter vs = over liquidityTokens (addTokens (view votesPerIssue vs) nTotalVotes toShare) vs

    addTokens (VoteCount num) (VoteCount denom) (LiquidityTokens toShare) (LiquidityTokens existing)
      = LiquidityTokens (existing + (toShare * num) `div` denom)

setVote :: VoterId -> IssueId -> Vote -> State -> State
setVote vid bid v = set (issues . melem bid . votes . melem vid . vote) v

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

    
