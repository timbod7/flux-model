{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Flux where

import Control.Lens(view,over,set,makeLenses,lens,Lens')

import qualified Data.Map as M

newtype VoterId = VoterId Int deriving (Show,Eq,Ord,Enum)
newtype IssueId = IssueId Int deriving (Show,Eq,Ord,Enum)
newtype VoteTokens = VoteTokens Int deriving (Show,Eq,Ord,Num,Enum,Integral,Real)
newtype LiquidityTokens = LiquidityTokens Int deriving (Show,Eq,Ord,Num)

-- | The overall system state
data State = State {
  _voters :: M.Map VoterId VoterState,
  _issues :: M.Map IssueId IssueState
  }

-- | The state associated with each votor
data VoterState = VoterState {
  -- | How many votes received for each new issue
  _votesPerIssue :: VoteTokens,

  -- | The number of liquidity tokens currently held
  _liquidityTokens :: LiquidityTokens
}
                    
  -- | The state for each voter on each issue
data VoterIssueState = VoterIssueState {
  --  | The number of votes available for the issue
  _availableVotes :: VoteTokens,

  -- | The voter's position on the issue
  _vote :: Vote
}

data IssueState = IssueState {
  _votes :: M.Map VoterId VoterIssueState
  }

data Vote = InFavour | Against | Abstained

data VoteResult = VoteResult {
  inFavour :: VoteTokens,
  against :: VoteTokens,
  abstained :: VoteTokens
}

makeLenses ''VoterState
makeLenses ''State
makeLenses ''VoterIssueState
makeLenses ''IssueState

-- | A initial state with no issues and no voters
initialState :: State
initialState = State M.empty M.empty

-- | Register a new voter, and allocate a unique id.
-- 
-- For each new issue the votor will receive `nVotes`
-- vote tokens. For most voters `nVotes` will be 1, though
-- polical parties will receive more votes where their
-- preferences have contributed to the election of a flux
-- senator.
--
newVoter :: VoteTokens -> State -> (State,VoterId)
newVoter nVotes state = (state',vid)
  where
    state' = over voters (M.insert vid details) state
    vid = nextKey (view voters state)
    details = VoterState nVotes 0

-- | Create a new issue. Currently registered voters
-- receive their allocated votes for the issue.
--    
newIssue :: State -> (State,IssueId)
newIssue state = (state,iid)
  where
    state' = over issues (M.insert iid details) state
    iid = nextKey (view issues state)
    details = IssueState (M.map newVotorIssueState (view voters state))
    newVotorIssueState vd = VoterIssueState (view votesPerIssue vd) Abstained

-- | Set the vote for a voter on a given issue
--
-- Subsequent calls to this function will reset the vote: the last
-- request prior to the issue ending will be used.
--
setVote :: VoterId -> IssueId -> Vote -> State -> State
setVote vid iid v = set (issues . melem iid . votes . melem vid . vote) v

-- | Trade some votes on an issue with another voter.
--
-- The request `tradeVote voter1 voter2 issue nVotes nLiquity` will
-- have voter1 receive nVotes from voter2, and give nLiquity tokens to
-- voter2.  All vote and liquidity token balances must remain
-- positive.
--
tradeVotes :: VoterId -> VoterId -> IssueId -> VoteTokens -> LiquidityTokens -> State -> State
tradeVotes vid1 vid2 iid voteCount tokens
    = over (issues . melem iid . votes. melem vid1 . availableVotes) (\v -> checkGEZero (v+voteCount))
    . over (issues . melem iid . votes . melem vid2 . availableVotes) (\v -> checkGEZero (v-voteCount))
    . over (voters . melem vid1 . liquidityTokens) (\v -> checkGEZero (v-tokens))
    . over (voters . melem vid2 . liquidityTokens) (\v -> checkGEZero (v+tokens))

-- | Mark an issue as completed, and accumulate the voting results.
--
endIssue :: IssueId -> State -> (State,VoteResult)
endIssue iid state  = (state',result)
  where
    issueState = view (issues . melem iid) state
    state' = over issues (M.delete iid) state
    result = VoteResult votesFor votesAgainst votesAbstained
    allVotes = M.elems (view votes issueState)
    votesFor = sum [votes | (VoterIssueState votes InFavour) <- allVotes]
    votesAgainst = sum [votes | (VoterIssueState votes Against) <- allVotes]
    votesAbstained = sum [votes | (VoterIssueState votes Abstained) <- allVotes]

-- | Distribute a new batch of liquidity tokens to voters.
--
-- Each voter will receive a fraction of the tokens according to
-- her/his fraction of the current voting pool
--    
distributeLiquidity :: LiquidityTokens -> State -> State
distributeLiquidity  toShare state = over voters (M.map updateVoter) state
  where
    nTotalVotes = sum [view votesPerIssue v | v <- M.elems (view voters state)]
    updateVoter vs = over liquidityTokens (addTokens (view votesPerIssue vs) nTotalVotes toShare) vs

    addTokens (VoteTokens num) (VoteTokens denom) (LiquidityTokens toShare) (LiquidityTokens existing)
      = LiquidityTokens (existing + (toShare * num) `div` denom)

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

    
