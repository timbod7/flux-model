{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Flux where

import Data.Maybe(fromMaybe)
import Control.Lens(view,over,set,makeLenses,lens,Lens')
import Data.Ratio((%), Rational, numerator, denominator)

import qualified Data.Map as M
import qualified Data.Set as S

newtype VoterId = VoterId Int deriving (Show,Eq,Ord,Enum)
newtype IssueId = IssueId Int deriving (Show,Eq,Ord,Enum)

-- | A quantity of votes for a particular issue.
--
-- A fixed pool of vote tokens are created for each issue. The vote tokens can
-- only be used to vote _for that issue_, or they can be be traded with other
-- votes in exchange for liquidity tokens.
newtype VoteTokens = VoteTokens Int deriving (Show,Eq,Ord,Num,Enum,Integral,Real)

-- | A quantity of liquidity tokens.
--
-- Liquidity tokens are the medium of exchange by which vote tokens
-- can be traded between votes. 
newtype LiquidityTokens = LiquidityTokens Int deriving (Show,Eq,Ord,Num,Enum,Integral,Real)

-- | The overall system state, consisting of information about
-- each voter, and about each issue.
data State = State {
  _voters :: M.Map VoterId VoterState,
  _issues :: M.Map IssueId IssueState
  }

-- | The state associated with each voter
data VoterState = VoterState {
  -- | How many votes this voter will receive for each new issue
  _votesPerIssue :: VoteTokens,

  -- | The number of liquidity tokens currently held
  _liquidityTokens :: LiquidityTokens,

  -- | How votes should be delegated
  _delegation :: M.Map VoterId Rational
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
-- For each new issue the voter will receive `nVotes`
-- vote tokens. For most voters `nVotes` will be 1, though
-- political parties will receive more votes where their
-- preferences have contributed to the election of a flux
-- senator.
--
newVoter :: VoteTokens -> State -> (State,VoterId)
newVoter nVotes state = (state',vid)
  where
    state' = over voters (M.insert vid details) state
    vid = nextKey (view voters state)
    details = VoterState nVotes 0 M.empty

-- | Create a new issue. Currently registered voters
-- receive their allocated votes for the issue, and their
-- vote on this issue will default to abstain.
--    
newIssue :: State -> (State,IssueId)
newIssue state = (state,iid)
  where
    state' = over issues (M.insert iid details) state
    iid = nextKey (view issues state)
    details = IssueState (M.fromList [ (vid,VoterIssueState nvotes Abstained)
                                     | (vid,nvotes) <- M.toList votesPostDelegation])

    votesPreDelegation :: M.Map VoterId VoteTokens
    votesPreDelegation = M.fromList [ (vid,view votesPerIssue vs) | (vid,vs) <- M.toList (view voters state)]

    votesPostDelegation :: M.Map VoterId VoteTokens
    votesPostDelegation = delegateVotes delegationGraph votesPreDelegation

    delegationGraph :: DelGraph
    delegationGraph = foldr (uncurry graphAdd) emptyGraph $
      [ (vid,view delegation vs) | (vid,vs) <- M.toList (view voters state) ]

-- | Set the vote for a voter on a given issue
--
-- Subsequent calls to this function will override previous calls: the
-- last request prior to the issue ending will be used.
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

    updateVoter vs = over liquidityTokens ((+) (scale toShare ratio)) vs
      where
        ratio = mkRational (view votesPerIssue vs) nTotalVotes

type VoteAllocation = M.Map VoterId VoteTokens

-- | Given a graph of the delegation requests between voters, and
-- the votes available to each voter, calculate the final distribution
-- of votes.
--
-- Notes:
--   * The graph must be acyclic.
--   * Any residual, undelegated vote remains with the original
--     voter
--
delegateVotes :: DelGraph -> VoteAllocation -> VoteAllocation
delegateVotes  g alloc
  | graphSize g == 0 = alloc
  | otherwise = let (g',alloc') = delegateStep g alloc
                in if graphSize g' == graphSize g
                   then error "step made no progress - graph contains cycles"
                   else delegateVotes g' alloc'
  where                         
    -- Each step we process the "leaf" voters, ie those that don't
    -- have any votes delegated to them. Assuming there are no cycles
    -- in the graph, then each step will produce new leaf voters until
    -- the graph is empty.
    delegateStep :: DelGraph -> VoteAllocation -> (DelGraph,VoteAllocation)
    delegateStep g alloc = (g',alloc')
      where
        leafVoters = S.difference (M.keysSet (delegationsTo g)) (M.keysSet (delegationsFrom g))
        alloc' = foldr distributeVotes alloc (S.toList leafVoters)
        g' = S.foldr graphRemove g leafVoters

        distributeVotes :: VoterId -> VoteAllocation -> VoteAllocation
        distributeVotes vid alloc = M.unionWith (+) toAdd $ M.unionWith (-) toRemove $ alloc
          where
            availableVotes :: VoteTokens
            availableVotes = fromMaybe 0 (M.lookup vid alloc)
        
            distribRatios :: M.Map VoterId Rational
            distribRatios = fromMaybe M.empty (M.lookup vid (delegationsTo g))

            toAdd,toRemove:: VoteAllocation
            toAdd = M.map (scale availableVotes) distribRatios
            toRemove = M.singleton vid (sum (M.elems toAdd))

----------------------------------------------------------------------
-- Directed Acyclic Graph structure for representing delegated votes
--
-- Invariants:
--    * The keyset of the delegationsTo map is the set of valid voters.
--      ie the values in the delegationsTo map, and the keys and values of
--      the delegationsFrom map must all be valid.
--    * The values of the delegationsFrom map must never be empty.
--    * The graph must be acyclic            
            

data DelGraph = DelGraph {
  delegationsFrom :: M.Map VoterId (S.Set VoterId),
  delegationsTo :: M.Map VoterId (M.Map VoterId Rational)
  }

emptyGraph :: DelGraph
emptyGraph = DelGraph M.empty M.empty

graphAdd :: VoterId -> (M.Map VoterId Rational) -> DelGraph -> DelGraph
graphAdd vid delegation g@(DelGraph from to)
  | M.member vid to = graphAdd vid delegation (graphRemove vid g)
  | M.null delegation = g
  | otherwise = DelGraph from' to'
  where
    from' = undefined
    to' = M.insert vid delegation to

graphRemove :: VoterId -> DelGraph -> DelGraph
graphRemove vid g@(DelGraph from to) =
  case M.lookup vid to of
    Nothing -> g
    (Just delegations) -> 
      let from' = foldr (removeFrom vid) from (M.keys delegations)
          to' = M.delete vid to
      in DelGraph from' to'
  where
    removeFrom :: VoterId -> VoterId -> M.Map VoterId (S.Set VoterId) -> M.Map VoterId (S.Set VoterId)
    removeFrom vid0 vid m = case M.lookup vid m of
      Nothing -> m
      (Just s) ->
        let s' = S.delete vid0 s
        in if S.null s' then M.delete vid m else M.insert vid s' m

graphSize :: DelGraph -> Int
graphSize g = M.size (delegationsTo g)

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

mkRational :: (Integral a) => a -> a -> Rational
mkRational v1 v2 = fromIntegral v1 % fromIntegral v2

scale :: Integral a => a-> Rational -> a
scale a rat = (a * fromIntegral (numerator rat)) `div` fromIntegral (denominator rat)

