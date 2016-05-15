{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Flux

main = hspec $ do
  describe "The delegation graph" $
    it "Should have size 0 when empty" $ do
     graphSize emptyGraph `shouldBe` 0

    -- adding and removing a voter is a no-op

    -- adding a voter is idempotent

    -- removing a voter is idempotent

    -- graph always meets internal invariants
     
instance Arbitrary VoterId where
  arbitrary = VoterId <$> choose (0,9)

