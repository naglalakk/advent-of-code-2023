module Test.Main where

import Prelude

import Day1 as D1
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "DAY-1 TESTS" do
    describe "getDigitsFromString" do
      it "String 'ax5yydQ4 should equal [5,4]" do
        let 
          testStr = "ax5yydQ4"
          digitArr = D1.getDigitsFromString testStr
        digitArr `shouldEqual` [5,4]
    describe "getDigitsFromString2" do
      it "oneightza59twoxp3one should equal [1,8,5,9,2,3,1]" do
        let
          testStr = "oneightza59twoxp3one"
          digitArr = D1.getDigitsFromString2 testStr
        digitArr `shouldEqual` [1,8,5,9,2,3,1]
