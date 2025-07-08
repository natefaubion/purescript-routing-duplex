module Test.Unit.SumOrderingTests (tests) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (parse, root, segment)
import Routing.Duplex.Generic as RDG
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

-- 'sum' parsing depends on position of constructor. To learn more read ["About ordering" section in README](#about-ordering)
tests :: Spec Unit
tests = it "sum - order of parsing is defined by ordering of constructors in definintion of datatype" do
  -- they both parse /foo, but one is more segment that the other
  parse (root $ RDG.sum { "AB1_A": segment, "AB1_B": segment }) "/1" `shouldEqual` Right (AB1_A "1")
  parse (root $ RDG.sum { "AB1_B": segment, "AB1_A": segment }) "/1" `shouldEqual` Right (AB1_A "1")

  -- now - mirror kingdom
  parse (root $ RDG.sum { "AB2_A": segment, "AB2_B": segment }) "/1" `shouldEqual` Right (AB2_B "1")
  parse (root $ RDG.sum { "AB2_B": segment, "AB2_A": segment }) "/1" `shouldEqual` Right (AB2_B "1")

--------------------
data AB1 = AB1_A String | AB1_B String

derive instance Eq (AB1)
derive instance Generic (AB1) _
instance Show (AB1) where
  show = genericShow

----------- same like previous but constructors order definition is reversed
data AB2 = AB2_B String | AB2_A String

derive instance Eq (AB2)
derive instance Generic (AB2) _
instance Show (AB2) where
  show = genericShow
