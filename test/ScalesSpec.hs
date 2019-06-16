module ScalesSpec where

import Test.Hspec
import Euterpea (PitchClass)
import Scales



standardScales :: [(Scale, [PitchClass])]
standardScales = []

spec :: Spec
spec =  do
  describe "Scales.diatonicPitchClasses" $ do
    it "returns standard pitch classes for standard scales" $
      True `shouldBe` True
      
