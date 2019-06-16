module ScalesSpec where

import Control.Monad (forM_)
import Test.Hspec
import Euterpea
import Scales


{-
  Derived these tests with some good ol' web scraping:

  Went here:

  - https://www.basicmusictheory.com/minor-scales-chart
  - https://www.basicmusictheory.com/major-scales-chart

  And ran this jquery:

  replaceAccidentals = function(s){ return s.replace(/b$/, "f").replace("#", "s")}

  jQuery("table.table-responsive tr").slice(3,18).each(
function(){
  var $this, name, notes;
  $this = $(this);
  name = replaceAccidentals($this.find("th").text().replace("natural minor scale", ""));
  notes = $this.find("td").toArray().slice(0,-1).reduce(function(a,b){
    return a.concat(replaceAccidentals(b.innerText))
  }, []);
  console.log(", ( minorScale", name, ", [", notes.toString(), "]) ")
})

  Then just copy-pasted, remove the annoying chrome linenumbers.

-}

majorScales, minorScales :: [(Scale, [PitchClass])]
majorScales = [ ( majorScale C  , [ C,D,E,F,G,A,B ]) 
              , ( majorScale G  , [ G,A,B,C,D,E,Fs ]) 
              , ( majorScale D  , [ D,E,Fs,G,A,B,Cs ]) 
              , ( majorScale A  , [ A,B,Cs,D,E,Fs,Gs ]) 
              , ( majorScale E  , [ E,Fs,Gs,A,B,Cs,Ds ]) 
              , ( majorScale B  , [ B,Cs,Ds,E,Fs,Gs,As ]) 
              , ( majorScale Fs  , [ Fs,Gs,As,B,Cs,Ds,Es ]) 
              , ( majorScale Cs  , [ Cs,Ds,Es,Fs,Gs,As,Bs ]) 
              , ( majorScale F  , [ F,G,A,Bf,C,D,E ]) 
              , ( majorScale Bf  , [ Bf,C,D,Ef,F,G,A ]) 
              , ( majorScale Ef  , [ Ef,F,G,Af,Bf,C,D ]) 
              , ( majorScale Af  , [ Af,Bf,C,Df,Ef,F,G ]) 
              , ( majorScale Df  , [ Df,Ef,F,Gf,Af,Bf,C ]) 
              , ( majorScale Gf  , [ Gf,Af,Bf,Cf,Df,Ef,F ]) 
              , ( majorScale Cf  , [ Cf,Df,Ef,Ff,Gf,Af,Bf ]) 
              ]

minorScales = [ ( minorScale A  , [ A,B,C,D,E,F,G ]) 
              , ( minorScale E  , [ E,Fs,G,A,B,C,D ]) 
              , ( minorScale B  , [ B,Cs,D,E,Fs,G,A ]) 
              , ( minorScale Fs  , [ Fs,Gs,A,B,Cs,D,E ]) 
              , ( minorScale Cs  , [ Cs,Ds,E,Fs,Gs,A,B ]) 
              , ( minorScale Gs  , [ Gs,As,B,Cs,Ds,E,Fs ]) 
              , ( minorScale Ds  , [ Ds,Es,Fs,Gs,As,B,Cs ]) 
              , ( minorScale As  , [ As,Bs,Cs,Ds,Es,Fs,Gs ]) 
              , ( minorScale D  , [ D,E,F,G,A,Bf,C ]) 
              , ( minorScale G  , [ G,A,Bf,C,D,Ef,F ]) 
              , ( minorScale C  , [ C,D,Ef,F,G,Af,Bf ]) 
              , ( minorScale F  , [ F,G,Af,Bf,C,Df,Ef ]) 
              , ( minorScale Bf  , [ Bf,C,Df,Ef,F,Gf,Af ]) 
              , ( minorScale Ef  , [ Ef,F,Gf,Af,Bf,Cf,Df ]) 
              , ( minorScale Af  , [ Af,Bf,Cf,Df,Ef,Ff,Gf ]) 
              ]

spec :: Spec
spec =  do
  -- inspired by: https://haskell-at-work.com/episodes/2018-01-13-dynamic-test-suites-in-haskell-using-hspec-and-tasty.html
  describe "Scales.diatonicPitchClasses" $ do
    forM_ (concat [majorScales, minorScales]) $ \(scale, expectedPitchClasses) -> do
      it ("constructs the "++
          (show $ root scale)++" "++
          (mode scale)++" scale as %s" ++
          ( show expectedPitchClasses )) $ do
        diatonicPitchClasses scale `shouldBe` expectedPitchClasses
      
