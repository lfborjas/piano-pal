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

replaceAccidentals = function(s){ return s.replace(/b/g, "f").replace(/#/g, "s")}


jQuery("table.table-responsive tr").slice(19).each(
function(){
  var $this, name, notes;
  $this = $(this);
  name = replaceAccidentals($this.find("th").text().replace(" major scale", ""));
  notes = $this.find("td").toArray().slice(0,-1).reduce(function(a,b){
    return a.concat(replaceAccidentals(b.innerText))
  }, []);
  console.log(", ( majorScale", name, ", [", notes.toString(), "]) ")
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
              -- weird major scales where the enharmonic is usually preferred
              , ( majorScale Ds , [ Ds,Es,Fss,Gs,As,Bs,Css ]) 
              , ( majorScale Es , [ Es,Fss,Gss,As,Bs,Css,Dss ]) 
              , ( majorScale Ff , [ Ff,Gf,Af,Bff,Cf,Df,Ef ]) 
              , ( majorScale Gs , [ Gs,As,Bs,Cs,Ds,Es,Fss ]) 
              , ( majorScale As , [ As,Bs,Css,Ds,Es,Fss,Gss ]) 
              , ( majorScale Bs , [ Bs,Css,Dss,Es,Fss,Gss,Ass ]) 
              , ( majorScale Css , [ Css,Dss,Ess,Fss,Gss,Ass,Bss ]) 
              , ( majorScale Dff , [ Dff,Eff,Ff,Gff,Aff,Bff,Cf ]) 
              --, ( majorScale Dss , [ Dss,Ess,Fsss,Gss,Ass,Bss,Csss ]) 
              , ( majorScale Eff , [ Eff,Ff,Gf,Aff,Bff,Cf,Df ]) 
              , ( majorScale Fss , [ Fss,Gss,Ass,Bs,Css,Dss,Ess ]) 
              -- , ( majorScale Gss , [ Gss,Ass,Bss,Css,Dss,Ess,Fsss ]) 
              , ( majorScale Aff , [ Aff,Bff,Cf,Dff,Eff,Ff,Gf ]) 
              --, ( majorScale Ass , [ Ass,Bss,Csss,Dss,Ess,Fsss,Gsss ]) 
              , ( majorScale Bff , [ Bff,Cf,Df,Eff,Ff,Gf,Af ]) 
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
              -- weird scales that don't usually show up (enharmonics are preferred:)
              , ( minorScale Df , [ Df,Ef,Ff,Gf,Af,Bff,Cf ]) 
              , ( minorScale Es , [ Es,Fss,Gs,As,Bs,Cs,Ds ]) 
              , ( minorScale Ff , [ Ff,Gf,Aff,Bff,Cf,Dff,Eff ]) 
              , ( minorScale Gf , [ Gf,Af,Bff,Cf,Df,Eff,Ff ]) 
              , ( minorScale Bs , [ Bs,Css,Ds,Es,Fss,Gs,As ]) 
              , ( minorScale Cf , [ Cf,Df,Eff,Ff,Gf,Aff,Bff ]) 
              , ( minorScale Css , [ Css,Dss,Es,Fss,Gss,As,Bs ])
              -- The Dff scale has a triple flat (B), which doesn't exist in Euterpea
              -- , ( minorScale Dff , [ Dff,Eff,Fff,Gff,Aff,Bfff,Cff ]) 
              , ( minorScale Dss , [ Dss,Ess,Fss,Gss,Ass,Bs,Css ]) 
              , ( minorScale Eff , [ Eff,Ff,Gff,Aff,Bff,Cff,Dff ]) 
              , ( minorScale Fss , [ Fss,Gss,As,Bs,Css,Ds,Es ]) 
              , ( minorScale Gss , [ Gss,Ass,Bs,Css,Dss,Es,Fss ]) 
              , ( minorScale Aff , [ Aff,Bff,Cff,Dff,Eff,Fff,Gff ]) 
              , ( minorScale Ass , [ Ass,Bss,Css,Dss,Ess,Fss,Gss ]) 
              , ( minorScale Bff , [ Bff,Cf,Dff,Eff,Ff,Gff,Aff ]) 
              ]

-- These scales contain triple accidentals which, while valid in music, aren't supported in Euterpea
-- considering that they're rare enough, I'm choosing to let them have repeated pitch classes
-- vs. upheaving the Euterpea approach
unsupportedScales = [ ( majorScale Dss , [ E,Fs,Gs,A,B,Cs,Ds ])
                    , ( majorScale Gss , [ A,B,Cs,D,E,Fs,Gs])
                    , ( majorScale Ass , [ B,Cs,Ds,E,Fs,Gs,As])
                    , ( minorScale Dff , [ C,D,Ds,F,G,Gs,As])
                    ]

spec :: Spec
spec =  do
  -- inspired by: https://haskell-at-work.com/episodes/2018-01-13-dynamic-test-suites-in-haskell-using-hspec-and-tasty.html
  describe "Scales.diatonicPitchClasses" $ do
    forM_ (concat [majorScales, minorScales, unsupportedScales]) $ \(scale, expectedPitchClasses) -> do
      it ("constructs the "++
          (show $ root scale)++" "++
          (mode scale)++" scale as %s" ++
          ( show expectedPitchClasses )) $ do
        diatonicPitchClasses scale `shouldBe` expectedPitchClasses
      
