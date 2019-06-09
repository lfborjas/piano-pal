module Scales where

import Euterpea

data Step = Half
          | Whole
          deriving (Show, Eq, Ord, Enum)

type Scale = [Music Pitch]

-- | Generates an infinite list of notes within the provided scale
mkScale :: [Step] -> Music Pitch -> Scale
mkScale intervals (Prim (Note d p)) =
  map (note d . pitch) $
  scanl (+) (absPitch p) (cycle intervals')
  where
    intervals' = map (succ . fromEnum) intervals

-- | Given a lowest and highest pitches, check if a provided
-- music value's pitch is within those bounds.
inRange :: Pitch -> Pitch -> Music Pitch -> Bool
inRange low high (Prim (Note d p)) =
  (absPitch p) >= (absPitch low) && (absPitch p) <= (absPitch high)

-- | Generates a finite list of notes that would fit in a piano
pianoScale :: Scale -> Scale
pianoScale scale =
  takeWhile (inRange lowestPitch highestPitch) scale
  where
    lowestPitch  = (A,0)
    highestPitch = (C,8)

-- | Given a starting note, generates a finite list of notes
-- within that scale that can be played on a piano
-- e.g  pianoScaleFrom (C,4) majorScale
-- (interactively: play $ line $  pianoScaleFrom (C,4) majorScale)
pianoScaleFrom :: Pitch -> (Music Pitch -> Scale) -> Scale
pianoScaleFrom p mkScale = pianoScale scale
  where
    scale = mkScale $ note qn p

-- | HELPER FUNCTIONS: scale builders/slicers

scaleOctave :: Scale -> Scale
scaleOctave scale = take 8 scale

scaleOctaves :: Scale -> Int -> Scale
scaleOctaves scale n = take (n*8) scale

majorScale :: Music Pitch -> Scale
majorScale = mkScale [Whole,Whole,Half,Whole,Whole,Whole,Half]

minorScale :: Music Pitch -> Scale
minorScale = mkScale [Whole,Half,Whole,Whole,Half,Whole,Whole]

-- TODO: add church modes, harmonic/melodic scales, etc.
