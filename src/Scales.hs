module Scales where

import Euterpea

data Step = Half
          | Whole
          deriving (Show, Eq, Ord, Enum)

type Scale = [Pitch]

-- | Generates an infinite list of notes within the provided scale
mkScale :: [Step] -> Pitch -> Scale
mkScale intervals p =
  map pitch $ 
  scanl (+) (absPitch p) (cycle intervals')
  where
    intervals' = map (succ . fromEnum) intervals

-- To play around with scales: produces a line of the first octave of the
-- given scale
-- e.g.
-- λ> cMajScale = majorScale (C,4)
-- λ> toMusic en cMajScale
-- Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (D,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (A,4)) :+: (Prim (Note (1 % 8) (B,4)) :+: (Prim (Note (1 % 8) (C,5)) :+: Prim (Rest (0 % 1)))))))))
-- λ> play $ toMusic en cMajScale
toMusic :: Dur -> Scale -> Music Pitch
toMusic d scale = line notes
  where
    notes = map (note d) (scaleOctave scale)

-- | Given a lowest and highest pitches, check if a provided
-- music value's pitch is within those bounds.
inRange :: Pitch -> Pitch -> Pitch -> Bool
inRange low high p =
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
pianoScaleFrom :: Pitch -> (Pitch -> Scale) -> Scale
pianoScaleFrom p scaleMaker = pianoScale scale
  where
    scale = scaleMaker p

-- | HELPER FUNCTIONS: scale builders/slicers

scaleOctave :: Scale -> Scale
scaleOctave scale = take 8 scale

scaleOctaves :: Scale -> Int -> Scale
scaleOctaves scale n = take (n*8) scale

majorScale :: Pitch -> Scale
majorScale = mkScale [Whole,Whole,Half,Whole,Whole,Whole,Half]

minorScale :: Pitch -> Scale
minorScale = mkScale [Whole,Half,Whole,Whole,Half,Whole,Whole]

-- TODO: add church modes, harmonic/melodic scales, etc.
