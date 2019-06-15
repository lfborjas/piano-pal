module Scales where

import Euterpea
import Data.List (intercalate, lookup)

data Step = Half
          | Whole
          deriving (Show, Eq, Ord, Enum)

data Degree = Tonic       -- I
            | Supertonic  -- II
            | Mediant     -- III
            | Subdominant -- IV
            | Dominant    -- V
            | Submediant  -- VI
            | LeadingTone -- VII (Subtonic in the natural minor scale)
            deriving (Show, Eq, Ord, Enum)

data Scale = Scale { steps :: [Step]
                   , root  :: PitchClass
                   } deriving (Eq, Ord)

instance Show Scale where
  show s = intercalate " " $ map show $ pitchClasses s


-- from: https://en.wikipedia.org/wiki/Relative_key
newtype KeySignature = KeySignature { _getPitchClasses :: [PitchClass] } deriving (Eq, Show)

diatonicSignatures :: [ (PitchClass, KeySignature) ]
-- notice that there's three extra tones: this is because notationally
-- there's a preference for certain enharmonics: B's signature over Cflat,
-- and Dflat over Csharp; and Gflat and Fsharp have the same number of accidentals.
-- All the other keys not included here are "theoretical" (the enharmonic
-- is more straightforward to play/notate, for example, Dsharp vs Eflat:
-- https://www.basicmusictheory.com/d-sharp-major-key-signature)
diatonicSignatures  = [ (Cf, Scales.KeySignature [Bf, Ef, Af, Df, Gf, Cf, Ff])
                      , (Gf, Scales.KeySignature [Bf, Ef, Af, Df, Gf, Cf])
                      , (Df, Scales.KeySignature [Bf, Ef, Af, Df, Gf])
                      , (Af, Scales.KeySignature [Bf, Ef, Af, Df])
                      , (Ef, Scales.KeySignature [Bf, Ef, Af])
                      , (Bf, Scales.KeySignature [Bf, Ef])
                      , (F,  Scales.KeySignature [Bf])
                      , (C,  Scales.KeySignature [])
                      , (G,  Scales.KeySignature [Fs])
                      , (D,  Scales.KeySignature [Fs, Cs])
                      , (A,  Scales.KeySignature [Fs, Cs, Gs])
                      , (E,  Scales.KeySignature [Fs, Cs, Gs, Ds])
                      , (B,  Scales.KeySignature [Fs, Cs, Gs, Ds, As])
                      , (Fs, Scales.KeySignature [Fs, Cs, Gs, Ds, As, Es])
                      , (Cs, Scales.KeySignature [Fs, Cs, Gs, Ds, As, Es, Bs])
                      ]

isMajor, isMinor :: Scale -> Bool
isMajor s = s == (parallelMajor s)
isMinor s = s == (parallelMinor s)

-- | Relationships between scales:
-- https://en.wikipedia.org/wiki/Closely_related_key

supertonicRelative, mediantRelative, subdominantRelative,
  dominantRelative, submediantRelative, relativeMinor :: Scale -> Scale

-- relative minor of the subdominant
supertonicRelative  s = minorScale $ s `pitchClassAt` Supertonic
-- relative minor of the dominant
mediantRelative     s = minorScale $ s `pitchClassAt` Mediant
-- one less sharp/one more flat around the circle of fifths
subdominantRelative s = majorScale $ s `pitchClassAt` Subdominant
-- one more sharp/one fewer flat around the circle of fifths
dominantRelative    s = majorScale $ s `pitchClassAt` Dominant
-- different tonic, same key signature
submediantRelative  s = minorScale $ s `pitchClassAt` Submediant
relativeMinor         = submediantRelative

-- from: https://en.wikipedia.org/wiki/Parallel_key
parallelMajor, parallelMinor :: Scale -> Scale
parallelMajor s = majorScale $ s `pitchClassAt` Tonic
parallelMinor s = minorScale $ s `pitchClassAt` Tonic


pitchClasses :: Scale -> [PitchClass]
pitchClasses s = if (noteCount s) == 7 then
                   diatonicPitchClasses s
                 else
                   genericPitchClasses s

noteCount :: Scale -> Int
noteCount s =  length $ steps s

genericPitchClasses :: Scale -> [PitchClass]
genericPitchClasses s = map fst $ take (noteCount s) $ pitches s

-- diatonic scales often have seven notes and thus can afford to have one different
-- "note" per degree, vs. a note followed by its accidental
-- roughly based on: https://en.wikipedia.org/wiki/Diatonic_and_chromatic#Diatonic_scales
-- and: https://en.wikipedia.org/wiki/Key_signature
diatonicPitchClasses :: Scale -> [PitchClass]
diatonicPitchClasses s = case (lookup (root s) diatonicSignatures) of
  Just accidentals -> map (replaceEnharmonic accidentals) (genericPitchClasses s)
  Nothing          -> genericPitchClasses s -- it's a theoretical scale, leave alone.

replaceEnharmonic :: KeySignature -> PitchClass -> PitchClass
replaceEnharmonic substitutes pc = if (null substitutions) then
                                     pc
                                   else
                                     head substitutions
  where substitutions = filter (isEnharmonic pc) (_getPitchClasses substitutes)

isEnharmonic :: PitchClass -> PitchClass -> Bool
isEnharmonic a b = a /= b && pitchA == pitchB
  where pitchA = absPitch (a,0)
        pitchB = absPitch (b,0)

degreePitchClass :: Scale -> Degree -> PitchClass
degreePitchClass scale degree =
  (pitchClasses scale) !! (fromEnum degree)

pitchClassAt = degreePitchClass

degreePitch :: Scale -> Degree -> Pitch
degreePitch scale degree =
  pitches scale !! fromEnum degree

rootPitch  s = head $ pitches s
notes      s = take (noteCount s) $ pitches s
-- | Generates an infinite list of pitches within the provided scale
pitches :: Scale -> [Pitch]
pitches s =
  map pitch $ 
  scanl (+) (absPitch start) (cycle intervals)
  where
    intervals = map (succ . fromEnum) (steps s)
    start     = ((root s), 0)


movePitches :: Int -> [Pitch] -> [Pitch]
movePitches n ps = [(pc, o+n) | (pc, o) <- ps]

-- move an arbitrary list of pitches to a lowest common point
normalizePitches :: [Pitch] -> [Pitch]
normalizePitches ps = [(pc, 0) | (pc, _) <- ps]

treblePitches, bassPitches, pitches' :: Scale -> [Pitch]
treblePitches s = movePitches 4 $ pitches s -- start on the same octave as the treble clef (centered around G4)
bassPitches   s = movePitches 2 $ pitches s-- start on the lowest non-ledgered C in the bass cleff (C2)
pitches'        = treblePitches -- by default, prefer the treble clef



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
    notes = map (note d) $ take n $ pitches' scale
    n     = noteCount scale

-- | Given a lowest and highest pitches, check if a provided
-- music value's pitch is within those bounds.
inRange :: Pitch -> Pitch -> Pitch -> Bool
inRange low high p =
  (absPitch p) >= (absPitch low) && (absPitch p) <= (absPitch high)

-- | Generates a finite list of notes that would fit in a piano
pianoScale :: Scale -> [Pitch]
pianoScale scale =
  takeWhile (inRange lowestPitch highestPitch) (pitches scale)
  where
    lowestPitch  = (A,0)
    highestPitch = (C,8)

-- | HELPER FUNCTIONS: scale builders/slicers

majorScale :: PitchClass -> Scale
majorScale = Scale [Whole,Whole,Half,Whole,Whole,Whole,Half]

minorScale :: PitchClass -> Scale
minorScale = Scale [Whole,Half,Whole,Whole,Half,Whole,Whole]

-- TODO: add church modes, harmonic/melodic scales, etc.
