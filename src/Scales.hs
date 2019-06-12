module Scales where

import Euterpea
import Data.List (intercalate)

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
                   ,root  :: PitchClass
                   } deriving (Eq, Ord)

instance Show Scale where
  show s = intercalate " " $ map show $ pitchClasses s


accidentalsMap =  [ (C, [Cff, Cf, C, Cs, Css])
                  , (D, [Dff, Df, D, Ds, Dss])
                  , (E, [Eff, Ef, E, Es, Ess])
                  , (F, [Fff, Ff, F, Fs, Fss])
                  , (G, [Gff, Gf, G, Gs, Gss])
                  , (A, [Aff, Af, A, As, Ass])
                  , (B, [Bff, Bf, B, Bs, Bss])
                  ]

findAccidentals :: PitchClass -> (PitchClass, [PitchClass])
findAccidentals pc = head $ filter ((elem pc) . snd) accidentalsMap

natural :: PitchClass -> PitchClass
natural  = fst . findAccidentals

accidentals :: PitchClass -> [PitchClass]
accidentals = snd . findAccidentals

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
diatonicPitchClasses :: Scale -> [PitchClass]
diatonicPitchClasses s = stepPCs (genericPitchClasses s) [] []

stepPCs :: [PitchClass] -> [PitchClass] -> [PitchClass] -> [PitchClass]
stepPCs [] [] _ = []
stepPCs [] ps@(pc:pcs) nacc = reverse $ ps
stepPCs (p:ps) pcs nacc  = stepPCs ps ((resolvePC p nacc):pcs) ((natural p):nacc)

resolvePC :: PitchClass -> [PitchClass] -> PitchClass
resolvePC pc [] = pc
resolvePC pc nacc@(n:ns) =
  if (natural pc) `elem` nacc then
    findEnharmonic pc
  else
    pc
  where
    findEnharmonic pc   = head $ filter (isEnharmonic pc) (accidentals $ succ $ n)
    isEnharmonic   pc x = (absPitch (pc, 0)) == (absPitch (x,0))

degreePitchClass :: Scale -> Degree -> PitchClass
degreePitchClass scale degree =
  (pitchClasses scale) !! (fromEnum degree)

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
