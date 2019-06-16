module Scales where

import Euterpea
import Data.List (intercalate, lookup)

-- these are actually just aliases for some main intervals:
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
data Step = Half
          | Whole
          | AugmentedSecond
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
  show s = intercalate " " $ map show $ diatonicPitchClasses s


isMajor, isMinor :: Scale -> Bool
isMajor s = (steps s) == majorScaleSteps
isMinor s = (steps s) == minorScaleSteps || s == (harmonicMinor s) || s == (melodicMinor s)
  where
    harmonicMinor s = harmonicMinorScale $ root s
    melodicMinor  s = melodicMinorAscending $ root s

-- | Relationships between scales:
-- https://en.wikipedia.org/wiki/Closely_related_key

supertonicRelative, mediantRelative, subdominantRelative,
  dominantRelative, submediantRelative, relativeMinor :: Scale -> Scale

-- | CLOSE KEYS FOR MAJOR SCALES
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

-- | CLOSE KEYS FOR MINOR SCALES
relativeMajor       s = majorScale $ s `pitchClassAt` Mediant

-- from: https://en.wikipedia.org/wiki/Parallel_key
parallelMajor, parallelMinor :: Scale -> Scale
parallelMajor s = majorScale $ (root s)
parallelMinor s = minorScale $ (root s)


noteCount :: Scale -> Int
noteCount s =  length $ steps s

pitchClasses :: Scale -> [PitchClass]
pitchClasses s = map fst $ take (noteCount s) $ pitches s

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

rotateAround p ps@(x@(q,_):xs)
  | (natural p) == q = ps
  | otherwise        = rotateAround p (xs ++ [x])

isEnharmonic :: PitchClass -> PitchClass -> Bool
isEnharmonic a b = pitchA == pitchB
  where pitchA = absPitch (a,0) `mod` 12
        pitchB = absPitch (b,0) `mod` 12
  
-- diatonic scales often have seven notes and thus can afford to have one different
-- "note" per degree, vs. a note followed by its accidental
-- roughly based on: https://en.wikipedia.org/wiki/Diatonic_and_chromatic#Diatonic_scales
-- and: https://en.wikipedia.org/wiki/Key_signature
diatonicPitchClasses :: Scale -> [PitchClass]
diatonicPitchClasses s =
  let
    r = root s
    candidates = tail $ map fst $ rotateAround (root s) accidentalsMap
  in
    case (stepPCs [r] candidates (tail $ notes s)) of
      Nothing  -> pitchClasses s
      Just pcs -> pcs

-- rotate through the naturals starting at the note's natural
-- and then exhaust that list; instead of starting from scratch.
stepPCs ::
  [PitchClass]       -> -- accumulator (pitch classes found so far)
  [PitchClass]       -> -- candidate pitch classes
  [Pitch]            -> -- pitches of the scale yet to find a pitch class for
  Maybe [PitchClass] -- if there's enough unique pitch classes, return; if not, give up
stepPCs _ (x:xs) []   = Nothing -- ran out of pitches, still have candidates
stepPCs _   [] (y:ys) = Nothing -- ran out of candidates, still have pitches
stepPCs pcs []   []   = Just $ reverse pcs -- seems like we exhausted both lists and found something!
stepPCs pcs (c:cs) (p:ps) = case (findEnharmonic p $ accidentals c) of
  Nothing    -> Nothing
  Just found -> stepPCs (found:pcs) cs ps
  where
    found = findEnharmonic p (accidentals c)
    findEnharmonic (pc, o) a =
      let
        enharmonics = filter (isEnharmonic pc) a
      in
        if (not $ null enharmonics) then
          Just $ head enharmonics
        else
          Nothing

mode :: Scale -> String
mode s
  | isMajor s = "major"
  | isMinor s = "minor"
  | otherwise = ""

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

majorScaleSteps = [Whole,Whole,Half,Whole,Whole,Whole,Half]
minorScaleSteps = [Whole,Half,Whole,Whole,Half,Whole,Whole]

majorScale, minorScale, harmonicMinorScale,
  melodicMinorAscending, melodicMinorDescending :: PitchClass -> Scale
majorScale             = Scale majorScaleSteps
minorScale             = Scale minorScaleSteps
harmonicMinorScale     = Scale [Whole,Half,Whole,Whole,Half,AugmentedSecond,Half]
melodicMinorAscending  = Scale [Whole,Half,Whole,Whole,Whole,Whole,Half]
melodicMinorDescending = minorScale
