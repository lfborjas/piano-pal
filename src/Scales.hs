module Scales where

import Euterpea
import Data.List (intercalate, nubBy, sortBy)

-- these are actually just aliases for some main intervals:
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
data Step = Half
          | Whole
          | AugmentedSecond
          deriving (Show, Eq, Ord)

-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
-- And: https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
data Interval = PerfectUnison   -- DiminishedSecond
              | MinorSecond     -- AugmentedUnison
              | MajorSecond     -- DiminishedThird
              | MinorThird      -- AugmentedSecond
              | MajorThird      -- DiminishedFourth
              | PerfectFourth   -- AugmentedThird
              | DiminishedFifth -- AugmentedFourth
              | PerfectFifth    -- DiminishedSixth
              | MinorSixth      -- AugmentedFifth
              | MajorSixth      -- DiminishedSeventh
              | MinorSeventh    -- AugmentedSixth
              | MajorSeventh    -- DiminishedOctave
              | PerfectOctave   -- AugmentedSeventh | DiminishedNinth
              | MinorNinth      -- AugmentedOctave
              | MajorNinth      -- DiminishedTenth
              | MinorTenth      -- AugmentedNinth
              | MajorTenth      -- DiminishedEleventh
              | PerfectEleventh -- AugmentedTenth
              | DiminishedTwelfth -- AugmentedEleventh
              | PerfectTwelfth  -- Tritave | DiminishedThirteenth
              | MinorThirteenth -- AugmentedTwelfth
              | MajorThirteenth -- DiminishedFourteenth
              | MinorFourteenth -- AugmentedThirteenth
              | MajorFourteenth -- DiminishedFifteenth
              | DoubleOctave    -- PerfectFifteenth | AugmentedFourteenth
              -- | AugmentedFifteenth -- We're stopping at 24 semitones (a double octave) since
              -- scales/chords with larger intervals seldom occur
              deriving (Show, Eq, Ord, Enum)

data Degree = Tonic       -- I
            | Supertonic  -- II
            | Mediant     -- III
            | Subdominant -- IV
            | Dominant    -- V
            | Submediant  -- VI
            | LeadingTone -- VII (Subtonic in the natural minor scale)
            deriving (Show, Eq, Ord, Enum)

data Scale = Scale { steps :: [Interval]
                   , root  :: PitchClass
                   } deriving (Eq, Ord)

instance Show Scale where
  show s = intercalate " " $ map show $ diatonicPitchClasses s


-- convenience function to translate some very common intervals
fromStep :: Step -> Interval
fromStep Half            = MinorSecond
fromStep Whole           = MajorSecond
fromStep AugmentedSecond = MinorThird

noteCount :: Scale -> Int
noteCount s =  length $ steps s

-- | Operations on Pitch Classes, meant for inspection/reasoning/notation
-- about scales; see the operations on Pitches below if you're more
-- interested in the sound (vs. the looks) of a scale.

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
stepPCs pcs (c:cs) ((pc,_):ps) = case (findEnharmonic pc $ accidentals c) of
  Nothing    -> Nothing
  Just found -> stepPCs (found:pcs) cs ps

-- Given a pitch class and set of possible matches, return the first enharmonic
findEnharmonic :: PitchClass -> [PitchClass] -> Maybe PitchClass
findEnharmonic pc a =
  let
    enharmonics = filter (isEnharmonic pc) a
  in
    if (not $ null enharmonics) then
      Just $ head enharmonics
    else
      Nothing

degreePitchClass :: Scale -> Degree -> PitchClass
degreePitchClass scale degree =
  (diatonicPitchClasses scale) !! (fromEnum degree)

pitchClassAt = degreePitchClass

-- | Operations on pitches: purely musical (sound) values, which means that
-- when inspected, it's not guaranteed they'll be the same pitch class
-- one is expecting, but it may be an enharmonic -- not to use for theoretical
-- inspection! But rather for performance or mathematical transformations
-- (such as calculating the right pitch classes for a scale, as used above)

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
    intervals = map fromEnum (steps s)
    start     = ((root s), 0)


-- Assumes that the scale provides notes up to the octave (i.e. the repeat of the root)
-- any repeated pitches beyond the octave are ignored as a simple cycle of the scale.
-- Pitches may not be provided in order, but they'll be ordered by ascending frequency
-- when determining the steps of the scale.
-- e.g.
-- fromPitches [(C,1), (D,1), (E,1), (F,1), (G,1), (A,1), (B,1), (C,2), (D,2)]
-- C D E F G A B
fromPitches :: [Pitch] -> Scale
fromPitches []  = error "Can't derive scale from empty pitch list"
fromPitches [x] = error "One note does not a scale make!"
fromPitches ps = Scale {root=r, steps=intervals}
  where (r,_)         = head ps
        intervals     = deriveIntervals $ uniquePitches $ sortPitches ps
        -- helper functions we could break out:
        uniquePitches  (oct:dp) = oct:(nubBy  (\(pc1, _) (pc2, _) -> pc1 == pc2) dp)
        sortPitches    up = sortBy (\a b -> absPitch a `compare` absPitch b) up
        deriveIntervals s = [ toEnum ((absPitch b - absPitch a) `mod` 24) :: Interval
                            | (a,b) <- zip s $ tail s ] -- make a list of pairs

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
-- λ> cMajScale = majorScale C
-- λ> toMusic en cMajScale
-- Prim (Note (1 % 8) (C,4)) :+: (Prim (Note (1 % 8) (D,4)) :+: (Prim (Note (1 % 8) (E,4)) :+: (Prim (Note (1 % 8) (F,4)) :+: (Prim (Note (1 % 8) (G,4)) :+: (Prim (Note (1 % 8) (A,4)) :+: (Prim (Note (1 % 8) (B,4)) :+: (Prim (Note (1 % 8) (C,5)) :+: Prim (Rest (0 % 1)))))))))
-- λ> play $ toMusic en cMajScale
toMusic :: Dur -> Scale -> Music Pitch
toMusic d scale = line notes
  where
    notes = map (note d) $ take n $ pitches' scale
    n     = noteCount scale

-- Given a lowest and highest pitches, check if a provided
-- music value's pitch is within those bounds.
inRange :: Pitch -> Pitch -> Pitch -> Bool
inRange low high p =
  (absPitch p) >= (absPitch low) && (absPitch p) <= (absPitch high)

-- Generates a finite list of notes that would fit in a piano
pianoScale :: Scale -> [Pitch]
pianoScale scale =
  takeWhile (inRange lowestPitch highestPitch) (pitches scale)
  where
    lowestPitch  = (A,0)
    highestPitch = (C,8)

-- | Query functions/default scales
asIntervals = map fromStep
majorScaleSteps = asIntervals [Whole,Whole,Half,Whole,Whole,Whole,Half]
minorScaleSteps = asIntervals [Whole,Half,Whole,Whole,Half,Whole,Whole]

majorScale, minorScale, harmonicMinorScale,
  melodicMinorAscending, melodicMinorDescending :: PitchClass -> Scale
majorScale             = Scale majorScaleSteps
minorScale             = Scale minorScaleSteps
harmonicMinorScale     = Scale $ asIntervals [Whole,Half,Whole,Whole,Half,AugmentedSecond,Half]
melodicMinorAscending  = Scale $ asIntervals [Whole,Half,Whole,Whole,Whole,Whole,Half]
melodicMinorDescending = minorScale

isMajor, isMinor :: Scale -> Bool
isMajor s = (steps s) == majorScaleSteps
isMinor s = (steps s) == minorScaleSteps || s == (harmonicMinor s) || s == (melodicMinor s)
  where
    harmonicMinor s = harmonicMinorScale $ root s
    melodicMinor  s = melodicMinorAscending $ root s

mode :: Scale -> String
mode s
  | isMajor s = "major"
  | isMinor s = "minor"
  | otherwise = ""

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
