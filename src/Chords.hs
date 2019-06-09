module Chords where

import Euterpea
import Scales

data Degree = Tonic       -- I
            | Supertonic  -- II
            | Mediant     -- III
            | Subdominant -- IV
            | Dominant    -- V
            | Submediant  -- VI
            | LeadingTone -- VII (Subtonic in the natural minor scale)
            deriving (Show, Eq, Ord, Enum)

-- From: https://en.wikipedia.org/wiki/Chord_(music)#Examples
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
              | PerfectOctave   -- AugmentedSeventh
              | DiminishedNinth  
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
              | AugmentedFifteenth
              deriving (Show, Eq, Ord, Enum)


-- TODO: a chord is more than an array of pitches:
-- https://en.wikipedia.org/wiki/Chord_(music)#Characteristics
-- e.g. (Dominant, majorTriad) == V
-- (Submediant, minorTriad) == vi
data Chord = Chord Degree [Interval]

-- Given a starting pitch and an interval, produce the pitch
-- found at the end of that interval
stepInterval :: Pitch -> Interval -> Pitch
stepInterval start interval = pitch (startAP + intervalAP)
  where
    startAP    = absPitch start
    intervalAP = fromEnum interval

mkChord :: [Interval] -> Pitch -> [Pitch]
mkChord intervals root = map (stepInterval root) intervals

-- Given a chord representation, return a playable version
-- e.g.
-- λ> toMusic qn $ majorTriad (C,4)
-- Prim (Note (1 % 4) (C,4)) :=: (Prim (Note (1 % 4) (E,4)) :=: (Prim (Note (1 % 4) (G,4)) :=: Prim (Rest (0 % 1))))
toMusic :: Dur -> [Pitch] -> Music Pitch
toMusic d c = chord notes
  where
    notes = map (note d) c

-- Given a duration for all chords, a scale and a number of "chord constructors"
-- generate a progression
-- e.g.

toProgression :: Dur -> Scale -> [(Scale -> [Pitch])] -> Music Pitch
toProgression d scale figures =
  line chords
  where
    chords = map (toMusic d) cs
    cs     = map ($ scale) figures


-- Given a scale and a chord description, returns the notes in that chord
-- for that scale
-- TODO: refactor scales to work with Pitches, not Music Pitches
-- TODO: should work with notes within the scale, not just
-- with the major scale of the given degree!!
tonalChord :: Chord -> Scale -> [Pitch]
tonalChord (Chord degree intervals) scale =
  mkChord intervals start
  where
    (Prim (Note _ start)) = (scaleOctave scale) !! (fromEnum degree)

-- Common Chords:
-- From: https://en.wikipedia.org/wiki/Chord_(music)#Examples


majorTriad = [PerfectUnison, MajorThird, PerfectFifth]
majorSixth = majorTriad ++ [MajorSixth]
domSeventh = majorTriad ++ [MinorSeventh]
majSeventh = majorTriad ++ [MajorSeventh]

augTriad   = [PerfectUnison, MajorThird, MinorSixth] -- MinorSixth == AugmentedFifth
augSeventh = augTriad ++ [MinorSeventh]

minorTriad = [PerfectUnison, MinorThird, PerfectFifth]
minorSixth = minorTriad ++ [MajorSixth]
minorSeventh = minorTriad ++ [MinorSeventh]
minMajSeventh = minorTriad ++ [MajorSeventh]

dimTriad = [PerfectUnison, MinorThird, DiminishedFifth]
dimSeventh = dimTriad ++ [MajorSixth] -- MajorSixth == DiminishedSeventh
halfDimSeventh = dimTriad ++ [MinorSeventh]

-- TODO: add more chords!

majorChord = mkChord majorTriad
minorChord = mkChord minorTriad

majorSixthChord = mkChord majorSixth
domSeventhChord = mkChord domSeventh
majSeventhChord = mkChord majSeventh

augSeventhChord = mkChord augSeventh

minorSixthChord = mkChord minorSixth
minorSeventhChord = mkChord minorSeventh
minMajSeventhChord = mkChord minMajSeventh

dimSeventhChord = mkChord dimSeventh
halfDimSeventhChord = mkChord halfDimSeventh


-- Common tonal chords, for progressions
-- https://en.wikipedia.org/wiki/Chord_progression
-- e.g.
-- λ> cMajScale = majorScale $ c 4 qn
-- λ> _I cMajScale
-- [(C,4),(E,4),(G,4)]
_I   = tonalChord $ Chord Tonic majorTriad
_IV  = tonalChord $ Chord Subdominant majorTriad
_V   = tonalChord $ Chord Dominant majorTriad
_ii  = tonalChord $ Chord Supertonic minorTriad
_iii = tonalChord $ Chord Mediant minorTriad
_vi  = tonalChord $ Chord Submediant minorTriad
