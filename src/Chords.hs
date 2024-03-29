module Chords where

import Euterpea
import Scales
import Data.Char (toLower)
import Data.List (intercalate, elemIndex)



-- From: https://en.wikipedia.org/wiki/Chord_(music)#Examples



-- TODO: a chord is more than an array of pitches:
-- https://en.wikipedia.org/wiki/Chord_(music)#Characteristics
-- e.g. (Dominant, majorTriad) == V
-- (Submediant, minorTriad) == vi
data Chord = Chord { intervals :: [Interval]
                   , degree    :: Degree
                   , scale     :: Scale
                   } deriving (Show, Eq)


-- TODO: add ability to cycle infinitely!
-- Given a chord, return the pitches corresponding to it
pitches :: Chord -> [Pitch]
pitches c@(Chord intervals degree scale) =
  map (stepInterval start) intervals
  where
    start = degreePitch scale degree

treblePitches, bassPitches, pitches' :: Chord -> [Pitch]
treblePitches c = movePitches 4 $ Chords.pitches c -- start on the same octave as the treble clef (centered around G4)
bassPitches   c = movePitches 2 $ Chords.pitches c-- start on the lowest non-ledgered C in the bass cleff (C2)
pitches'        = Chords.treblePitches -- by default, prefer the treble clef

-- Given a fully configured chord, return a common name
chordName :: Chord -> String
chordName c@(Chord intervals d s) =
  (show pc) ++ (mkName intervals)
  where
    pc = degreePitchClass s d
    mkName i
      | i == majorTriad = ""
      | i == majorSixth = "maj6"
      | i == domSeventh = "dom7"
      | i == majSeventh = "maj7"
      | i == augTriad   = "aug"
      | i == augSeventh = "aug7"
      | i == minorTriad = "min"
      | i == minorSixth = "min6"
      | i == minorSeventh = "min7"
      | i == minMajSeventh = "min/maj7"
      | i == dimTriad = "dim"
      | i == dimSeventh = "dim7"
      | i == halfDimSeventh = "/07"
      | otherwise = concat ["[", (intercalate ", " (map show i)), "]"]

degreeName :: Chord -> String
degreeName c@(Chord intervals degree _) =
  decorate intervals dName
  where
    dName = degreeNumbers !! fromEnum degree
    degreeNumbers = ["I", "II", "III", "IV", "V", "VI", "VII"]
    decorate i dn
      | i == majorTriad = dn
      | i == minorTriad = map toLower dn
      | i == augTriad   = dn ++ "+"
      | i == dimTriad   = (map toLower dn) ++ "0"
      | otherwise = dn

-- Useful representation of a chord, with
-- both its "absolute" name and the degree
-- relative to its producing scale:
-- λ> cMajScale = majorScale $ C
-- λ> cMin = Chord minorTriad Tonic cMajScale
-- λ> label cMin
-- Cmin (i)
label :: Chord -> String
label c = (chordName c) ++ " (" ++ (degreeName c) ++ ")"

-- Given a set of pitches and a scale, try to guess the chord they imply/make
-- e.g.
-- λ> cm = minorScale C
-- λ> cmI = Chord minorTriad Tonic cm
-- λ> cmI
-- Chord {intervals = [PerfectUnison,MinorThird,PerfectFifth], degree = Tonic, scale = C D Ds F G Gs As}
-- λ> label cmI
-- "Cmin (i)"
-- λ> fromPitches [(C,5), (Ef, 5), (G, 5)] cm
-- Just (Chord {intervals = [PerfectUnison,MinorThird,PerfectFifth], degree = Tonic, scale = C D Ds F G Gs As})
-- λ> label $ fromJust $ fromPitches [(C,5), (Ef, 5), (G, 5)] cm
-- "Cmin (i)"

fromPitches :: [Pitch] -> Scale -> Maybe Chord
fromPitches ps scale =
  let
    ps' = normalizePitches ps
  in
    case ((head ps') `elemIndex` (Scales.pitches scale)) of
      Just degreeN -> Just $ Chord (getIntervals ps' scale) (toEnum degreeN) scale
      Nothing -> Nothing

getIntervals :: [Pitch] -> Scale -> [Interval]
getIntervals ps scale =
  let
    rootAP = absPitch ((root scale), 0)
  in
    map (toEnum . (subtract rootAP) . absPitch) ps
  
-- Given a starting pitch and an interval, produce the pitch
-- found at the end of that interval
stepInterval :: Pitch -> Interval -> Pitch
stepInterval start interval = pitch (startAP + intervalAP)
  where
    startAP    = absPitch start
    intervalAP = fromEnum interval

-- Given a chord's vector of pitches, return a playable version
-- e.g.
-- λ> toMusic qn $ Chord majorTriad Tonic cMajScale
-- Prim (Note (1 % 4) (C,4)) :=: (Prim (Note (1 % 4) (E,4)) :=: (Prim (Note (1 % 4) (G,4)) :=: Prim (Rest (0 % 1))))
toMusic :: Dur -> Chord -> Music Pitch
toMusic d c = chord notes
  where
    notes = map (note d) (Chords.pitches' c)

-- Given a duration for all chords, a scale and a number of "chord constructors"
-- generate a progression
-- e.g.
-- λ> cMajScale = minorScale C
-- λ> toProgression qn cMajScale [_I, _ii, _V]
-- λ> play $ toProgression qn cMajScale [_I, _ii, _V]
toProgression :: Dur -> Scale -> [(Scale -> Chord)] -> Music Pitch
toProgression d scale figures =
  line chords
  where
    chords = map (Chords.toMusic d) cs
    cs     = map ($ scale) figures


-- Common Chord Components:
-- From: https://en.wikipedia.org/wiki/Chord_(music)#Examples

{-

Notice the liberal use of currying of the Chord constructor to express
both general chords and scale degree chords.

-}


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

-- TODO: add more components!

majorChord = Chord majorTriad
minorChord = Chord minorTriad

majorSixthChord = Chord majorSixth
domSeventhChord = Chord domSeventh
majSeventhChord = Chord majSeventh

augSeventhChord = Chord augSeventh

minorSixthChord = Chord minorSixth
minorSeventhChord = Chord minorSeventh
minMajSeventhChord = Chord minMajSeventh

dimSeventhChord = Chord dimSeventh
halfDimSeventhChord = Chord halfDimSeventh


-- Common tonal chords, for progressions
-- https://en.wikipedia.org/wiki/Chord_progression
-- e.g.
-- λ> cMajScale = majorScale C
-- λ> _I cMajScale
-- [(C,4),(E,4),(G,4)]
_I   = Chord majorTriad Tonic
_IV  = Chord majorTriad Subdominant
_V   = Chord majorTriad Dominant
_ii  = Chord minorTriad Supertonic
_iii = Chord minorTriad Mediant
_vi  = Chord minorTriad Submediant
