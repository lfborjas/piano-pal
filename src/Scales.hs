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


-- from: https://en.wikipedia.org/wiki/Relative_key
newtype KeySignature = KeySignature { _getPitchClasses :: [PitchClass] } deriving (Eq, Show)

-- This one's a fun one: in common practice, a few major keys (and their relative minor keys)
-- are preferred over their enharmonic equivalents for standard key signatures;
-- notice that there's three extra tones in `diatonicSignatures`: this is because notationally
-- there's a preference for certain enharmonics: B's signature over Cflat,
-- and Dflat over Csharp; and Gflat and Fsharp have the same number of accidentals.
-- All the other keys not included here are "theoretical" (the enharmonic
-- is more straightforward to play/notate, for example, Dsharp vs Eflat:
-- https://www.basicmusictheory.com/d-sharp-major-key-signature,) and, as such
-- their key signature is replaced by its enharmonic's.
standardKeySignature :: PitchClass -> Maybe KeySignature
standardKeySignature pc =
  case (lookup pc diatonicSignatures) of
    Just ks -> Just ks
    Nothing -> lookupEnharmonic pc diatonicSignatures
  where
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
    lookupEnharmonic p [] = Nothing
    lookupEnharmonic p ((o,k):next) = if (isEnharmonic p o) then
                                        Just k
                                      else
                                        lookupEnharmonic p next

isMajor, isMinor :: Scale -> Bool
isMajor s = (steps s) == majorScaleSteps
isMinor s = (steps s) == minorScaleSteps || s == (harmonicMinor s) || s == (melodicMinor s)
  where
    harmonicMinor s = harmonicMinorScale $ s `pitchClassAt` Tonic
    melodicMinor  s = melodicMinorAscending $ s `pitchClassAt` Tonic

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
parallelMajor s = majorScale $ s `pitchClassAt` Tonic
parallelMinor s = minorScale $ s `pitchClassAt` Tonic


noteCount :: Scale -> Int
noteCount s =  length $ steps s

pitchClasses :: Scale -> [PitchClass]
pitchClasses s = map fst $ take (noteCount s) $ pitches s

-- diatonic scales often have seven notes and thus can afford to have one different
-- "note" per degree, vs. a note followed by its accidental
-- roughly based on: https://en.wikipedia.org/wiki/Diatonic_and_chromatic#Diatonic_scales
-- and: https://en.wikipedia.org/wiki/Key_signature
diatonicPitchClasses, diatonicPitchClasses' :: Scale -> [PitchClass]
diatonicPitchClasses s =
  case getKeySignature s of
    Just accidentals -> map (replaceEnharmonic (_getPitchClasses accidentals)) (pitchClasses s)
    Nothing          -> pitchClasses s -- it's non-diatonic (more notes, weird intervals, etc), leave alone.



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
  

diatonicPitchClasses' s =
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
stepPCs pcs (c:cs) (p:ps) =
  stepPCs (found:pcs) cs ps
  where
    found = findEnharmonic p (accidentals c)
    findEnharmonic (pc, o) a = head $ filter (isEnharmonic pc) a

-- the key signature for a melodic and harmonic minor is still
-- that which corresponds to its natural equivalent:
-- https://en.wikipedia.org/wiki/Minor_scale
getKeySignature :: Scale -> Maybe KeySignature
getKeySignature s
  | isMajor s = standardKeySignature $ root s
  | isMinor s = standardKeySignature $ root (relativeMajor s)
  | otherwise = Nothing

mode :: Scale -> String
mode s
  | isMajor s = "major"
  | isMinor s = "minor"
  | otherwise = ""


-- Given a set of possible subsitutes, and a pitch class,
-- replace the pitch class with the first of the substitutes
-- which is its enharmonic.
replaceEnharmonic :: [PitchClass] -> PitchClass -> PitchClass
replaceEnharmonic substitutes pc = if (null substitutions) then
                                     pc
                                   else
                                     head substitutions
  where substitutions = filter (isEnharmonic pc) substitutes

isEnharmonic :: PitchClass -> PitchClass -> Bool
isEnharmonic a b = pitchA == pitchB
  where pitchA = absPitch (a,0) `mod` 12
        pitchB = absPitch (b,0) `mod` 12

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
