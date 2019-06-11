{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Render where

import Euterpea
import Scales
import Chords
import Data.Ratio
import Data.Char (toLower)
import Data.List (intercalate)
import System.Process (callCommand)



type ScoreFragment = String
data Score = Score { clef :: String
                   , fragments :: [ScoreFragment]
                   }

-- For reference, start here: http://lilypond.org/text-input.html
-- and: http://lilypond.org/doc/v2.18/Documentation/learning/simple-notation#all-together
class LilypondRenderable a where
  toLilypond :: a -> ScoreFragment

instance LilypondRenderable Chord where
  toLilypond c = "<>"

-- Allows us to render a subset of possible music values:
-- e.g.
-- λ> toLilypond $ removeZeros $ Scales.toMusic en cMaj
-- "c'8 d'8 e'8 f'8 g'8 a'8 b'8 c''8"
instance LilypondRenderable (Music Pitch) where
  toLilypond (Prim (Note d p)) = concat [toLilypond p, toLilypond d]
  toLilypond (Prim (Rest 0)) = "s"
  toLilypond (Prim (Rest d)) = "r" ++ toLilypond d
  toLilypond (m1 :+: m2) = lconcat [toLilypond m1, toLilypond m2]

instance LilypondRenderable Pitch where
  toLilypond (pc, o) = concat [toLilypond pc, toLilypond o]

-- See: http://lilypond.org/doc/v2.18/Documentation/notation/writing-rhythms
instance LilypondRenderable Dur where
  -- TODO: fails with values longer than 1/2 (see lilypond docs above)
  toLilypond d = show $ denominator d

-- See: http://lilypond.org/doc/v2.18/Documentation/notation/writing-pitches
instance LilypondRenderable PitchClass where
  toLilypond pc = map toLower $ show pc

-- See: http://lilypond.org/doc/v2.18/Documentation/notation/writing-pitches
instance LilypondRenderable Octave where
  toLilypond o
    | o == 3 = ""
    | o < 3  = concat $ take (3-o) $ repeat ","
    | o > 3  = concat $ take (o-3) $ repeat "'"


lconcat = intercalate " "

toScore :: Music Pitch -> Score
toScore m = Score{clef="treble", fragments=[f]}
  where
    f = toLilypond $ removeZeros $ m

-- TODO: there's many more bells and whistles, like scales and tempi:
-- http://lilypond.org/doc/v2.18/Documentation/learning/simple-notation#all-together
renderScore :: Score -> String
renderScore s = nlconcat ["{", clefRender, fragmentsRender, "}"]
  where
    clefRender    = lconcat ["\\clef", (clef s)]
    fragmentsRender = nlconcat (fragments s)
    nlconcat        = intercalate "\n"

writeScore :: Score -> String -> IO ()
writeScore s n = do
  writeFile (n ++ ".ly") (renderScore s)
  callCommand $ concat ["~/bin/lilypond ", n, ".ly"]

{-

sorta working:

λ> cMaj = minorScale (A,4)
λ> s = renderScore $ toLilypond $ removeZeros $ Scales.toMusic en cMaj
λ> renderScoreFile s "a-minor"
GNU LilyPond 2.18.2
Processing `a-minor.ly'
Parsing...
a-minor.ly:1: warning: no \version statement found, please add

\version "2.18.2"

for future compatibility
Interpreting music...
Preprocessing graphical objects...
Finding the ideal number of pages...
Fitting music on 1 page...
Drawing systems...
Layout output to `a-minor.ps'...
Converting to `./a-minor.pdf'...
Success: compilation successfully completed
λ> s
"{a'8 b'8 c''8 d''8 e''8 f''8 g''8 a''8}"
λ> 

-}
