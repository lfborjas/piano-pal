{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

type LilypondCommand =  (String, [String])

data Score = Score { clef :: String
                   , fragments :: [ScoreFragment]
                   , meta :: [LilypondCommand]
                   } deriving (Show)

-- For reference, start here: http://lilypond.org/text-input.html
-- and: http://lilypond.org/doc/v2.18/Documentation/learning/simple-notation#all-together
class LilypondRenderable a where
  toLilypond :: a -> ScoreFragment

class Notate a where
  toScore :: a -> Score

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

-- Allows us to render a subset of possible music values:
-- e.g.
-- λ> toLilypond $ removeZeros $ Scales.toMusic en cMaj
-- "c'8 d'8 e'8 f'8 g'8 a'8 b'8 c''8"
instance LilypondRenderable (Music Pitch) where
  toLilypond (Prim (Note d p)) = concat [toLilypond p, toLilypond d]
  toLilypond (Prim (Rest 0)) = "s"
  toLilypond (Prim (Rest d)) = "r" ++ toLilypond d
  toLilypond (m1 :+: m2) = lconcat [toLilypond m1, toLilypond m2]
  toLilypond m@(_ :=: _) = showChord m

instance Notate (Music Pitch) where
  toScore m = Score{clef="treble", fragments=[f], meta=[]}
    where
      f = toLilypond $ removeZeros $ m  

instance Notate Scale where
  toScore s = Score{clef="treble", fragments=[m], meta=meta}
    where
      m  = toLilypond $ removeZeros $ Scales.toMusicOctaves 2 qn s
      meta = [("key", [pitch, "\\"++(mode s)])]
      pitch = map toLower $ show (root s)

-- inspired by: https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.Show.html#showList__
-- this method was giving bonkers error messages that I never really figured out
-- (fixed by enabling FlexibleContexts, though that seems rather arcane to me)
-- see bottom of file for more.
-- also: http://lilypond.org/doc/v2.19/Documentation/notation/displaying-chords
showChord :: Music Pitch -> ScoreFragment
showChord (m :=: ms) =
  lconcat ["<", toLilypond (getPitch m), (toL ms)]
  where
    toL x@(Prim _)   = lconcat [toLilypond (getPitch x),  ">", toLilypond (getDur x)]
    toL (y :=: ys)   = lconcat [toLilypond (getPitch y), toL ys]
    getPitch (Prim (Note _ p)) = p
    getDur   (Prim (Note d _)) = d

lconcat = intercalate " "

-- TODO: there's many more bells and whistles, like scales and tempi:
-- http://lilypond.org/doc/v2.18/Documentation/learning/simple-notation#all-together
renderScore :: Score -> String
renderScore s = nlconcat [languageRender, "{", clefRender, metaRender, fragmentsRender, "}"]
  where
    languageRender  = "\\language \"english\""
    clefRender      = lconcat ["\\clef", (clef s)]
    nlconcat        = intercalate "\n"
    metaRender      = nlconcat $ map (\(c, a) -> "\\" ++ c ++ " " ++ (intercalate " " a)) (meta s)
    fragmentsRender = nlconcat (fragments s)

writeScore :: String -> Score -> IO ()
writeScore n s = do
  writeFile (n ++ ".ly") (renderScore s)
  callCommand $ concat ["~/bin/lilypond ", n, ".ly"]

{-

sorta working:

-- SCALES:

λ> renderScore $ toScore $ minorScale C
"\\language \"english\"\n{\n\\clef treble\n\\key c \\minor\nc'4 d'4 ef'4 f'4 g'4 af'4 bf'4\nc''4 d''4 ds''4 f''4 g''4 gs''4 as''4\n}"
λ> writeScore "cmin3" $ toScore $ minorScale C
GNU LilyPond 2.18.2
Processing `cmin3.ly'
Parsing...
cmin3.ly:1: warning: no \version statement found, please add

\version "2.18.2"

for future compatibility
Interpreting music...
Preprocessing graphical objects...
Finding the ideal number of pages...
Fitting music on 1 page...
Drawing systems...
Layout output to `cmin3.ps'...
Converting to `./cmin3.pdf'...
Success: compilation successfully completed

-- CHORDS:

λ> aMin = minorScale (A,4)
λ> aMinChord = Chord minorTriad Tonic aMin
λ> s = toScore $ Chords.toMusic qn aMinChord
λ> print $ renderScore s
"\\language \"english\"\n{\n\\clef treble\n< a' c'' e'' > 4\n}"
λ> writeScore s "achord2"
GNU LilyPond 2.18.2
Processing `achord2.ly'
Parsing...
achord2.ly:1: warning: no \version statement found, please add

\version "2.18.2"

for future compatibility
Interpreting music...
Preprocessing graphical objects...
Finding the ideal number of pages...
Fitting music on 1 page...
Drawing systems...
Layout output to `achord2.ps'...
Converting to `./achord2.pdf'...
Success: compilation successfully completed


A Story of Flexible Contexts:

The simple asChord function above failed very weirdly:

/Users/luis/code/piano-pal/src/Render.hs:42:5: error:
    • Non type-variable argument
        in the constraint: LilypondRenderable (Music a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        toL :: forall a. LilypondRenderable (Music a) => Music a -> [Char]
      In an equation for ‘showChord’:
          showChord (m :=: ms)
            = lconcat ["<", toLilypond m, (toL ms)]
            where
                toL x@(Prim _) = lconcat [(toLilypond x), ....]
                toL (m' :=: ms') = lconcat [(toLilypond m'), ....]
   |
42 |     toL x@(Prim _)   = lconcat [(toLilypond x),  ">"]
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...

Apparently it was trying to create a constraint with LilypondRenderable (Music a),
which is illegal. But... I wasn't sure how or why, or why simply adding FlexibleContexts,
which I actually did in hopes of getting a more reasonable error message, fixed it:

https://stackoverflow.com/questions/31251163/what-is-the-flexiblecontexts-extension-good-for-could-you-please-explain-it-usi


-}
