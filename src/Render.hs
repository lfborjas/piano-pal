{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Render where

import Euterpea
import Scales
import Chords
import Data.Ratio
import Data.Char (toLower)

-- For reference, start here: http://lilypond.org/text-input.html

class LilypondRenderable a where
  toLilypond :: a -> String

instance LilypondRenderable Chord where
  toLilypond c = "<>"

instance LilypondRenderable (Music Pitch) where
  toLilypond (Prim (Note d p)) = concat [toLilypond p, toLilypond d]
  toLilypond (Prim (Rest 0)) = "s"
  toLilypond (Prim (Rest d)) = "r" ++ toLilypond d
  toLilypond (m1 :+: m2) = concat [toLilypond m1, toLilypond m2]

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

-- renderScale :: Scale -> String
-- renderScale s = mconcat $ mMap toLilypond $ Scales.toMusic en s
