# Piano Pal

Learning [more Haskell through music](https://github.com/lfborjas/distractions/blob/a6b6bcd09270fa73233b05f3afb1800b977da9c3/haskell_school_of_music/README.md) and more [music through Haskell](https://github.com/lfborjas/musicaloffering): The `Scales` and `Chords` modules represent my current understanding of both concepts, and they provide functions that are useful for plugging in snippets of music found in the wild and seeing how they conform to (or diverge from) classical music theory (I'm, at the moment, daunted by jazz, folk music and other practices outside of the common practice period, sorry!)

The main goal here is personal use, for now: not only am I currently mostly focused on classical music theory, I'm practicing the piano and thus a lot of helper/rendering functions are geared towards generating sheet music in a grand staff, within the piano range and with piano fingerings, to help me practice some fundamentals in the instrument, or decode some of the pieces I'm practicing (which tend to fall in the Bach-Grieg continuum, with perhaps some future incursions into twelve-tone shenanigans.)

## What does it do?

- Interactive MIDI-out experimentation from `ghci`
- Rendering of scales, chords and arpeggios using [Lilypond](http://lilypond.org/doc/v2.18/Documentation/web/macos-x). Can generate some basic piano exercises (see the `example-scores` directory for some scores I've generated).
- [`pending`] Real-time MIDI-in experimentation, with the `listen` binary.


For example, you can play around in `gchi` and, if you have [a MIDI setup compatible with Euterpea](http://www.euterpea.com/euterpea/setting-up-midi/), you'll be able to see and hear scales and chords:

```haskell

λ> cMin = minorScale C
λ> show cMin
"C D Ef F G Af Bf"

λ> play $ Scales.toMusic en cMin

λ> cMinChord = Chord minorTriad Tonic cMin
λ> play $ Chords.toMusic qn cMinChord


```

Or you can run the `listen` binary, plug in a MIDI keyboard, send the delimiter signals and then try and see what scales or chords may apply. 

In both contexts above, once you generate some `Music` values, you can render them to Lilypond.


## Installation

This project uses Haskell's [`stack`](https://docs.haskellstack.org/en/stable/GUIDE/#existing-projects), so local development should be pretty much self-contained once you clone the repository. Haven't created distributable binaries yet!



