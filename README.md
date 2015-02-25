# harmony-haskell
A library for musical scales and notes. Useful for creating  scales and chords and notes and manipulating them like transposing them. Can generate chord diagrams for the guitar, and has various tools for working with and generating scales.


Can print stuff like:
====================

	ghci> putStr $ fretdiagram (majorTriad A) standardTuning 5

	5 7 7 6 5 5
	===========
	| | | | | |
	| | | | | |
	| | | | | |
	| | | | | |
	o | | | o o
	| | | o | |
	| o o | | |
	| | | | | |
	| | | | | |
	| | | | | |
	| | | | | |


Also see this [blog post](http://kah0ona.github.io/software/haskell/2015/02/23/A-Journey-Into-Haskell-Chord-And-Scale-Library.html)
