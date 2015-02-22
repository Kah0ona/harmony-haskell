import Data.List (intersperse)

-- [A, A#, B, C, C#, D, D#, E, F, F#, G, G#]
-- [C, C#, D, D#, E, F, F#, G, G#, A, A#,B, C ]
-- [E, F , F#,G , G#,A, A#, B, C,  C#,D ,D#,E ]
-- [1      2      3  4      5      6     7  8 ]
-- [_             _         _                 ]
-- [_      _      _  _      _      _     _  _ ]
-- [0, 1 , 2 ,3 , 4 ,5, 6 , 7, 8,  9 ,10,11,12]

data Note = A | A' | B | C | C' | D | D' | E | F | F' | G | G' deriving (Show, Ord, Eq, Enum)
newtype Scale a = Scale [a] deriving (Show)

instance Functor Scale where
    fmap f (Scale []) = Scale []
    fmap f (Scale (x:xs)) = (Scale ((f x):(fmap f xs)))

join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

-- Gets next chromatic note, makes it circular
next :: Note -> Note
next G' = A
next x  = succ x

-- Gets previous chromatic note, makes it circular
prev :: Note -> Note
prev A = G'
prev x = pred x

-- Given a baseNote, return the Note at the n-th half-interval
noteAtInterval :: Note -> Int -> Note
noteAtInterval baseNote n = iterate next baseNote !! n

-- Creates a Scale based on a list of indices
createScale :: Note -> [Int] -> Scale Note
createScale root list  = (Scale (map (noteAtInterval root) list))

-- The majorScale can be generated as follows:
-- Take the root note, and then take the notes at the
-- 2nd, 4th, 5th, 7th, 9th, 11th, and 12th half-interval.
-- Note: this works by counting from 0 as root
majorScale :: Note -> Scale Note
majorScale root = createScale root [0,2,4,5,7,9,11,12]

minorScale :: Note -> Scale Note
minorScale root = createScale root [0,2,3,5,7,8,10,12]

majorTriad :: Note -> Scale Note
majorTriad root = createScale root [0,4,7] 

minorTriad :: Note -> Scale Note
minorTriad root = createScale root [0,3,7]

addedFourth :: Note -> Scale Note
addedFourth root = createScale root [0,4,5,7]

sixth :: Note -> Scale Note
sixth root = createScale root [0,4,7,9]

sixNine :: Note -> Scale Note
sixNine root = createScale root [0,4,7,9,2]

majorSeventh :: Note -> Scale Note
majorSeventh root = createScale root [0,4,7,10]

cmaj7 :: Scale Note
cmaj7 = majorSeventh C 

-- Get the Note at the given index in a scale
(!!!) :: (Scale Note) -> Int -> Note
(Scale xs)     !!! n | n < 0 = undefined
(Scale [])     !!! _         = undefined
(Scale (x:_))  !!! 0         = x
(Scale (_:xs)) !!! n         = xs !! (n-1)

-- Get the Note at the given index in a scale
noteAt :: Scale Note -> Int -> Note
noteAt scale index = scale !!! index

-- Returns number of notes in a scale
sizeOf :: Scale Note -> Int
sizeOf (Scale []) = 0
sizeOf (Scale (x:xs)) = 1 + (sizeOf (Scale (xs)))

-- Function that returns the next note in the scale
nextInScale :: Scale Note -> Int -> Note
nextInScale scale startIndex = noteAt scale (startIndex+1)

-- Function thar returns the prev note in scale
prevInScale :: Scale Note -> Int -> Note
prevInScale scale startIndex = noteAt scale (startIndex-1)

scaleContains :: Scale Note -> Note -> Bool
scaleContains (Scale []) _ = False
scaleContains (Scale (x:xs)) note = (x == note) || (scaleContains (Scale xs) note)

-- Transpose a scale with n half-steps
transpose :: Scale Note -> Int -> Scale Note
transpose (Scale []) _ = (Scale [])
transpose s 0 = s
transpose s n = fmap (\note -> noteAtInterval note n) s

guitarString :: Note -> Scale Note
guitarString root = createScale root $ [0..12] ++ [0..12]

guitarStandardTuning :: [Scale Note]
guitarStandardTuning = map guitarString [E,A,D,G,B,E]

-- return list of indices of a scale that have a note contained in the supplied Scale Note
-- counting starts at 0
getPositionOfNotes :: (Scale Note) -> (Scale Note) -> [Int]
getPositionOfNotes (Scale []) _ = []
getPositionOfNotes scale (Scale []) = take (sizeOf scale) $ [0..]
getPositionOfNotes scale chord = [ x | x <- [0..(sizeOf scale)], 
											(x < (sizeOf scale)) && -- x < number of notes in scale, and...
											 scaleContains chord (noteAt scale x) ] -- the chord 

getPositionOfNotes1 scale chord = map (1+) $ getPositionOfNotes scale chord 

listofzeroes :: Int -> [Int]
listofzeroes 0 = []
listofzeroes length = [0] ++ listofzeroes (length-1)

-- gets a list of frets that should be played on a n-string guitar tuned in the specified tuning
-- The specified tuning is just a list of scales, use chromatic scales to represent a normal guitar
-- Hint: use built-in `guitarStandardTuning` to get [E,A,D,G,B,E]
-- Or use map guitarString [D,A,D,G,A,D] to get another tuning
frets :: (Scale Note) -> [Scale Note] -> [Int]
frets (Scale []) strings  = listofzeroes (length strings) 
frets _ [] = []
frets chord strings = map (\string -> head $ getPositionOfNotes string chord) strings

fretfoldfunction :: Int -> [Char] -> [Char]
fretfoldfunction 0 result = "| " ++ result  
fretfoldfunction n result = "o " ++ result  

fretrow fretIndices = foldr fretfoldfunction "" fretIndices

getDiagramGrid :: [[Int]] -> [Int] -> Int -> [[Int]]
getDiagramGrid result list 0 = result
getDiagramGrid result list n = 
						let 
							toAdd = [ if x == n then 1 else 0 | x <- list ]
						in 
							getDiagramGrid (toAdd : result) list (n-1)

fretdiagram :: (Scale Note) -> [Scale Note] -> [Char]
fretdiagram chord tuning =
				let 
					indices = frets chord tuning
					divider = "===========\n"
					header = "\n" ++ (join " " (map show indices)) ++ "\n"
					grid = getDiagramGrid [] indices (length indices)
					fretrows = map fretrow grid
				in
					header ++ divider ++ (join "\n" fretrows) ++ "\n\n"

--fretdiagram indices = map
-- An Am chord in ascii
-- E A D G B e
--  
-- 0 0 2 2 1 0     Fretnum
-- ===========
-- | | | | o |     1
-- | | o o | |     2
-- | | | | | |     3
-- | | | | | |     4
-- | | | | | |     5
-- | | | | | |
-- | | | | | |
