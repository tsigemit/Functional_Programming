module Sudoku where

import Test.QuickCheck
import Data.Maybe
import Data.Char
import Data.List

------------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle (grid is 9x9 & digits are between 1 and 9)
isSudoku :: Sudoku -> Bool
isSudoku sud = length (rows sud) == 9 && 
               -- Nothing becomes 0, Just 1-9 become 1-9
               and [and [fromMaybe 0 x `elem` [0..9] | x <- row] 
               && length row == 9 | row <- rows sud] 

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled sud = and [and [fromMaybe 0 x `elem` [1..9] | x <- row] | row <- rows sud]

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStrLn (sudokuRowsToString (rows sud))

-- helper function for printSudoku:
-- given the sudoku rows, it generates the string to output
sudokuRowsToString :: [[Maybe Int]] -> String
sudokuRowsToString [] = ""
sudokuRowsToString [y] = map maybeIntToChar y
sudokuRowsToString (y:ys) = map maybeIntToChar y 
                          ++ "\n" 
                          ++ sudokuRowsToString ys

-- given a Maybe Int, it returns the corresponding char
maybeIntToChar :: Maybe Int -> Char
maybeIntToChar Nothing = '.'
maybeIntToChar (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do 
  file_content <- readFile path
  let file_lines = lines file_content
  checkFormat $ Sudoku $ map (map charToMaybeInt) file_lines

-- if a character is a digit in [1,9], it returns the corresponding Maybe Int
charToMaybeInt :: Char -> Maybe Int
charToMaybeInt '.' = Nothing
charToMaybeInt char =
  if isDigit char 
    then Just (digitToInt char)
    else error "The input file contains an invalid character."

-- calls isSudoku to check the format of the parsed file
-- returns an IO Sudoku iff the format is valis
checkFormat :: Sudoku -> IO Sudoku
checkFormat sud = 
  if isSudoku sud
    then return sud
    else error "Invalid format!"


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, do n <- choose(1,9); return (Just n))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku

------------------------------------------------------------------------------

type Block = [Maybe Int]


-- * D1

-- returns True if the block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock b = length b == length (nubBy maybeIntEq b)

-- equality test for Maybe Ints: 
-- needed not to considered Nothing elements as duplicates in isOkayBlock
maybeIntEq :: Maybe Int -> Maybe Int -> Bool
maybeIntEq Nothing Nothing = False
maybeIntEq x y = x == y

-- * D2

-- given a sudoku, it returns a list of all of its blocks
-- format: rows ++ columns ++ 3*3 "squares"
blocks :: Sudoku -> [Block]
blocks sud = rows sud
             ++ transpose (rows sud) 
             ++ squares (rows sud)

-- returns the list of 3*3 blocks of a sudoku
squares :: [[Maybe Int]] -> [Block]
squares [] = []
squares rows = takeSquare rows : squares (dropSquare rows)

-- constructs a square out of a part of a sudoku
takeSquare :: [[Maybe Int]] -> Block
takeSquare rows = concatMap (take 3) (take 3 rows)

-- "drops" a square of a part of a sudoku
dropSquare :: [[Maybe Int]] -> [[Maybe Int]]
dropSquare rows = filter (not . null) (map (drop 3) (take 3 rows)) 
                  ++ drop 3 rows

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sud = length (blocks sud) == 3*9 && --there are 3*9 blocks
                          -- the size of each block is 9
                          all (\block -> length block == 9) (blocks sud)

-- * D3

-- returns true if no block of a given sudoku contains duplicate digits
isOkay :: Sudoku -> Bool
isOkay sud = all isOkayBlock (blocks sud)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1
-- Given a Sudoku, returns a list of the positions of the blanks elements
blanks :: Sudoku -> [Pos]
blanks sud = [(i,j) | i <- [0..8], j<- [0..8], isNothing ((rows sud !! i) !! j)]

-- Property to check all blank cells are actually blank
prop_blanks_allBlank :: Sudoku -> Bool
prop_blanks_allBlank sud = 
  all (\element -> isNothing ((sud' !! fst element) !! snd element)) blankPos
    where blankPos   = blanks sud
          sud'       = rows sud

-- * E2
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] (_,_) = []
(!!=) xs (i,y) = if i > length xs -1 || i < 0 then xs
             else 
             let (firstPart, secondPart) = splitAt i xs
               in firstPart ++ y: drop 1 secondPart 

-- Test (!!=) property
prop_bangBangEquals_correct :: [a] -> (Int,a) -> Bool
prop_bangBangEquals_correct xs (i,y) = length xs == length replace 
                                where
                                   replace = xs !!= (abs i,y)
-- * E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud pos newCell = 
  Sudoku (rows sud !!= (fst pos, rows sud !! fst pos !!= (snd pos, newCell)))

prop_update_updated :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_updated sud pos val = (rows sud' !! fst pos) !! snd pos == val 
  where sud' = update sud pos val

-- * E4
candidates :: Sudoku -> Pos -> [Int]
candidates = candidates' [1..9]
-- helper for candidates
candidates' :: [Int] -> Sudoku -> Pos -> [Int]
candidates' [] _ _ = []
candidates' (x:xs) sud pos | isOkay(update sud pos (Just x)) = 
                              x : candidates' xs sud pos
                           | otherwise = candidates' xs sud pos


-- A position generator, creating a position between (0,0) and (8,8).
-- Used for quickcheck testing
rPos :: Gen Pos
rPos = do i <- elements [0..8]
          j <- elements [0..8]
          return (i,j)
          
prop_candidates_correct :: Sudoku -> Property
prop_candidates_correct sudoku = isOkay sudoku ==>
                        forAll rPos (`prop_candidates_correct'` sudoku)
  where
    prop_candidates_correct' :: Pos -> Sudoku -> Bool
    prop_candidates_correct' (i,j) sudoku = all isOkay [update sudoku (i,j)
                                        (Just v) | v <-
                                        candidates sudoku (i,j)]
------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve sud = if isSudoku sud && isOkay sud then
                  solve' sud (blanks sud)
              else Nothing
  
solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' sud [] = Just sud
solve' sud (blank:bs) = listToMaybe $ catMaybes $
                            map ((`solve'` bs))
                            [update sud blank (Just x) | x <- candidates sud blank]
-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve file = do sud <- readSudoku file
                       let solvedSud = solve sud
                       maybe (putStrLn "(no solution)") printSudoku solvedSud

-- * F3
-- return true if a sudoku is a solution of another
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf sol sud = isFilled sol 
                       && isOkay sol 
                       && isOf sol sud

-- returns true if all digits in the second sudoku are maintained in the first one
isOf :: Sudoku -> Sudoku -> Bool
isOf sol sud = all (isEqualCell sol sud) (filled sud)
  where filled sud = [(i,j) | i <- [0..8], j<- [0..8], isJust ((rows sud !! i) !! j)]
        
-- checks if two sudokus have the same value stored at the same position  
isEqualCell :: Sudoku -> Sudoku -> Pos -> Bool
isEqualCell sud sud' pos = 
  (rows sud !! fst pos) !! snd pos == (rows sud' !! fst pos) !! snd pos

  
-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isSudoku sud && isOkay sud && isJust sol ==> 
                      isSolutionOf (fromJust sol) sud
                        where sol = solve sud