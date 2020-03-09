-- | Types and functions for shapes. The list of all tetris pieces.
module Shapes where
import Data.List(transpose)
import Data.Maybe(isNothing, isJust)
import Test.QuickCheck

-- * Shapes

type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.

data Shape = S [Row] deriving (Eq)
type Row = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs

-- * Showing shapes

showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
  where
    showRow :: Row -> String
    showRow r = [showSquare s | s <- r]
    
    showSquare Nothing = '.'
    showSquare (Just Black) = '#' -- can change to '█' on linux/mac
    showSquare (Just Grey)  = 'g' -- can change to '▓'
    showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss)++r


-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of connected 4 blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [S (makeSquares s) | s <- shapes] 
   where
      makeSquares = map (map colour)
      colour c    = lookup c [('I',Red),('J',Grey),('T',Blue),('O',Yellow),
                              ('Z',Cyan),('L',Green),('S',Purple)]
      shapes = 
              [[
              "S S S",
              " SSS ",
              " SSS ",
              "  S  ",
              "S   S"],
              [
              " Z Z ",
              "  Z  ",
              " ZZZ ",
              "  Z  ",
              "ZZZZZ"
              ]]

-- * Some simple functions
--data Shape = S [Row] deriving (Eq)
--type Row = [Square]
-- ** A01
emptyShape :: (Int,Int) -> Shape
emptyShape (w,h) = S (replicate h (emptyRow w))

emptyRow :: Int -> [Square]
emptyRow w = replicate w Nothing

-- ** A02

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int,Int)
shapeSize s = (length (head r), length r)
  where r = rows s

-- ** A03

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount s = blockCount' (concat (rows s))
  where blockCount' [] = 0
        blockCount' (Nothing:xs) = blockCount' xs
        blockCount' (Just _:xs) = 1 + blockCount' xs

-- * The Shape invariant

-- ** A04
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape s = w >= 1 && h >= 1 && length (concat(rows s)) == w * h
  where w = fst (shapeSize s)
        h = snd (shapeSize s)

-- * Test data generators

-- ** A05
-- | A random generator for colours
rColour :: Gen Colour
rColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = rColour

-- ** A06
-- | A random generator for shapes
rShape :: Gen Shape
rShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = rShape

-- * Transforming shapes

-- ** A07
-- | Rotate a shape 90 degrees (clockwise)
rotateShape :: Shape -> Shape
rotateShape s = S (transpose (rows s))

-- ** A08
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int,Int) -> Shape -> Shape
shiftShape (a, l) s = shiftShapeV (shiftShapeH s)
  where shiftShapeV s = S (rows (emptyShape (fst (shapeSize s),l)) 
                        ++ rows s)
        shiftShapeH s = S (transpose (rows (emptyShape (snd (shapeSize s),a))
                        ++ transpose (rows s)))

-- ** A09
-- | padShape adds empty squares below and to the right of the shape
padShape :: (Int,Int) -> Shape -> Shape
padShape (b,r) s = padShapeV (padShapeH s)
  where padShapeV s = S (rows s ++ rows (emptyShape (fst (shapeSize s),b)))
        padShapeH s = S (transpose (transpose (rows s)
                      ++ rows (emptyShape (snd (shapeSize s),r))))

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int,Int) -> Shape -> Shape
padShapeTo (w,h) s = padShape (h-h',w-w') s
  where (w',h') = shapeSize s

-- * Comparing and combining shapes

-- ** A11

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or [rowsOverlap r1 r2 | r1 <- rows s1, r2 <- rows s2]
  where 
    rowsOverlap r1 r2 = or (zipWith (\x1 x2 -> isJust x1 && isJust x2) r1 r2)

-- ** A12
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square->Square->Square) -> Shape -> Shape -> Shape
zipShapeWith f (S []) _       = S []
zipShapeWith f  _  (S [])     = S []
zipShapeWith f  (S r1) (S r2) = 
  S (zipWith f h1 h2 : rows (zipShapeWith f (S t1) (S t2)))
    where h1 = head r1
          t1 = tail r1
          h2 = head r2
          t2 = tail r2

-- ** A13
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = 
  if not (s1 `overlaps` s2)
    then zipShapeWith (\x1 x2 -> if isJust x1 then x1 else x2) s1' s2'
    else error "Overlapping shapes cannot be combined!"
    where size1   = shapeSize s1
          size2   = shapeSize s2
          newSize = (max (fst size1) (fst size2), max (snd size1) (snd size2))
          s1'     = padShapeTo newSize s1
          s2'     = padShapeTo newSize s2

