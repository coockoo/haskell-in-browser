import CodeWorld

data List a = Empty | Entry a (List a) deriving (Show)
data Coord = C Integer Integer deriving (Show)
data Dir = U | R | D | L
data Pos = Pos Coord Dir
data State = S Pos (List Coord) 
data SSState world = StartScreen | Running world
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)
type Maze = Integer -> Integer -> Tile

-- mindblow: data type constructor also support partial application
-- insight: data type constructor is a function too

maze :: Maze
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

mazeWithNoBoxes :: Maze
mazeWithNoBoxes x y = case maze x y of
  Box -> Ground
  other -> other

isBox :: List Coord -> Integer -> Integer -> Bool
isBox Empty _ _ = False
isBox (Entry (C cx cy) rest) x y
  | cx == x && cy == y = True 
  | otherwise = isBox rest x y

mazeWithBoxes :: List Coord -> Maze
mazeWithBoxes boxes x y
  | isBox boxes x y = Box
  | otherwise = mazeWithNoBoxes x y

drawTile :: Tile -> Picture
drawTile t = case t of
  Wall -> colored black $ solidRectangle 1 1
  Ground -> colored grey $ solidRectangle 1 1
  Storage -> colored yellow (solidCircle 0.1) & drawTile Ground
  Box ->
    colored black (rectangle s s) &
    colored brown (solidRectangle s s) &
    drawTile Ground
    where s = 0.8
  Blank -> blank

append :: List a -> a -> List a
append list item = Entry item list

merge :: List a -> List a -> List a
merge lf (Entry a rest) = merge (Entry a lf) rest
merge lf Empty = lf

getTiles :: Integer -> (Integer -> Integer -> Bool) -> List Coord
-- TODO: I really need to learn that syntax with dot, as it looks rad
-- function composition
getTiles n check = mergeTimes n (mergeTimes n . mt)
  where
    mt x y = if check x y then Entry (C x y) Empty else Empty

mergeTimes :: Integer -> (Integer -> List a) -> List a
mergeTimes m something = doTimes m something merge

resetable :: Activity world -> Activity world
resetable (Activity initialState onEvent draw) = Activity initialState onEvent' draw
  where
    onEvent' (KeyPress "Esc") _ = initialState
    onEvent' e s = onEvent e s

withStartScreen :: Activity world -> Activity (SSState world)
withStartScreen (Activity initialState onEvent draw) = Activity initialState' onEvent' draw'
  where
    initialState' = StartScreen

    onEvent' (KeyPress " ") StartScreen = Running initialState
    onEvent' _ StartScreen = StartScreen
    onEvent' e (Running s) = Running (onEvent e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

    startScreen = scaled 3 3 (lettering "Sokoban!")

runActivity :: Activity world -> IO ()
runActivity (Activity initialState onEvent draw) = activityOf initialState onEvent draw

initialBoxes :: List Coord
initialBoxes = getTiles 4 (\x y -> maze x y == Box)

initialState :: State
initialState = S (Pos (C 0 (-1)) U) initialBoxes

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

canMove :: List Coord -> Coord -> Bool -> Bool
canMove boxes (C x y) isPlayer = case mazeWithBoxes boxes x y of
  Storage -> True
  Ground -> True
  Box -> isPlayer
  _ -> False

moveBox :: Coord -> Coord -> Coord -> Coord
moveBox b (C xf yf) to = if xb == xf && yb == yf then to else b
  where
    (C xb yb) = b

data Move = Move Coord (List Coord)
withMove :: Dir -> Coord -> List Coord -> Move
withMove dir c boxes
  | playerMoves && boxMoves = Move n (mapList (\b -> moveBox b n nb) boxes)
  | otherwise = Move c boxes
  where
    n = adjacentCoord dir c
    (C nx ny) = n
    nb = adjacentCoord dir n
    playerMoves = canMove boxes n True
    boxMoves = not (isBox boxes nx ny) || canMove boxes nb False

handleEvent :: Event -> State -> State
handleEvent (KeyPress k) (S (Pos c dir) boxes) = S (Pos nextPos nextDir) nextBoxes
  where
    nextDir = case k of
      "Up" -> U
      "Down" -> D
      "Left" -> L
      "Right" -> R
      _ -> dir
    -- todo: this thing does move even if no arrow key is pressed
    (Move nextPos nextBoxes) = withMove nextDir c boxes
  
handleEvent _ s = s

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromIntegral x) (fromIntegral y)

draw :: State -> Picture
draw (S pos boxes) = drawPlayer pos & drawMaze boxes

-- todo:
-- 1. define & operator on list
-- 2. make sure this function accepts "a" that has this operator
doTimes :: Integer -> (Integer -> a) -> (a -> a -> a) -> a
doTimes m something op = go (-m)
  where
    go i
      | i >= m = something i
      | otherwise = something i `op` go (i + 1)

drawTimes :: Integer -> (Integer -> Picture) -> Picture
drawTimes m something = doTimes m something (&)


adjacentCoord :: Dir -> Coord -> Coord
adjacentCoord dir (C x y) = case dir of
  R -> C (x+1) y
  L -> C (x-1) y
  U -> C  x   (y+1)
  D -> C  x   (y-1)

drawCell :: Maze -> Integer -> Integer -> Picture
drawCell maze y x = atCoord (C x y) (drawTile (maze x y))

drawPlayer :: Pos -> Picture
drawPlayer (Pos c d) =
  atCoord c (
    colored white (polygon line) &
    colored red (solidPolygon line)
  )
  where
    s = 0.8 / 2
    line = case d of
      U -> [(-s, -s), (s, -s), (0, s)]
      R -> [(-s, s), (-s, -s), (s, 0)]
      D -> [(-s, s), (s, s), (0, -s)]
      L -> [(s, -s), (s, s), (-s, 0)]

drawMaze :: List Coord -> Picture
drawMaze boxes = drawTimes 4 (drawTimes 4 . drawCell (mazeWithBoxes boxes))

sokoban :: Activity State
sokoban = Activity initialState handleEvent draw

main :: IO ()
main = runActivity (resetable (withStartScreen sokoban))
