import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Dir = U | R | D | L
data Coord = C Integer Integer

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

wall :: Picture
wall = colored black $ solidRectangle 1 1

ground :: Picture
ground = colored grey $ solidRectangle 1 1

storage :: Picture
storage = colored yellow (solidCircle 0.1) & ground

box :: Picture
box =
  colored black (rectangle s s) &
  colored brown (solidRectangle s s) &
  ground
  where s = 0.8

player :: Integer -> Integer -> Picture
player x y =
  translated (fromIntegral x) (fromIntegral y) (
    colored white (rectangle s s) &
    colored red (solidRectangle s s)
  )
  where s = 0.8

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

drawCell :: Integer -> Integer -> Picture
drawCell y x = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

drawTimes :: Integer -> (Integer -> Picture) -> Picture
drawTimes m something = go (-m)
  where
    go i
      | i >= m = something i
      | otherwise = something i & go (i + 1)

pictureOfMaze :: Picture
-- pictureOfMaze = drawTimes 4 (\y -> drawTimes 4 (\x -> drawCell y x))
-- this . (dot) operator was suggested by linter and WOW
pictureOfMaze = drawTimes m (drawTimes m . drawCell)
  where m = 4

mazeWithPlayer :: Coord -> Picture
mazeWithPlayer (C x y) = player x y & pictureOfMaze

initialPos :: Coord
initialPos = C 0 (-1)

adjacentCoord :: Dir -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

canMove :: Coord -> Bool
canMove (C x y)
  | tg == Storage || tg == Ground = True
  | otherwise = False
  where tg = maze x y

withMove :: Dir -> Coord -> Coord
withMove d c
  | canMove n = n
  | otherwise = c
  where n = adjacentCoord d c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress "Up") c = withMove U c
handleEvent (KeyPress "Right") c = withMove R c
handleEvent (KeyPress "Down") c = withMove D c
handleEvent (KeyPress "Left") c = withMove L c
handleEvent _ c = c

main :: IO ()
main = activityOf initialPos handleEvent mazeWithPlayer
