import CodeWorld
import Data.Fixed (E0)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Dir = U | R | D | L
data Coord = C Integer Integer Dir

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

player :: Coord -> Picture
player (C x y d) =
  translated (fromIntegral x) (fromIntegral y) (
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
mazeWithPlayer c = player c & pictureOfMaze

initialPos :: Coord
initialPos = C 0 (-1) U

adjacentCoord :: Dir -> Coord -> Coord
adjacentCoord R (C x y _) = C (x+1) y R
adjacentCoord U (C x y _) = C  x   (y+1) U
adjacentCoord L (C x y _) = C (x-1) y L
adjacentCoord D (C x y _) = C  x   (y-1) D

canMove :: Coord -> Bool
canMove (C x y _)
  | tg == Storage || tg == Ground = True
  | otherwise = False
  where tg = maze x y

withMove :: Dir -> Coord -> Coord
withMove d c
  | canMove n = n
  | otherwise = C x y d
  where
    n = adjacentCoord d c
    (C x y _) = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress "Up") c = withMove U c
handleEvent (KeyPress "Right") c = withMove R c
handleEvent (KeyPress "Down") c = withMove D c
handleEvent (KeyPress "Left") c = withMove L c
handleEvent _ c = c

main :: IO ()
main = activityOf initialPos handleEvent mazeWithPlayer
