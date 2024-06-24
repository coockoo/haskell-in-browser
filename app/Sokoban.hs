import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank
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

main :: IO ()
main = drawingOf (player 1 1 & pictureOfMaze)
