import CodeWorld

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1 -- wall
  | x ==  2 && y <= 0        = 1 -- wall
  | x ==  3 && y <= 0        = 3 -- storage
  | x >= -2 && y == 0        = 4 -- box
  | otherwise                = 2 -- ground

wall :: Picture
wall = colored black $ solidRectangle 1 1

ground :: Picture
ground = colored grey $ solidRectangle 1 1

storage :: Picture
storage = colored yellow (solidCircle 0.1) & ground

box :: Picture
box = colored black (rectangle 0.8 0.8) & colored brown (solidRectangle 0.8 0.8) & ground

drawTile :: Integer -> Picture
drawTile d
  | d == 1 = wall
  | d == 2 = ground
  | d == 3 = storage
  | d == 4 = box
  | otherwise = blank

intToDouble :: Integer -> Double
intToDouble n = fromIntegral n / 1.0

drawCell :: Integer -> Integer -> Picture
drawCell y x = translated (intToDouble x) (intToDouble y) (drawTile (maze x y))

drawRow :: Integer -> Picture
drawRow y = drawTimes 4 (drawCell y)

doDrawTimes :: Integer -> Integer -> (Integer -> Picture) -> Picture
doDrawTimes i m something
  | abs i > m = blank
  | otherwise = something i & doDrawTimes (i + 1) m something

drawTimes :: Integer -> (Integer -> Picture) -> Picture
drawTimes m = doDrawTimes (-m) m

pictureOfMaze :: Picture
pictureOfMaze = drawTimes 4 drawRow

main :: IO ()
main = drawingOf pictureOfMaze
