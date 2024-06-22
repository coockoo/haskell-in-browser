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

drawCell :: Integer -> Integer -> Integer -> Picture
drawCell val x y = translated (intToDouble x) (intToDouble y) (drawTile val)

drawRow :: Integer -> Integer -> Picture
drawRow y x
  | val == 0 = blank
  | otherwise = drawCell val x y & drawRow y (x + 1)
  where val = maze x y

drawMaze :: Integer -> Picture
drawMaze y
  | abs y > 4 = blank
  | otherwise = drawRow y (-4) & drawMaze (y + 1)

pictureOfMaze :: Picture
pictureOfMaze = drawMaze (-4)

main :: IO ()
main = drawingOf pictureOfMaze
