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
drawCell x y = translated (intToDouble x) (intToDouble y) (drawTile (maze x y))

drawRow :: Integer -> Integer -> Picture
drawRow x y
  | abs x > 4 = blank
  | otherwise = drawCell x y & drawRow (x + 1) y

drawRows :: Integer -> Picture
drawRows y
  | abs y > 4 = blank
  | otherwise = drawRow (-4) y & drawRows (y + 1)

pictureOfMaze :: Picture
pictureOfMaze = drawRows (-4)

main :: IO ()
main = drawingOf pictureOfMaze
