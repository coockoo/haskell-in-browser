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
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

pictureOfMaze :: Picture
pictureOfMaze = drawTile $ maze 0 0

main :: IO ()
main = drawingOf pictureOfMaze
