import CodeWorld

topCircle :: Color -> Picture
topCircle c = translated 0 2.5 $ colored c $ solidCircle 1

midCircle :: Color -> Picture
midCircle c = colored c $ solidCircle 1

botCircle :: Color -> Picture
botCircle c = translated 0 (-2.5) $ colored c $ solidCircle 1

frame :: Picture
frame = rectangle 2.5 7.5

data State = Go | PreStop | Stop | PreGo

lights :: State -> Picture
lights Go = topCircle black & midCircle black & botCircle green
lights PreStop = topCircle black & midCircle yellow & botCircle black
lights Stop = topCircle red & midCircle black & botCircle black
lights PreGo = topCircle red & midCircle yellow & botCircle black

controller :: State -> Picture
controller state = lights state & frame

animatedController :: Double -> Picture
animatedController t
  | n == 0 || n == 1 = controller Go
  | n == 2           = controller PreStop
  | n == 3 || n == 4 = controller Stop
  | n == 5           = controller PreGo
  | otherwise        = blank
  where n = (round t `mod` 6) :: Integer

main :: IO ()
main = animationOf animatedController
