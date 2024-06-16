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
animatedController t = case round t `mod` 6 of
  0 -> controller Go
  1 -> controller Go
  2 -> controller PreStop
  3 -> controller Stop
  4 -> controller Stop
  5 -> controller PreGo
  _ -> blank

main :: IO ()
main = animationOf animatedController
