import CodeWorld

topCircle :: Color -> Picture
topCircle c = translated 0 1.5 $ colored c $ solidCircle 1

botCircle :: Color -> Picture
botCircle c = translated 0 (-1.5) $ colored c $ solidCircle 1

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = topCircle black & botCircle green & frame
trafficLight False = topCircle red & botCircle black & frame

trafficController :: Double -> Picture
trafficController t
  | even $ round (t / 3) = trafficLight True
  | otherwise          = trafficLight False

main :: IO ()
main = animationOf trafficController
