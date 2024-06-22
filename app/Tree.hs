import CodeWorld

flower :: Double -> Picture
flower t = colored yellow $ solidCircle (min (t/20) 0.3)

tree :: Integer -> Double -> Picture
tree 0 t = flower t
tree n t = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) t) & rotated (-pi/10) (tree (n-1) t))

main :: IO ()
main = animationOf $ tree 5
