import CodeWorld

flower :: Double -> Picture
flower t = colored yellow $ solidCircle (min (t/33) 0.3)

tree :: Integer -> Picture -> Picture
tree 0 pic = pic
tree n pic = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) pic) & rotated (-pi/10) (tree (n-1) pic))

bloom :: Double -> Picture
bloom t = tree 8 $ flower t

main :: IO ()
main = animationOf bloom
