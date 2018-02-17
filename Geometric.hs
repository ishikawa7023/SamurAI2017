module Geometric where

newtype Point = P (Double,Double) deriving (Eq, Ord, Show)
newtype Line  = L (Point,Point) deriving (Eq, Ord)
data Direction = Left | Right | On deriving Show

hoge = 3

whereIsR :: Point -> Point -> Point  -> Direction -- p.18 1.4
whereIsR (P (px,py)) (P (qx,qy)) (P (rx,ry))
  | _D == 0 = On
  | _D > 0  = Geometric.Left
  | otherwise = Geometric.Right
  where _D = (qx*ry + px*qy + py*rx) - (py*qx  + px*ry + qy*rx)

{-
       | 1 px py |
  D =  | 1 qx qy |
       | 1 rx ry |
-}

doesCross :: Line -> Line -> Bool
doesCross l1@(L (P (x1,y1), P (x2,y2))) l2@(L (P (x3,y3), P (x4,y4)))
  | l1 == l2 = True
  | 0 <= s && s <= 1 && 0 <= t && t <= 1 = True
  | otherwise = False
  where
    delta = (x3-x4)*(y2-y1) - (x2-x1)*(y3-y4)
    s = ((x3-x1)*(y2-y1) - (x2-x1)*(y3-y4)) / delta
    t = ((x3-x4)*(y3-y1) - (x3-x1)*(y3-y4)) / delta

pp = P (0,0.5)
p0 = P (-1,1)
p1 = P (0,0)
p2 = P (1,1)
p3 = P (0,1)
-- p4 = P (1,0)

-- l1 = L (p1,p2)
-- l2 = L (p3,p4)

-- p5 = P (5,1)
-- p6 = P (6,4)
-- p7 = P (-8,7)
-- p8 = P (-3,1)

-- l3 = L (p5,p6)
-- l4 = L (p7,p8)
