-- This is the first sample Main Program
-- 
-- $ hlint hogePlayer.hs

--import Control.Monad.Trans
--import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.Map as Map

import qualified Information as Info
import qualified Initialization as Init
import qualified PerStep as PS
import qualified Geometric as Geo
import System.IO
import Data.List

main :: IO ()
main =
  Init.getInitialInformation >>=
  \initInfo -> Init.initialOutput >>
  (runReaderT mainLoop initInfo)

mainLoop :: ReaderT Info.InitialInformation IO ()
mainLoop = PS.getPerStepInformation >>=
  \pi -> safeDecision pi >>=
  \as -> PS.perStepOutput as >> mainLoop

-- Control.Monad.Trans.Reader.ask :: Monad m => ReaderT r m r
-- lift :: (Monad m, MonadTrans t) => m a -> t m a

-- safeDecision :: Info.PerStepInformation -> ReaderT Info.InitialInformation IO (Int, Int)
-- safeDecision (Info.PSI ste reTi pSta oSta obPo) =  return (1,1)

safeDecision :: Info.PerStepInformation -> ReaderT Info.InitialInformation IO (Int, Int)
safeDecision (Info.PSI ste reTi (pos,vel) oSta obPo) = ask >>= \(Info.II _ _ width length vl) ->
              return $ head $ filter (adjastVel obPo pos vel width vl length) $ filter (adjastVelHorizon pos vel width) $ filter (isNone obPo pos vel width vl length) $ filter (isInCource pos vel width) $ makeAccelerationCandidates vel


adjastVelHorizon :: Info.Pos -> Info.Velocity -> Int -> (Int,Int) -> Bool
adjastVelHorizon (px,_) (vx,_) width (acx,_) 
  | 2 >= px && vx == -2 = 0 < acx
  | (width-1)-2 <= px && vx == 2 = acx < 0
  | otherwise           = True

adjastVel :: Info.ObstaclePoints -> Info.Pos -> Info.Velocity -> Int -> Int -> Int -> (Int,Int) -> Bool
adjastVel obPo (px,py) (vx,vy) width visinLimit length (acx,acy) 
  | length <= py+vy+acy = True
  | distanceBetweenObstaclePointAndPlyer >= Just 2 && vy == 2 = acy < 0
  | vx < 0 && vy==2 && obPo!!(estimatedOPindex+width)==Info.Obstacle = acy < 0
  | 0 < vx && vy==2 && obPo!!(estimatedOPindex+width+1)==Info.Obstacle = acy < 0
  | otherwise           = True
          where
            distanceBetweenObstaclePointAndPlyer = findIndex (Info.Obstacle==) (map ((!!) obPo) [(visinLimit*width+px),(visinLimit*width+px)+width,(visinLimit*width+px)+2*width,(visinLimit*width+px)+3*width])
            estimatedOPindex = ( (visinLimit * width + px) + (width * (vy+acy)) + (vx+acx) ) -- (vl * width + px) is present player's position

isNone :: Info.ObstaclePoints -> Info.Pos -> Info.Velocity -> Int -> Int -> Int -> (Int,Int) -> Bool
isNone obPo (px,py) (vx,vy) width vl length (acx,acy) = if vl < (vy+acy) then False else if length <= py+vy+acy then True else Info.None == obPo!!(estimatesOPindex (vx+acx) (vy+acy))&& (if vx+acx == 0 && vy+acy == 2 then Info.None == obPo!!((vl*width+px)+width) else True) && not ( or ( map (Geo.doesCross (Geo.L(Geo.P(fromIntegral px,fromIntegral py),Geo.P(fromIntegral (px+vx+acx),fromIntegral (py+vy+acy)) ) ) ) obstacleLineList))
                            where 
                             obstacleLineList = [ Geo.L(Geo.P(fromIntegral lx,fromIntegral ly),Geo.P(fromIntegral rx,fromIntegral ry)) | (lx,ly) <- pointList, (rx,ry) <- pointList,
                                                abs(lx-rx) < 2 && abs(ly-ry) < 2 && (lx,ly)/=(rx,ry)]
                             pointList = [ (px + x',py + y') | (x',y') <- [(x,y)|y<-ylist,x<-xlist],Info.Obstacle==obPo!!(estimatesOPindex x' y')]
                             xlist = if (vx+acx) >= 0 then [0..(vx+acx)] else [(vx+acx)..0]
                             ylist = if (vy+acy) >= 0 then [0..(vy+acy)] else [(vy+acy)..0]
                             estimatesOPindex x y = ( (vl*width+px) + (width*y) + x ) -- (vl*width+px) is present player's position

isInCource :: Info.Pos -> Info.Velocity -> Int -> (Int,Int) -> Bool
isInCource (px,py) (vx,vy) width (acx,acy) = 0 <= (px + vx + acx) && (px + vx + acx) < width && 0 <= (py + vy + acy)


makeAccelerationCandidates :: Info.Velocity -> [(Int,Int)]
makeAccelerationCandidates vel = convertTotuple $ quicksort $ isIllegalVelocity $ isLazy $ makeAllCombination vel


estimatePosition :: Info.PlayerState -> (Int,Int) -> Info.Pos
estimatePosition ((px,py),(vx,vy)) (x,y) = (px+vx+x,py+vy+y)

estimateVelocity :: Info.PlayerState -> (Int,Int) -> Info.Velocity
estimateVelocity (_,(vx,vy)) (x,y) = (vx+x,vy+y)

hogeL :: Map.Map (Int,Int) Int
hogeL = Map.fromList [
  ((-2,2),1),
  ((-1,2),2),
  ((-2,1),3),
  ((-1,1),4),
  ((0,2),5),
  ((0,1),6),
  ((1,2),7),
  ((2,2),8),
  ((2,1),9),
  ((1,1),10),
  ((-2,0),11),
  ((-1,0),12),
  ((2,0),13),
  ((1,0),14),
  ((-1,-1),15),
  ((-2,-1),16),
  ((-2,-2),17),
  ((-1,-2),18),
  ((0,-1),19),
  ((0,-2),20),
  ((1,-1),21),
  ((2,-1),22),
  ((1,-2),23),
  ((2,-2),24),
  ((0,0),25)]

hogeR :: Map.Map (Int,Int) Int
hogeR = Map.fromList [
  ((-2,0),1),
  ((-1,0),2),
  ((-2,2),3),
  ((-2,1),4),
  ((-1,2),5),
  ((-1,1),6),
  ((0,2),7),
  ((0,1),8),
  ((2,2),9),
  ((1,2),10),
  ((2,1),11),
  ((1,1),12),
  ((2,0),13),
  ((1,0),14),
  ((1,-2),15),
  ((2,-2),16),
  ((2,-1),17),
  ((1,-1),18),
  ((0,-2),19),
  ((0,-1),20),
  ((-2,-2),21),
  ((-2,-1),22),
  ((-1,-2),23),
  ((-1,-1),24),
  ((0,0),25)]

quicksort :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
quicksort [] = []
quicksort ((vx,vy,ax,ay):xs) = 
          let smallerOrEqual = [ (vx',vy',ax',ay') | (vx',vy',ax',ay') <- xs, Map.lookup (vx'+ax',vy'+ay') hogeL <= Map.lookup (vx+ax,vy+ay) hogeL]
              larger = [(vx',vy',ax',ay') | (vx',vy',ax',ay') <- xs, Map.lookup (vx'+ax',vy'+ay') hogeL > Map.lookup (vx+ax,vy+ay) hogeL]
          in quicksort smallerOrEqual ++ [(vx,vy,ax,ay)] ++ quicksort larger

isIllegalVelocity :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
isIllegalVelocity  xs = filter func xs
                   where 
                         func = \(vx,vy,ax,ay) -> -3 < vx+ax && vx+ax < 3 && -3 < vy+ay && vy+ay < 3

isLazy :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
isLazy  xs = filter func xs
        where 
              func = \(vx,vy,ax,ay) -> vx /= 0 || ax /= 0 || 0 /= vy || ay /= 0


foo :: [(Int,Int,Int,Int)] 
foo = [ (-1,0,x,y) | x <- [-1..1], y <- [-1..1]]

makeAllCombination :: Info.Velocity -> [(Int,Int,Int,Int)] 
makeAllCombination (vx,vy) = [ (vx,vy,x,y) | x <- [-1..1], y <- [-1..1]]


convertTotuple :: [(Int,Int,Int,Int)] -> [(Int,Int)]
convertTotuple xs = map (\(vx,vy,ax,ay) -> (ax,ay)) xs


-- isCourceOut :: Info.Pos -> Info.Velocity -> (Int,Int) -> Int -> Bool
-- isCourceOut (px,_) (vx,_) (acx,_) w = (px + vx + acx) < 0 || w < (px + vx + acx) 
