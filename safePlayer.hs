-- This is the first sample Main Program
-- 
-- $ hlint hogePlayer.hs

import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Map as Map

import qualified Information as Info
import qualified Initialization as Init
import qualified PerStep as PS
import qualified Geometric as Geo
import System.IO


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
safeDecision (Info.PSI ste reTi (pos,vel) oSta obPo) = ask >>= \(Info.II _ _ width _ vl) ->
              return $ head $ filter (isNone obPo pos vel width vl) $ filter (isInCource pos vel width) $ makeAccelerationCandidates vel

isNone :: Info.ObstaclePoints -> Info.Pos -> Info.Velocity -> Int -> Int -> (Int,Int) -> Bool
isNone obPo (px,py) (vx,vy) width vl (acx,acy) =  if vl < (vy+acy) then False 
       else Info.None == (obPo !! estimatedOPindex ) && (Info.None == obPo !! (estimatedOPindex+1) || (Info.None == obPo !! (estimatedOPindex-width)))
            where 
            estimatedOPindex = ( (vl * width + px) + (width * (vy+acy)) + (vx+acx) )
                              
isInCource :: Info.Pos -> Info.Velocity -> Int -> (Int,Int) -> Bool
isInCource (px,py) (vx,vy) width (acx,acy) = 0 <= (px + vx + acx) && (px + vx + acx) < width && 0 <= (py + vy + acy)


makeAccelerationCandidates :: Info.Velocity -> [(Int,Int)]
makeAccelerationCandidates vel = convertTotuple $ quicksort $ isIllegalVelocity $ isLazy $ makeAllCombination vel


estimatePosition :: Info.PlayerState -> (Int,Int) -> Info.Pos
estimatePosition ((px,py),(vx,vy)) (x,y) = (px+vx+x,py+vy+y)

estimateVelocity :: Info.PlayerState -> (Int,Int) -> Info.Velocity
estimateVelocity (_,(vx,vy)) (x,y) = (vx+x,vy+y)

-- hoge :: Map.Map (Int,Int) Int
-- hoge = Map.fromList [
--   ((-1,1),1),
--   ((0,1),2),
--   ((1,1),3),
--   ((1,0),4),
--   ((1,-1),5),
--   ((0,-1),6),
--   ((-1,-1),7),
--   ((-1,0),8),
--   ((0,0),9)]

hoge :: Map.Map (Int,Int) Int
hoge = Map.fromList [
  ((-1,1),2),
  ((0,1),3),
  ((1,1),4),
  ((1,0),5),
  ((1,-1),6),
  ((0,-1),7),
  ((-1,-1),8),
  ((-1,0),1),
  ((0,0),9)]

quicksort :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
quicksort [] = []
quicksort ((vx,vy,ax,ay):xs) = 
          let smallerOrEqual = [ (vx',vy',ax',ay') | (vx',vy',ax',ay') <- xs, Map.lookup (vx'+ax',vy'+ay') hoge <= Map.lookup (vx+ax,vy+ay) hoge]
              larger = [(vx',vy',ax',ay') | (vx',vy',ax',ay') <- xs, Map.lookup (vx'+ax',vy'+ay') hoge > Map.lookup (vx+ax,vy+ay) hoge]
          in quicksort smallerOrEqual ++ [(vx,vy,ax,ay)] ++ quicksort larger

isIllegalVelocity :: [(Int,Int,Int,Int)] -> [(Int,Int,Int,Int)]
isIllegalVelocity  xs = filter func xs
                   where 
                         func = \(vx,vy,ax,ay) -> -2 < vx+ax && vx+ax < 2 && -2 < vy+ay && vy+ay < 2

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
