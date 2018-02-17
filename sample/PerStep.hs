module PerStep where

import System.IO
import Information as Info
import Control.Monad
--import Control.Monad.Trans
-- import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- input information example per step
-- 0
-- 2000
-- 5 0 0 0
-- 9 0 0 0
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
-- 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
-- 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
-- 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 
-- 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 
-- 0 0 0 0 0 0 1 1 0 0 0 0 0 0 0 
-- 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 
-- 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 
-- 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 
-- 0 0 0 0 1 1 1 1 1 1 0 0 0 0 0 

getPerStepInformation :: ReaderT Info.InitialInformation IO PerStepInformation
getPerStepInformation =
  lift (fmap read getLine) >>=
  \s -> lift (fmap read getLine) >>=
  \rt -> lift (fmap (map read . words) getLine) >>=
  \[px,py,pvx,pvy] -> lift (fmap (map read . words) getLine) >>=
  \[ox,oy,ovx,ovy] -> getObstaclePoints >>=
  \ops -> return (PSI s rt ((px,py),(pvx,pvy)) ((ox,oy),(ovx,ovy)) ops)

getObstaclePoints :: ReaderT Info.InitialInformation IO ObstaclePoints
getObstaclePoints = 
  Control.Monad.Trans.Reader.ask >>=
  \(II _ _ _ _ d) -> 
  lift (fmap concat (replicateM (2*d+1) (fmap (map (intToObstaclestate . read) . words) getLine))) 

perStepOutput :: (Int, Int) -> ReaderT Info.InitialInformation IO ()
perStepOutput (ax, ay) = lift (putStrLn ((show ax) ++ " " ++ (show ay)) ) >> lift (hFlush stdout)
