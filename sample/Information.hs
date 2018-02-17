module Information where

type Pos = (Int, Int)
type Velocity = (Int, Int)
type PlayerState = (Pos, Velocity)

data ObstacleState = None | Obstacle deriving (Eq,Show)
type ObstaclePoints = [ObstacleState]

-- 4.1
data InitialInformation = II { remainingTime' :: Int,
                               stepLimit      :: Int,
                               width          :: Int,
                               length'        :: Int,
                               visionLimit    :: Int }

-- 4.3
data PerStepInformation = PSI { step           :: Int,
                                remainingTime  :: Int,
                                pState         :: PlayerState,
                                oState         :: PlayerState,
                                obstaclePoints :: ObstaclePoints } deriving (Show)

intToObstaclestate :: Int -> ObstacleState
intToObstaclestate 0 = None
intToObstaclestate _ = Obstacle
