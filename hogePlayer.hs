-- This is the first sample Main Program
-- 
-- $ hlint hogePlayer.hs

import Control.Monad.Trans
import Control.Monad.Reader

import qualified Information as Info
import qualified Initialization as Init
import qualified PerStep as PS
import System.IO

main :: IO ()
main =
  Init.getInitialInformation >>=
  \initInfo -> Init.initialOutput >>
  (runReaderT mainLoop initInfo)

mainLoop :: ReaderT Info.InitialInformation IO ()
mainLoop = PS.getPerStepInformation >>
  lift (putStrLn "0 0") >> 
  lift (hFlush stdout) >> 
  mainLoop             

-- Control.Monad.Trans.Reader.ask :: Monad m => ReaderT r m r
-- lift :: (Monad m, MonadTrans t) => m a -> t m a
