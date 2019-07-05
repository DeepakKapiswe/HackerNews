module Main where

import           Control.Monad
import           Control.Concurrent
import           Helpers

-- | The main function to be called
-- forkIO is used so that for all the 30 Stories parallel requests
-- could be made. MVar is used to print the result in somewhat
-- deterministic manner.
main :: IO ()
main = do
  sIds <- get30TopStoryIds
  mvar <- newMVar ()
  case sIds of
    Just ids -> do 
      forM_ ids $ \sid -> forkIO (processStory mvar sid)
      threadDelay (10 * 100000000)      -- A naive approach for waiting before dying
    _ -> print "oops something went wrong can't retrieve the data"

