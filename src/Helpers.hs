{-# LANGUAGE BangPatterns #-}

module Helpers
 ( get30TopStoryIds
 , processStory
 ) where

import           Data.Aeson (eitherDecode')
import           Network.HTTP.Simple

import qualified Data.ByteString.Lazy.Char8 as BL8

import           Control.Monad
import           Control.Concurrent
import           Data.Maybe (fromJust)
import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Types


-- | Takes a Story Id and prints its title with top commentors and
-- their comment count
processStory :: MVar () -> Int -> IO ()
processStory mvar sid = do
  st <- getItemById sid
  let parsedStory = (eitherDecode' st :: Either String Story)
  case parsedStory of
    Right st -> do
      comments <- getAllCommentStory st
      let !top10commentors = getTop10Commentors comments
      takeMVar mvar

      putStrLn $ "\n$$ ------- ------ -------- -------- ------ $$"
      putStrLn $ "Top comments of Story id: " <> (show sid)
      putStrLn $ "Story Title: " <> (sTitle st)
      mapM_ putStrLn $ decorate <$> zip [1..10] top10commentors
      putStrLn $ "$$ ------- ------ -------- -------- ------ $$ \n"

      putMVar mvar ()

    _        -> print $ "error parsing story id: " <> (show sid)
  where
    decorate :: (Int, (String, Int)) -> String
    decorate (slNum, (name, count)) = show slNum <> "\t" <> name <> "\t -->> \t" <> show count

-- | Runs a http request query and returns the response body
-- along with printing the status code on stderr
runQuery :: String -> IO (BL8.ByteString)
runQuery uri = do
  req      <- parseRequest $ "GET " <> uri
  response <- httpLBS req
  let responseBody = getResponseBody response
  return responseBody

-- | Get Top 30 Story Ids
get30TopStoryIds :: IO (Maybe [Int])
get30TopStoryIds = do
  resp <- runQuery getTopStoriesUri
  case (eitherDecode' resp :: Either String [Int]) of
    Right ids -> return (Just $ take 30 ids)
    _         -> return Nothing

-- | get an Item by Id
getItemById :: Int -> IO (BL8.ByteString)
getItemById itemId = do
  let uri = mkItemUriById (show itemId)
  runQuery uri


-- | Get all the comments for a given story
getAllCommentStory :: Story -> IO [Comment]
getAllCommentStory st = case (sKids st) of
  Nothing -> pure []
  Just kids -> do
    dcs'             <- forM kids getCommentById
    let directChilds = fromJust <$> filter (/= Nothing) dcs'
    subChilds        <- mconcat <$> forM directChilds getAllComments
    return           $ directChilds <> subChilds


-- | Get a comment by Id
getCommentById :: Int -> IO (Maybe Comment)
getCommentById cmtId = do
  cmt <- getItemById cmtId
  case eitherDecode' cmt of
    Right comment -> return (Just comment)
    _             -> return Nothing


-- | Given a Comment gather all of the child comments
getAllComments :: Comment -> IO [Comment]
getAllComments cmt = case (cKids cmt) of
  Nothing   -> pure []
  Just kids -> do
    dcs'             <- forM kids getCommentById
    let directChilds = fromJust <$> filter (/= Nothing) dcs'
    subChilds        <- mconcat (getAllComments <$> directChilds)
    return $ directChilds <> subChilds


-- | Given a list of Comments for a story return the name of top 10
-- commentors and their comment count
getTop10Commentors :: [Comment] -> [(String, Int)]
getTop10Commentors cmts =
  let commentorCounts = (\cmt -> (cBy cmt, 1)) <$> cmts
      cMap = Map.fromListWith (+) commentorCounts
      top10 = take 10 . sortOn (negate.snd) $ Map.assocs cMap
  in top10


-- | Uri to get TopStories
getTopStoriesUri :: String
getTopStoriesUri =
  "https://hacker-news.firebaseio.com/v0/topstories.json?print=pretty"


-- | Given an Item Id returns a Uri to get that item.
mkItemUriById :: String -> String
mkItemUriById itemId = "https://hacker-news.firebaseio.com/v0/item/"
  <> itemId <> ".json?print=pretty"
