module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad.ST.Strict (runST)

import Data.Aeson (decodeStrict, encodeFile)
import Data.ByteString qualified as BS
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Vector qualified as V

import RelatedPostGen (Post, computeRelatedPosts)

inputFile, outputFile :: FilePath
inputFile = "../posts.json"
outputFile = "../related_posts_haskell.json"

main :: IO ()
main = do
  Just (posts :: V.Vector Post) <- (evaluate . force) . decodeStrict =<< BS.readFile inputFile

  start <- getPOSIXTime
  relatedPosts <- evaluate . force $! runST (computeRelatedPosts posts)
  end <- getPOSIXTime

  putStrLn $ "Processing time (w/o IO): " ++ show (realToFrac ((end - start) * 1000) :: Double) ++ "ms"
  encodeFile outputFile relatedPosts
