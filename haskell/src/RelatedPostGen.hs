{-# LANGUAGE Strict #-}

module RelatedPostGen (module RelatedPostGen) where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.ST.Strict (ST)

import Data.Aeson (FromJSON, ToJSON)
import Data.Primitive.ByteArray (MutableByteArray, newByteArray, readByteArray, writeByteArray)
import Data.Text.Short (ShortText)
import Data.Vector (Vector, indexed, (!))
import Data.Vector qualified as V
import Data.Vector.Hashtables qualified as H
import Data.Vector.Mutable qualified as VM
import Data.Vector.Storable.Mutable (STVector)
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word32, Word8)

import Foreign.Storable.Tuple ()

import GHC.Generics (Generic)

type HashTable s k v = H.Dictionary (H.PrimState (ST s)) VM.MVector k VM.MVector v

type TagMap s = HashTable s ShortText (STVector s Word32)

data Post = MkPost
  { _id :: !ShortText
  , tags :: !(Vector ShortText)
  , title :: !ShortText
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, NFData)

data RelatedPosts = MkRelatedPosts
  { _id :: !ShortText
  , tags :: !(Vector ShortText)
  , related :: !(Vector Post)
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, NFData)

limitTopN :: Int
limitTopN = 5

computeRelatedPosts :: Vector Post -> ST s (Vector RelatedPosts)
computeRelatedPosts posts = do
  !tagMap :: TagMap s <- H.initialize 0
  let !postsIdx = indexed posts
  populateTagMap tagMap postsIdx
  buildRelatedPosts tagMap postsIdx
{-# INLINE computeRelatedPosts #-}

populateTagMap :: TagMap s -> Vector (Int, Post) -> ST s ()
populateTagMap tagMap postsIdx = do
  V.forM_ postsIdx \(!ix, MkPost{tags}) ->
    V.forM_ tags \tag ->
      H.lookup tagMap tag >>= \case
        Just vec -> do
          -- could be optimized with exponential growth if Vector exposed capacity
          !vec' <- VSM.grow vec 1
          VSM.write vec' (VSM.length vec) (fromIntegral ix)
          H.insert tagMap tag vec'
        Nothing -> H.insert tagMap tag =<< VSM.replicate 1 (fromIntegral ix)
{-# INLINE populateTagMap #-}

buildRelatedPosts :: TagMap s -> Vector (Int, Post) -> ST s (Vector RelatedPosts)
buildRelatedPosts tagMap postsIdx = do
  !sharedTags :: STVector s Word8 <- VSM.replicate (V.length postsIdx) 0 -- shared tag count for each post
  !topN :: STVector s (Word32, Word8) <- VSM.replicate limitTopN (0, 0) -- top N post indices and their shared tag counts
  !mba <- newByteArray 1 -- current minimum shared tag count (Word8); variable as a raw byte array with 1 element
  V.forM postsIdx \(!ix, MkPost{_id, tags}) -> do
    collectSharedTags sharedTags tagMap tags
    VSM.write sharedTags ix 0 -- exclude self from related posts
    rankTopN mba topN sharedTags
    !related <- buildRelated postsIdx topN
    VSM.set topN (0, 0) -- reset
    VSM.set sharedTags 0 -- reset
    pure MkRelatedPosts{_id, tags, related}
{-# INLINE buildRelatedPosts #-}

collectSharedTags :: STVector s Word8 -> TagMap s -> Vector ShortText -> ST s ()
collectSharedTags sharedTags tagMap tags = do
  V.forM_ tags \(!tag) -> do
    !idxs <- H.lookup' tagMap tag
    VSM.forM_ idxs $ VSM.modify sharedTags (+ 1) . fromIntegral
{-# INLINE collectSharedTags #-}

rankTopN :: MutableByteArray s -> STVector s (Word32, Word8) -> STVector s Word8 -> ST s ()
rankTopN mba topN sharedTags = do
  writeByteArray mba 0 (0 :: Word8) -- initialize the count
  VSM.iforM_ sharedTags \(!ix) (!count) -> do
    !minTags <- readByteArray mba 0
    when (count > minTags) do
      !upperBound <- getUpperBound (limitTopN - 2) count topN
      VSM.write topN (upperBound + 1) (fromIntegral ix, count)
      (_, !count') <- VSM.read topN (limitTopN - 1)
      writeByteArray mba 0 count'
 where
  getUpperBound :: Int -> Word8 -> STVector s (Word32, Word8) -> ST s Int
  getUpperBound upper count topN_ = go upper
   where
    go !curr
      | curr >= 0 = do
          !entry@(_, !count') <- VSM.read topN_ curr
          if count > count'
            then do VSM.write topN_ (curr + 1) entry; go (curr - 1)
            else pure curr
      | otherwise = pure curr
{-# INLINE rankTopN #-}

buildRelated :: Vector (Int, Post) -> STVector s (Word32, Word8) -> ST s (Vector Post)
buildRelated posts topN = do
  !res <- VM.unsafeNew limitTopN
  let go !ix
        | ix >= limitTopN = V.unsafeFreeze res
        | otherwise = do
            (!jx, _) <- VSM.read topN ix
            VM.write res ix (snd (posts ! fromIntegral jx))
            go (ix + 1)
  go 0
{-# INLINE buildRelated #-}
