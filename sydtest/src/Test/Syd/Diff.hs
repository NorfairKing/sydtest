{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports -Werror=name-shadowing #-}

module Test.Syd.Diff
  ( -- * Diffing
    Diff,
    PolyDiff (..),
    getTextDiff,
    getStringDiff,
    getGroupedStringDiff,
    getVectorDiff,
    getGroupedVectorDiff,
    getVectorDiffBy,
    getGroupedVectorDiffBy,

    -- ** Internals
    Edit (..),
    getEditScript,
    getEditScriptBy,
    computeDiffFromEditScript,
    computeGroupedDiffFromEditScript,

    -- ** Backwards compatibility with @Diff@
    getDiff,
    getDiffBy,
    getGroupedDiff,
    getGroupedDiffBy,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Maybe (fromJust)
import Data.STRef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV

type Diff a = PolyDiff a a

mapDiff :: (a -> b) -> Diff a -> Diff b
mapDiff f = bimapPolyDiff f f

-- | A value is either from the 'First' list, the 'Second' or from 'Both'.
-- 'Both' contains both the left and right values, in case you are using a form
-- of equality that doesn't check all data (for example, if you are using a
-- newtype to only perform equality on side of a tuple).
data PolyDiff a b = First a | Second b | Both a b
  deriving (Show, Eq)

bimapPolyDiff :: (a -> c) -> (b -> d) -> PolyDiff a b -> PolyDiff c d
bimapPolyDiff f g = \case
  First a -> First (f a)
  Second b -> Second (g b)
  Both a b -> Both (f a) (g b)

-- |
--
-- For backward compatibility with 'Diff', use more specific functions if you can.
getDiff :: Eq a => [a] -> [a] -> [Diff a]
getDiff = getDiffBy (==)

-- |
--
-- For backward compatibility with 'Diff', use more specific functions if you can.
getDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff a b]
getDiffBy eq as bs = V.toList (getVectorDiffBy eq (V.fromList as) (V.fromList bs))

-- |
--
-- For backward compatibility with 'Diff', use more specific functions if you can.
getGroupedDiff :: Eq a => [a] -> [a] -> [Diff [a]]
getGroupedDiff = getGroupedDiffBy (==)

-- |
--
-- For backward compatibility with 'Diff', use more specific functions if you can.
getGroupedDiffBy :: (a -> b -> Bool) -> [a] -> [b] -> [PolyDiff [a] [b]]
getGroupedDiffBy eq as bs = V.toList (V.map (bimapPolyDiff V.toList V.toList) (getGroupedVectorDiffBy eq (V.fromList as) (V.fromList bs)))

-- | Text diff
--
-- Uses pack and unpack, so does not roundtrip.
-- It uses pack and unpack because 'Text' is not the same as @Vector Char@;
-- You can't index a text in O(1) time, it takes O(n) time.
getTextDiff :: Text -> Text -> Vector (Diff Text)
getTextDiff expected actual = V.map (mapDiff packFromVector) $ getGroupedVectorDiff (unpackToVector expected) (unpackToVector actual)
  where
    packFromVector :: Vector Char -> Text
    packFromVector = T.pack . V.toList
    unpackToVector :: Text -> Vector Char
    unpackToVector = V.fromList . T.unpack

-- | 'String' diff
--
-- You probably want to use 'getTextDiff' with packed strings instead, but this
-- function doesn't have the roundtripping problem that 'getTextDiff' has.
getStringDiff :: String -> String -> [Diff Char]
getStringDiff actual expected = V.toList (getVectorDiff (V.fromList actual) (V.fromList expected))

-- | Grouped 'String' diff
--
-- Like 'getStringDiff' but with entire strings instead of individual characters.
getGroupedStringDiff :: String -> String -> [Diff String]
getGroupedStringDiff actual expected = V.toList $ V.map (mapDiff V.toList) $ getGroupedVectorDiff (V.fromList actual) (V.fromList expected)

-- | Diff two vectors
--
-- Prefer 'getGroupedVectorDiff' for performance reasons.
getVectorDiff :: Eq a => Vector a -> Vector a -> Vector (Diff a)
getVectorDiff = getVectorDiffBy (==)

-- | Diff two vectors with different types using a custom equality operator
--
-- Prefer 'getGroupedVectorDiffBy' for performance reasons.
getVectorDiffBy :: forall a b. (a -> b -> Bool) -> Vector a -> Vector b -> Vector (PolyDiff a b)
getVectorDiffBy eq old new = computeDiffFromEditScript old new (getEditScriptBy eq old new)

-- | Diff two vectors with grouped results
getGroupedVectorDiff :: Eq a => Vector a -> Vector a -> Vector (Diff (Vector a))
getGroupedVectorDiff = getGroupedVectorDiffBy (==)

-- | Diff two vectors with grouped results using a custom equality operator
getGroupedVectorDiffBy :: forall a b. (a -> b -> Bool) -> Vector a -> Vector b -> Vector (PolyDiff (Vector a) (Vector b))
getGroupedVectorDiffBy eq old new = computeGroupedDiffFromEditScript old new (getEditScriptBy eq old new)

-- | Compute the edit script to turn a given vector into the second given vector
getEditScript :: forall a. Eq a => Vector a -> Vector a -> Vector Edit
getEditScript = getEditScriptBy (==)

-- | Compute the edit script to turn a given vector into the second given vector with a custom equality operator
--
-- From https://blog.robertelder.org/diff-algorithm/
getEditScriptBy :: forall a b. (a -> b -> Bool) -> Vector a -> Vector b -> Vector Edit
getEditScriptBy eq old new = V.fromList $ DList.toList $ runST $ go old new 0 0
  where
    go :: forall s. Vector a -> Vector b -> Int -> Int -> ST s (DList Edit)
    go e f i j = do
      -- N,M,L,Z = len(e),len(f),len(e)+len(f),2*min(len(e),len(f))+2
      let upperN :: Int
          upperN = V.length e

      let upperM :: Int
          upperM = V.length f

      let upperL :: Int
          upperL = upperN + upperM

      let upperZ :: Int
          upperZ = 2 * min upperN upperM + 2

      -- if N > 0 and M > 0:
      if upperN > 0 && upperM > 0
        then do
          -- w,g,p = N-M,[0]*Z,[0]*Z
          let w :: Int
              w = upperN - upperM

          g <- MV.replicate upperZ 0 :: ST s (MVector s Int)

          p <- MV.replicate upperZ 0 :: ST s (MVector s Int)

          -- for h in range(0, (L//2+(L%2!=0))+1):
          let hs :: [Int]
              hs = [0 .. ((upperL `quot` 2) + (if odd upperL then 1 else 0))]

          fmap fromJust $ -- We use fromJust because the algorithm is guaranteed to terminate
            forUntilJust hs $ \h -> do
              -- for r in range(0, 2):
              forUntilJust [0, 1 :: Int] $ \r -> do
                -- c,d,o,m = (g,p,1,1) if r==0 else (p,g,0,-1)
                let (c, d, o, m) = if r == 0 then (g, p, 1, 1) else (p, g, 0, -1)

                -- for k in range(-(h-2*max(0,h-M)), h-2*max(0,h-N)+1, 2):
                let lo :: Int
                    lo = -(h - 2 * max 0 (h - upperM))

                let hi :: Int
                    hi = h - 2 * max 0 (h - upperN)

                let ks :: [Int]
                    ks = [lo, lo + 2 .. hi]

                forUntilJust ks $ \k -> do
                  -- a = c[(k+1)%Z] if (k==-h or k!=h and c[(k-1)%Z]<c[(k+1)%Z]) else c[(k-1)%Z]+1
                  initAVal <- do
                    let part1 = k == -h

                    let part2 = k /= h

                    -- (k+1)%Z
                    let kp1Ix = (k + 1) `modPortable` upperZ

                    -- (k-1)%Z
                    let km1Ix = (k - 1) `modPortable` upperZ
                    if part1
                      then MV.unsafeRead c kp1Ix
                      else do
                        if part2
                          then do
                            -- c[(k-1)%Z]
                            km1 <- MV.unsafeRead c km1Ix

                            -- c[(k+1)%Z]
                            kp1 <- MV.unsafeRead c kp1Ix
                            let part3 = km1 < kp1
                            pure $
                              if part3
                                then kp1
                                else km1 + 1
                          else do
                            km1 <- MV.unsafeRead c km1Ix

                            pure $ km1 + 1

                  a <- newSTRef initAVal

                  -- b = a-k
                  let initBVal :: Int
                      initBVal = initAVal - k

                  b <- newSTRef initBVal

                  -- s,t = a,b
                  s <- newSTRef initAVal
                  t <- newSTRef initBVal

                  -- while a<N and b<M and e[(1-o)*N+m*a+(o-1)]==f[(1-o)*M+m*b+(o-1)]:
                  let computeWhileCond = do
                        aVal <- readSTRef a
                        -- a<N
                        let part1 = aVal < upperN
                        if part1
                          then do
                            bVal <- readSTRef b
                            -- b<M
                            let part2 = bVal < upperM
                            -- e[(1-o)*N+m*a+(o-1)]==f[(1-o)*M+m*b+(o-1)]:
                            let mkPart3 = do
                                  let imo = 1 - o
                                      omi = o - 1
                                  -- e[(1-o)*N+m*a+(o-1)]
                                  leftVal <- do
                                    -- (1-o)*N+m*a+(o-1)
                                    let ix = imo * upperN + m * aVal + omi

                                    pure $ e ! ix
                                  -- f[(1-o)*M+m*b+(o-1)]
                                  rightVal <- do
                                    -- (1-o)*M+m*b+(o-1)
                                    let ix = imo * upperM + m * bVal + omi

                                    pure $ f ! ix
                                  pure $ leftVal `eq` rightVal
                            part2 &&. mkPart3
                          else pure False
                  whileM_ computeWhileCond $ do
                    --   a,b = a+1,b+1
                    modifySTRef a (+ 1)
                    modifySTRef b (+ 1)
                  -- c[k%Z],z=a,-(k-w)
                  do
                    aVal <- readSTRef a

                    MV.unsafeWrite c (k `modPortable` upperZ) aVal

                  let z = -(k - w)

                  -- if L%2==o and z>=-(h-o) and z<=h-o and c[k%Z]+d[z%Z] >= N:
                  let -- L%2==o
                      part1 = upperL `rem` 2 == o
                      -- (h-o)
                      hmo = h - o
                      -- z>=-(h-o)
                      part2 = z >= -hmo
                      -- z<=h-o
                      part3 = z <= hmo
                      -- c[k%Z]+d[z%Z] >= N
                      mkPart4 = do
                        ck <- MV.unsafeRead c (k `modPortable` upperZ)

                        dz <- MV.unsafeRead d (z `modPortable` upperZ)

                        pure (ck + dz >= upperN)
                      mkCondition = part1 &&. (part2 &&. (part3 &&. mkPart4))
                  condition <- mkCondition
                  if condition
                    then do
                      -- D,x,y,u,v = (2*h-1,s,t,a,b) if o==1 else (2*h,N-a,M-b,N-s,M-t)
                      (upperD, x, y, u, v) <- do
                        aVal <- readSTRef a
                        bVal <- readSTRef b
                        sVal <- readSTRef s
                        tVal <- readSTRef t
                        pure $
                          if o == 1
                            then (2 * h - 1, sVal, tVal, aVal, bVal)
                            else (2 * h, upperN - aVal, upperM - bVal, upperN - sVal, upperM - tVal)

                      -- if D > 1 or (x != u and y != v):
                      if upperD > 1 || (x /= u && y /= v)
                        then do
                          -- return diff(e[0:x],f[0:y],i,j)+diff(e[u:N],f[v:M],i+u,j+v)
                          -- diff(e[0:x],f[0:y],i,j)
                          firstHalf <- go (V.slice 0 x e) (V.slice 0 y f) i j
                          -- diff(e[u:N],f[v:M],i+u,j+v)
                          secondHalf <- go (sliceIx u upperN e) (sliceIx v upperM f) (i + u) (j + v)
                          pure (Just (firstHalf <> secondHalf))
                        else -- elif M > N:

                          if upperM > upperN
                            then do
                              -- return diff([],f[N:M],i+N,j+N)
                              Just <$> go V.empty (sliceIx upperN upperM f) (i + upperN) (j + upperN)
                            else -- elif M < N:

                              if upperM < upperN
                                then do
                                  --   return diff(e[M:N],[],i+M,j+M)
                                  Just <$> go (sliceIx upperM upperN e) V.empty (i + upperM) (j + upperM)
                                else -- else:
                                --   return []
                                  pure (Just mempty)
                    else pure Nothing
        else do
          -- elif N > 0: #  Modify the return statements below if you want a different edit
          if upperN > 0
            then do
              -- return [{"operation": "delete", "position_old": i+n} for n in range(0,N)]

              pure $ DList.singleton (Delete i upperN)
            else do
              -- return [{"operation": "insert", "position_old": i,"position_new":j+n} for n in range(0,M)]
              if upperM > 0
                then do
                  pure $ DList.singleton (Insert i j upperM)
                else do
                  pure DList.empty

-- | Compute a diff using an edit script.
--
-- Prefer `computeGroupedDiffFromEditScript` for performance reasons.
computeGroupedDiffFromEditScript :: Vector a -> Vector b -> Vector Edit -> Vector (PolyDiff (Vector a) (Vector b))
computeGroupedDiffFromEditScript old new editSteps = V.create $ do
  -- Computing the exact size is cumbersome, so we make enough space and cut down later.
  -- Enough space means: Space between every two edit steps, and one before and one after.
  let size = length editSteps * 2 + 1
  v <- MV.new size
  groupMarker <- newSTRef 0

  oldMarker <- newSTRef 0
  curMarker <- newSTRef 0
  newMarker <- newSTRef 0

  forM_ editSteps $ \editStep -> do
    -- Copy over the pieces between the last and current edit
    inbetweenIx <- readSTRef oldMarker
    let inbetweenLen = oldPosition editStep - inbetweenIx
    when (inbetweenLen > 0) $ do
      groupIx <- readSTRef groupMarker
      oldIx <- readSTRef oldMarker
      newIx <- readSTRef newMarker
      MV.unsafeWrite v groupIx (Both (V.slice oldIx inbetweenLen old) (V.slice newIx inbetweenLen new))
      modifySTRef groupMarker (+ 1)
      modifySTRef oldMarker (+ inbetweenLen)
      modifySTRef curMarker (+ inbetweenLen)
      modifySTRef newMarker (+ inbetweenLen)

    -- Apply the edit
    case editStep of
      Delete oldPosStart upperN -> do
        groupIx <- readSTRef groupMarker
        MV.unsafeWrite v groupIx (First (V.slice oldPosStart upperN old))
        modifySTRef groupMarker (+ 1)
        modifySTRef oldMarker (+ upperN)
        modifySTRef curMarker (+ upperN)
      Insert _ newPosStart upperM -> do
        groupIx <- readSTRef groupMarker
        MV.unsafeWrite v groupIx (Second (V.slice newPosStart upperM new))
        modifySTRef groupMarker (+ 1)
        modifySTRef curMarker (+ upperM)
        modifySTRef newMarker (+ upperM)

  oldIx <- readSTRef oldMarker
  let afterLen = V.length old - oldIx
  when (afterLen > 0) $ do
    newIx <- readSTRef newMarker
    groupIx <- readSTRef groupMarker
    MV.unsafeWrite v groupIx (Both (V.slice oldIx afterLen old) (V.slice newIx afterLen new))
    modifySTRef groupMarker (+ 1)
    modifySTRef oldMarker (+ 1)
    modifySTRef curMarker (+ 1)
    modifySTRef newMarker (+ 1)

  endGroupIx <- readSTRef groupMarker

  pure (MV.slice 0 endGroupIx v)

-- | Compute a diff using an edit script.
--
-- Prefer `computeGroupedDiffFromEditScript` for performance reasons.
computeDiffFromEditScript :: Vector a -> Vector b -> Vector Edit -> Vector (PolyDiff a b)
computeDiffFromEditScript old new editSteps = V.create $ do
  -- The total size of the diff is the size of the old vector plus the number
  -- of inserts that need to happen.
  -- Not minus the number of deletions, because they get a 'First' constructor and stay.
  let totalSize = V.length old + sum (V.map insertLength editSteps)

  v <- MV.new totalSize
  oldMarker <- newSTRef 0
  curMarker <- newSTRef 0
  newMarker <- newSTRef 0

  forM_ editSteps $ \editStep -> do
    let computeWhileCond1 = do
          oldIx <- readSTRef oldMarker
          pure $ oldPosition editStep > oldIx
    -- Copy over the pieces between the last and current edit
    whileM_ computeWhileCond1 $ do
      oldIx <- readSTRef oldMarker
      curIx <- readSTRef curMarker
      newIx <- readSTRef newMarker
      MV.unsafeWrite v curIx (Both (old ! oldIx) (new ! newIx))
      modifySTRef oldMarker (+ 1)
      modifySTRef curMarker (+ 1)
      modifySTRef newMarker (+ 1)

    -- Apply the edit
    case editStep of
      Delete oldPosStart upperN -> do
        curIx <- readSTRef curMarker
        forM_ [0 .. upperN - 1] $ \n -> do
          MV.unsafeWrite v (curIx + n) (First (old ! (oldPosStart + n)))
        modifySTRef oldMarker (+ upperN)
        modifySTRef curMarker (+ upperN)
      Insert _ newPosStart upperM -> do
        curIx <- readSTRef curMarker
        forM_ [0 .. upperM - 1] $ \n -> do
          MV.unsafeWrite v (curIx + n) (Second (new ! (newPosStart + n)))
        modifySTRef curMarker (+ upperM)
        modifySTRef newMarker (+ upperM)

  let computeWhileCond2 = do
        oldIx <- readSTRef oldMarker
        pure $ oldIx < V.length old

  -- Copy over the pieces between the last and current edit
  whileM_ computeWhileCond2 $ do
    oldIx <- readSTRef oldMarker
    curIx <- readSTRef curMarker
    newIx <- readSTRef newMarker

    MV.unsafeWrite v curIx (Both (old ! oldIx) (new ! newIx))
    modifySTRef oldMarker (+ 1)
    modifySTRef curMarker (+ 1)
    modifySTRef newMarker (+ 1)

  pure v

data Edit
  = -- | Delete from the old vector
    Delete
      Int
      -- ^ position in the old vector
      Int
      -- ^ number of items to delete
  | -- | Insert into the old vector
    Insert
      Int
      -- ^ position in the old vector
      Int
      -- ^ position in the new vector
      Int
      -- ^ number of items to insert
  deriving (Show, Eq, Ord)

oldPosition :: Edit -> Int
oldPosition = \case
  Delete i _ -> i
  Insert i _ _ -> i

insertLength :: Edit -> Int
insertLength = \case
  Delete _ _ -> 0
  Insert _ _ m -> m

modPortable :: Int -> Int -> Int
modPortable a b =
  let r = a `rem` b
   in if r >= 0 then r else r + b

sliceIx :: Int -> Int -> Vector a -> Vector a
sliceIx start end = V.slice start (end - start)

-- | Short-circuiting monadic (&&)
(&&.) :: Applicative m => Bool -> m Bool -> m Bool
(&&.) b1 mkB2 = do
  if b1
    then mkB2
    else pure False

forUntilJust :: Monad m => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
forUntilJust [] _ = pure Nothing
forUntilJust (a : rest) func = do
  mRes <- func a
  case mRes of
    Nothing -> forUntilJust rest func
    Just res -> pure $ Just res

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
  where
    go = do
      x <- p
      if x
        then f >> go
        else return ()
