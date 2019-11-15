module ST where

import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)


data MyData = MyData [String] [Int]

reorder :: forall a. [a] -> [a]
reorder (x1 : x2 : xs) = x2 : x1 : xs
reorder xs = xs

reorderMyData :: (forall a. [a] -> [a]) -> (MyData -> MyData)
reorderMyData f (MyData strs ints) = MyData (f strs) (f ints)


newtype ST s a = ST { unST :: IO a }
  deriving (Functor, Applicative, Monad)

newtype STRef s a = STRef { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = ST . fmap STRef . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = ST . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef ir) = ST . writeIORef ir

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef (STRef ir) = ST . modifyIORef ir

runST :: forall a. (forall s. ST s a) -> a
runST (ST io) = unsafePerformIO io

test :: ST s Int
test = do
  r <- newSTRef 0
  modifySTRef r (+1)
  readSTRef r

main :: IO ()
main =
  let x = runST test in print x

{-
bad :: ST s (STRef s Int)
bad = do
  r <- newSTRef 0
  modifySTRef r (+1)
  pure r

veryBad :: STRef s Int -> ST s Int
veryBad r = do
  modifySTRef r (+1)
  readSTRef r

data Labels = One | Two | Three

veryVeryBad :: Int
veryVeryBad =
  let r = runST (newSTRef 0)
  in runST (veryBad r)
-}
