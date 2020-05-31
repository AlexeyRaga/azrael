{-# LANGUAGE BangPatterns #-}
module Control.Monad.Lazy
( interleaveUnfoldrIO
)
where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified System.IO.Unsafe       as IO

interleaveUnfoldrIO :: (b -> IO (Maybe (a, b))) -> b -> IO [a]
interleaveUnfoldrIO f z = do
  IO.unsafeInterleaveIO (go z)
  where
    go !b = do
      m <- f b
      case m of
        Nothing      -> pure []
        Just (!a, b') -> do
          rest <- IO.unsafeInterleaveIO (go b')
          pure (a:rest)
