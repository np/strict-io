--------------------------------------------------------------------
-- |
-- Module     : Data.IORef.Strict
-- Copyright  : (c) Nicolas Pouillard 2009
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability  : provisional
--
-- Mutable references of strict values in the 'SIO' monad.
--
-- The type of references remains the same as in the 'IO' monad
-- and is just re-exported here.
--------------------------------------------------------------------

module Data.IORef.Strict
  (IORef
  ,newIORef
  ,readIORef
  ,writeIORef
  ,modifyIORef
  ,atomicModifyIORef
  ,mkWeakIORef
  )
where

import System.IO.Strict.Internals (SIO(..))
import System.Mem.Weak (Weak)
import qualified Data.IORef as IO
import Data.IORef (IORef)
import Control.DeepSeq (NFData(..))

-- | Build a new 'IORef', but force the value before storing it.
newIORef :: NFData sa => sa -> SIO (IORef sa)
newIORef value = rnf value `seq` SIO (IO.newIORef value)

-- | Read the value of an 'IORef'
readIORef :: IORef a -> SIO a
readIORef = SIO . IO.readIORef

-- | Deeply force a value and write it into an 'IORef'
writeIORef :: NFData sa => IORef sa -> sa -> SIO ()
writeIORef ref value = rnf value `seq` SIO $ IO.writeIORef ref value

-- | Mutate the contents of an 'IORef'
modifyIORef :: NFData sa => IORef sa -> (sa -> sa) -> SIO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

-- | Atomically modifies the contents of an 'IORef'.
--
-- This function is useful for using 'IORef' in a safe way in a multithreaded program.
-- If you only have one 'IORef', then using 'atomicModifyIORef' to access and modify
-- it will prevent race conditions.
--
-- Extending the atomicity to multiple 'IORef's is problematic, so it is recommended that
-- if you need to do anything more complicated then using "Control.Concurrent.MVar.MVar"
-- instead is a good idea.
atomicModifyIORef :: (NFData sa, NFData sb) => IORef sa -> (sa -> (sa, sb)) -> SIO sb
atomicModifyIORef ref f = SIO $ do x <- IO.atomicModifyIORef ref (rnf' . f)
                                   rnf x `seq` return x
        -- since the result of 'f' is a pair which has to forced 
  where rnf' x = rnf x `seq` x

-- | Make a 'Weak' pointer to an 'IORef'
mkWeakIORef :: IORef a -> SIO () -> SIO (Weak (IORef a))
mkWeakIORef ref (SIO finalizer) = SIO $ IO.mkWeakIORef ref finalizer

