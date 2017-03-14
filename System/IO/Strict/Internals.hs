{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module     : System.IO.Strict.Internals
-- Copyright  : (c) Nicolas Pouillard 2009
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability  : provisional
--
-- This module exports the internals of "System.IO.Strict" so that other packages can extend the
-- 'SIO' monad. This module has to be used with great care: by lifting a lazy
-- function or a function that let leaks its lazy arguments, one breaks the only purpose
-- of the "System.IO.Strict" module.
--------------------------------------------------------------------

module System.IO.Strict.Internals
(
  -- * Types
  SIO(..),

  -- * Running the 'SIO' monad
  run,

  -- * A stricter 'return'
  return',
  
  -- * Wrapping functions
  wrap0, -- :: IO a -> SIO a
  wrap0', -- :: NFData sa => IO sa -> SIO sa
  wrap1, -- :: (a -> IO b) -> a -> SIO b
  wrap1', -- :: NFData sb => (a -> IO sb) -> a -> SIO sb
  wrap2, -- :: (a -> b -> IO c) -> a -> b -> SIO c
  wrap2', -- :: NFData sc => (a -> b -> IO sc) -> a -> b -> SIO sc
  wrap3, -- :: (a -> b -> c -> IO d) -> a -> b -> c -> SIO d
  wrap3', -- :: NFData sd => (a -> b -> c -> IO sd) -> a -> b -> c -> SIO sd
)
where

import Prelude ((.), seq)
import Control.DeepSeq (NFData(..))
import Control.Monad
import Control.Monad.Fix
-- import Control.Monad.Error
import Control.Applicative
import System.IO (IO)

newtype SIO a = SIO { rawRun :: IO a }
  deriving (Functor, Applicative, Monad, MonadFix {-, MonadPlus, MonadError IOError-}) -- Not MonadIO !!!!

-- | 'run' allows to return to the wider world of 'IO's.
run :: NFData sa => SIO sa -> IO sa
run mx = rawRun mx >>= return'
{-# INLINE run #-}

-- | A stricter version of 'return', that works for every monad.
return' :: (Monad m, NFData sa) => sa -> m sa
return' x = rnf x `seq` return x
{-# INLINE return' #-}

-- | Wraps a strict /IO/ computation without arguments.
wrap0 :: IO a -> SIO a
wrap0 = SIO
{-# INLINE wrap0 #-}

-- | Wraps a lazy /IO/ computation without arguments and forces its contents.
wrap0' :: NFData sa => IO sa -> SIO sa
wrap0' mx = SIO (do x <- mx; rnf x `seq` return x)
{-# INLINE wrap0' #-}

-- | Wraps a strict /IO/ computation with a single argument.
wrap1 :: (a -> IO b) -> a -> SIO b
wrap1 = (wrap0 .)
{-# INLINE wrap1 #-}

-- | Wraps a lazy /IO/ computation with a single argument and forces its contents.
wrap1' :: NFData sb => (a -> IO sb) -> a -> SIO sb
wrap1' = (wrap0' .)
{-# INLINE wrap1' #-}

-- | Wraps a strict /IO/ computation with two arguments.
wrap2 :: (a -> b -> IO c) -> a -> b -> SIO c
wrap2 = (wrap1 .)
{-# INLINE wrap2 #-}

-- | Wraps a strict /IO/ computation with two arguments and forces its contents.
wrap2' :: NFData sc => (a -> b -> IO sc) -> a -> b -> SIO sc
wrap2' = (wrap1' .)
{-# INLINE wrap2' #-}

-- | Wraps a strict /IO/ computation with two arguments.
wrap3 :: (a -> b -> c -> IO d) -> a -> b -> c -> SIO d
wrap3 = (wrap2 .)
{-# INLINE wrap3 #-}

-- | Wraps a strict /IO/ computation with two arguments and forces its contents.
wrap3' :: NFData sd => (a -> b -> c -> IO sd) -> a -> b -> c -> SIO sd
wrap3' = (wrap2' .)
{-# INLINE wrap3' #-}
