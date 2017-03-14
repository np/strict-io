--------------------------------------------------------------------
-- |
-- Module     : System.IO.Strict
-- Copyright  : (c) Nicolas Pouillard 2009
-- License    : BSD3
--
-- Maintainer : Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability  : provisional
--
-- This module wraps the functions of the "System.IO" module at a different type namely 'SIO'.
--
-- The purpose of this module is to export only strict /IO/ functions, by strict
-- we mean strict in the result. The arguments of these functions may only by partially forced,
-- but when the function returns, the arguments can no longer be forced by the function.
-- When the type of the value to be forced is polymorphic, a 'NFData' constraint is added
-- since we (internally) use 'rnf' to force the value. Then we rely on the behavior
-- of 'NFData' instances to provide the fact that any lazy argument passed to a 'SIO' function
-- will not leak-out after the call.
--
-- These functions do not necessarily use their arguments completely but they do not hold
-- or return any value that could depend on these arguments. If the original functions 
-- from "System.IO" module were already strict, then this module just provides them
-- at another type.
-- Some functions from the original module are famously lazy like the 'getContents' like
-- functions: in this module their results are deeply forced.
--
-- In Haskell, monad operations ('return' and '>>=') have to be lazy. Therefore the 'SIO'
-- monad is not completely strict (i.e. pure values can still be lazy). So in this module we
-- expose the 'return'' function that forces the given value before putting it into the monad.
--
-- Since this module uses the same names as "System.IO", it is designed to be imported /qualified/.
--
-- @
--    import System.IO.Strict (SIO)
--    import qualified System.IO.Strict as SIO
-- @
--------------------------------------------------------------------
module System.IO.Strict
(
  -- * Types
  SIO,
  run,
  return',
  
  -- * Functions stricter than there "System.IO" counterparts
  getContents, -- :: SIO String
  hGetContents, -- :: Handle -> SIO String
  readFile, -- :: FilePath -> SIO String
  read, -- :: (NFData sa, Read sa) => String -> SIO sa
  readLn, -- :: (NFData sa, Read sa) => SIO sa
  fix, -- :: NFData sa => (sa -> SIO sa) -> SIO sa
  withBinaryFile, -- :: NFData sr => FilePath -> IOMode -> (Handle -> SIO sr) -> SIO sr
  withFile, -- :: NFData sr => FilePath -> IOMode -> (Handle -> SIO sr) -> SIO sr

  -- * Functions as strict as there "System.IO" counterparts
  appendFile, -- :: FilePath -> String -> SIO ()
  getChar, -- :: SIO Char
  getLine, -- :: SIO String
  hPrint, -- :: (Show a) => Handle -> a -> SIO ()
  hPutStrLn, -- :: Handle -> String -> SIO ()
  hReady, -- :: Handle -> SIO Bool
  interact, -- :: (String -> String) -> SIO ()
  openBinaryTempFile, -- :: FilePath -> String -> SIO (FilePath, Handle)
  openTempFile, -- :: FilePath -> String -> SIO (FilePath, Handle)
  print, -- :: (Show a) => a -> SIO ()
  putChar, -- :: Char -> SIO ()
  putStr, -- :: String -> SIO ()
  putStrLn, -- :: String -> SIO ()
  writeFile, -- :: FilePath -> String -> SIO ()
  hClose, -- :: Handle -> SIO ()
  hFileSize, -- :: Handle -> SIO Integer
  hFlush, -- :: Handle -> SIO ()
  hGetBuf, -- :: Handle -> Ptr a -> Int -> SIO Int
  hGetBufNonBlocking, -- :: Handle -> Ptr a -> Int -> SIO Int
  hGetBuffering, -- :: Handle -> SIO BufferMode
  hGetChar, -- :: Handle -> SIO Char
  hGetEcho, -- :: Handle -> SIO Bool
  hGetLine, -- :: Handle -> SIO String
  hGetPosn, -- :: Handle -> SIO HandlePosn
  hIsClosed, -- :: Handle -> SIO Bool
  hIsEOF, -- :: Handle -> SIO Bool
  hIsOpen, -- :: Handle -> SIO Bool
  hIsReadable, -- :: Handle -> SIO Bool
  hIsSeekable, -- :: Handle -> SIO Bool
  hIsTerminalDevice, -- :: Handle -> SIO Bool
  hIsWritable, -- :: Handle -> SIO Bool
  hLookAhead, -- :: Handle -> SIO Char
  hPutBuf, -- :: Handle -> Ptr a -> Int -> SIO ()
  hPutBufNonBlocking, -- :: Handle -> Ptr a -> Int -> SIO Int
  hPutChar, -- :: Handle -> Char -> SIO ()
  hPutStr, -- :: Handle -> String -> SIO ()
  hSeek, -- :: Handle -> SeekMode -> Integer -> SIO ()
  hSetBinaryMode, -- :: Handle -> Bool -> SIO ()
  hSetBuffering, -- :: Handle -> BufferMode -> SIO ()
  hSetEcho, -- :: Handle -> Bool -> SIO ()
  hSetFileSize, -- :: Handle -> Integer -> SIO ()
  hSetPosn, -- :: HandlePosn -> SIO ()
  hShow, -- :: Handle -> SIO String
  hTell, -- :: Handle -> SIO Integer
  hWaitForInput, -- :: Handle -> Int -> SIO Bool
  isEOF, -- :: SIO Bool
  openBinaryFile, -- :: FilePath -> IOMode -> SIO Handle
  openFile, -- :: FilePath -> IOMode -> SIO Handle
  stderr, -- :: Handle
  stdin, -- :: Handle
  stdout -- :: Handle
)
where

import Prelude (Bool, Int, Integer, String, Char, FilePath, Read, Show, (.))
import Control.DeepSeq (NFData(..))
import System.IO (IOMode, Handle, BufferMode, HandlePosn, SeekMode)
import GHC.Ptr (Ptr)
import System.IO.Strict.Internals (SIO, wrap0, wrap0', wrap1, wrap1', wrap2, wrap3, run, return')
import qualified System.IO.Strict.Internals as SIO
import qualified System.IO as IO

-- | Note that 'getContents' is stricter than its counterpart in "System.IO".
getContents :: SIO String
getContents = wrap0' IO.getContents

-- | Note that 'hGetContents' is stricter than its counterpart in "System.IO".
hGetContents :: Handle -> SIO String
hGetContents = wrap1' IO.hGetContents

-- | Note that 'withBinaryFile' is stricter than its counterpart in "System.IO".
withBinaryFile :: NFData sr => FilePath -> IOMode -> (Handle -> SIO sr) -> SIO sr
withBinaryFile fp mode kont = wrap0' (IO.withBinaryFile fp mode (SIO.run . kont))

-- | Note that 'withFile' is stricter than its counterpart in "System.IO".
withFile :: NFData sr => FilePath -> IOMode -> (Handle -> SIO sr) -> SIO sr
withFile fp mode kont = wrap0' (IO.withFile fp mode (SIO.run . kont))

-- | Note that 'fix' is stricter than its counterpart in "System.IO".
fix :: NFData sa => (sa -> SIO sa) -> SIO sa
fix kont = wrap0' (IO.fixIO (SIO.run . kont))

-- | Note that 'readFile' is stricter than its counterpart in "System.IO".
readFile :: FilePath -> SIO String
readFile = wrap1' IO.readFile

-- | Note that 'read' is stricter than its counterpart in "System.IO".
read :: (NFData sa, Read sa) => String -> SIO sa
read = wrap1' IO.readIO

-- | Note that 'readLn' is stricter than its counterpart in "System.IO".
readLn :: (NFData sa, Read sa) => SIO sa
readLn = wrap0' IO.readLn

getChar :: SIO Char
getChar = wrap0 IO.getChar

appendFile :: FilePath -> String -> SIO ()
appendFile = wrap2 IO.appendFile

getLine :: SIO String
getLine = wrap0 IO.getLine
hPrint :: (Show a) => Handle -> a -> SIO ()
hPrint = wrap2 IO.hPrint
hPutStrLn :: Handle -> String -> SIO ()
hPutStrLn = wrap2 IO.hPutStrLn
hReady :: Handle -> SIO Bool
hReady = wrap1 IO.hReady
interact :: (String -> String) -> SIO ()
interact = wrap1 IO.interact
openBinaryTempFile :: FilePath -> String -> SIO (FilePath, Handle)
openBinaryTempFile = wrap2 IO.openBinaryTempFile
openTempFile :: FilePath -> String -> SIO (FilePath, Handle)
openTempFile = wrap2 IO.openTempFile
print :: (Show a) => a -> SIO ()
print = wrap1 IO.print
putChar :: Char -> SIO ()
putChar = wrap1 IO.putChar
putStr :: String -> SIO ()
putStr = wrap1 IO.putStr
putStrLn :: String -> SIO ()
putStrLn = wrap1 IO.putStrLn
writeFile :: FilePath -> String -> SIO ()
writeFile = wrap2 IO.writeFile
hClose :: Handle -> SIO ()
hClose = wrap1 IO.hClose
hFileSize :: Handle -> SIO Integer
hFileSize = wrap1 IO.hFileSize
hFlush :: Handle -> SIO ()
hFlush = wrap1 IO.hFlush
hGetBuf :: Handle -> Ptr a -> Int -> SIO Int
hGetBuf = wrap3 IO.hGetBuf
hGetBufNonBlocking :: Handle -> Ptr a -> Int -> SIO Int
hGetBufNonBlocking = wrap3 IO.hGetBufNonBlocking
hGetBuffering :: Handle -> SIO BufferMode
hGetBuffering = wrap1 IO.hGetBuffering
hGetChar :: Handle -> SIO Char
hGetChar = wrap1 IO.hGetChar
hGetEcho :: Handle -> SIO Bool
hGetEcho = wrap1 IO.hGetEcho
hGetLine :: Handle -> SIO String
hGetLine = wrap1 IO.hGetLine
hGetPosn :: Handle -> SIO HandlePosn
hGetPosn = wrap1 IO.hGetPosn 
hIsClosed :: Handle -> SIO Bool
hIsClosed = wrap1 IO.hIsClosed
hIsEOF :: Handle -> SIO Bool
hIsEOF = wrap1 IO.hIsEOF
hIsOpen :: Handle -> SIO Bool
hIsOpen = wrap1 IO.hIsOpen
hIsReadable :: Handle -> SIO Bool
hIsReadable = wrap1 IO.hIsReadable
hIsSeekable :: Handle -> SIO Bool
hIsSeekable = wrap1 IO.hIsSeekable 
hIsTerminalDevice :: Handle -> SIO Bool
hIsTerminalDevice = wrap1 IO.hIsTerminalDevice
hIsWritable :: Handle -> SIO Bool
hIsWritable = wrap1 IO.hIsWritable 
hLookAhead :: Handle -> SIO Char
hLookAhead = wrap1 IO.hLookAhead
hPutBuf :: Handle -> Ptr a -> Int -> SIO ()
hPutBuf = wrap3 IO.hPutBuf
hPutBufNonBlocking :: Handle -> Ptr a -> Int -> SIO Int
hPutBufNonBlocking = wrap3 IO.hPutBufNonBlocking
hPutChar :: Handle -> Char -> SIO ()
hPutChar = wrap2 IO.hPutChar
hPutStr :: Handle -> String -> SIO ()
hPutStr = wrap2 IO.hPutStr
hSeek :: Handle -> SeekMode -> Integer -> SIO ()
hSeek = wrap3 IO.hSeek
hSetBinaryMode :: Handle -> Bool -> SIO ()
hSetBinaryMode = wrap2 IO.hSetBinaryMode
hSetBuffering :: Handle -> BufferMode -> SIO ()
hSetBuffering = wrap2 IO.hSetBuffering
hSetEcho :: Handle -> Bool -> SIO ()
hSetEcho = wrap2 IO.hSetEcho
hSetFileSize :: Handle -> Integer -> SIO ()
hSetFileSize = wrap2 IO.hSetFileSize
hSetPosn :: HandlePosn -> SIO ()
hSetPosn = wrap1 IO.hSetPosn 
hShow :: Handle -> SIO String
hShow = wrap1 IO.hShow
hTell :: Handle -> SIO Integer
hTell = wrap1 IO.hTell
hWaitForInput :: Handle -> Int -> SIO Bool
hWaitForInput = wrap2 IO.hWaitForInput
isEOF :: SIO Bool
isEOF = wrap0 IO.isEOF
openBinaryFile :: FilePath -> IOMode -> SIO Handle
openBinaryFile = wrap2 IO.openBinaryFile
openFile :: FilePath -> IOMode -> SIO Handle
openFile = wrap2 IO.openFile
stderr :: Handle
stderr = IO.stderr
stdin :: Handle
stdin = IO.stdin
stdout :: Handle
stdout = IO.stdout
