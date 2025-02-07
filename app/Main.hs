module Main where

import Control.Concurrent
import Control.DeepSeq (deepseq)
import Control.Exception
import Control.Monad
import System.Console.ANSI
import System.IO

import System.Random (randomRIO)


main :: IO ()
main = do
  ansiSupport <- hNowSupportsANSI stdout
  unless ansiSupport quit
  getTerminalSize >>= maybe quit runMDR
  
  where
    quit = error "ANSI error"

runMDR :: (Int, Int) -> IO ()
runMDR size@(xSize, ySize) = do
  buffering <- hGetBuffering stdout
  hHideCursor stdout
  hSetBuffering stdout (BlockBuffering $ Just bufferSize)

  let
    restoreStdout = hSetBuffering stdout buffering >> hShowCursor stdout

  bracket_ useAlternateScreenBuffer (useNormalScreenBuffer >> restoreStdout) $ do
    frameBuffer <- newEmptyMVar
    setSGR [SetConsoleIntensity FaintIntensity, SetColor Foreground Dull Cyan]
    initRenderer fps frameBuffer
    createFrames size fps frameBuffer
  where
    fps        = 2
    bufferSize = head $ tail $ dropWhile (< xSize * ySize) $ iterate (* 2) 4096

createFrames :: (Int, Int) -> Int -> MVar String -> IO ()
createFrames (xSize, ySize) _fps frameBuffer = forever $ do
  buf <- replicateM (xSize * ySize) $ (['0' .. '9'] !!) <$> randomRIO (0, 9)
  buf `deepseq` putMVar frameBuffer buf

initRenderer :: Int -> MVar String -> IO ()
initRenderer fps frameBuffer =
  void $ forkIO $ forever $ do
    out <- takeMVar frameBuffer
    hPutStr stdout $ resetScreen <> out
    hFlush stdout
    threadDelay delay
  where
    resetScreen = clearScreenCode <> setCursorPositionCode 0 0
    delay       = 1000 * (1000 `div` fps)
