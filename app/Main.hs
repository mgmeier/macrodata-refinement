module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
    setSGR (heightStyle 0)
    initRenderer fps frameBuffer
    createFrames size fps frameBuffer
  where
    fps        = 16
    bufferSize = head $ tail $ dropWhile (< xSize * ySize) $ iterate (* 2) 4096

data Counters = Counters
      { xPos :: {-# UNPACK #-} !Double
      , yPos :: {-# UNPACK #-} !Double
      }

newtype Height = Height Text

createFrames :: (Int, Int) -> Int -> MVar Text -> IO ()
createFrames res@(ySize, xSize) _fps frameBuffer = do
  source <- T.pack <$> replicateM (xSize * ySize)
              ((['0' .. '9'] !!) <$> randomRIO (0, 9))
  let
    frameLines = T.chunksOf xSize source

    go counters@Counters{..} = do
      let
        x     = round (xMax + (xMax * sin xPos))
        hs    = [(x, height 1), (10, height 2), (10, height 3), (10, height 2), (10, height 1), (10, height 0)]
        frame = T.concat $ map (insertHeights hs) frameLines
      frame `deepseq` putMVar frameBuffer frame
      go $ stepCounters step1 counters

  writeFile "debug.log" (show (T.length $ head frameLines) ++ "x" ++ show (length frameLines) ++ "\n" ++ show res ++ "\n")
  go startCounters
  where
    step1  = 0.8 / fromIntegral _fps
    height = Height . T.pack . setSGRCode . heightStyle

    xLen   = 50 -- xSize `div` 10
    xMax   = fromIntegral (xSize - xLen) / 2
    -- yLen   = fromIntegral ySize / 20


startCounters :: Counters
startCounters = Counters 1.0 1.0

{-# INLINE stepCounters #-}
stepCounters :: Double -> Counters -> Counters
stepCounters s1 Counters{..} =
  Counters (step s1 xPos) yPos
  where
    phase    = 2 * pi
    step s c = let c' = c - s in if c' < 0.0 then c' + phase else c'

insertHeights :: [(Int, Height)] -> Text -> Text
insertHeights [] t = t
insertHeights ((ofs, Height h):rest) t =
  let (pref, suff) = T.splitAt ofs t
  in pref <> h <> insertHeights rest suff


initRenderer :: Int -> MVar Text -> IO ()
initRenderer fps frameBuffer =
  void $ forkIO $ forever $ do
    out <- takeMVar frameBuffer
    T.hPutStr stdout $ resetScreen <> out
    hFlush stdout
    threadDelay delay
  where
    resetScreen = T.pack $ clearScreenCode ++ setCursorPositionCode 0 0
    delay       = 1000 * (1000 `div` fps)

heightStyle :: Int -> [SGR]
heightStyle 0 = [SetConsoleIntensity FaintIntensity,  SetColor Foreground Dull Cyan]
heightStyle 1 = [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull Cyan]
heightStyle 2 = [SetConsoleIntensity NormalIntensity, SetColor Foreground Vivid Cyan]
heightStyle 3 = [SetConsoleIntensity BoldIntensity,   SetColor Foreground Vivid Cyan]
heightStyle n = error $ "heightStyle: " ++ show n ++ "; expected [0-3]"
