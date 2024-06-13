module Main where

import Control.Monad
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt

import qualified Data.ByteString as B
import Graphics.GD

data Options = Options { optVerbose :: Bool
                       , optSPS :: Int
                       , optFPS :: Int
                       , optBase :: Float
                       , optMaxV :: Float
                       , optWsize :: Int
                       , optHeight :: Int
                       , optBrushsize :: Int
                       } deriving Show
defaultOptions = Options { optVerbose = False
                         , optSPS = 150
                         , optFPS = 25
                         , optBase = 1.65
                         , optMaxV = 0.6
                         , optWsize = 3
                         , optHeight = 300
                         , optBrushsize = 2
                         }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v'] ["verbose"]
      (NoArg (\opts -> opts { optVerbose = True }))
      "Verbose output"
  , Option ['s'] ["samplerate"]
      (ReqArg (\d opts -> opts { optSPS = (read d) }) "SPS")
      "Sample rate on input"
  , Option ['r'] ["framerate"]
      (ReqArg (\d opts -> opts { optFPS = (read d) }) "FPS")
      "Frame rate on output"
  , Option ['b'] ["baseline"]
      (ReqArg (\d opts -> opts { optBase = (read d) }) "mV")
      "base line in mV"
  , Option ['m'] ["maxamp"]
      (ReqArg (\d opts -> opts { optMaxV = (read d) }) "mV")
      "max amplitude in mV"
  , Option ['w'] ["window"]
      (ReqArg (\d opts -> opts { optWsize = (read d) }) "Sec")
      "Size of window in seconds"
  , Option ['h'] ["hight"]
      (ReqArg (\d opts -> opts { optHeight = (read d) }) "PX")
      "Hight of window in pixels"
  , Option ['b'] ["brushsize"]
      (ReqArg (\d opts -> opts { optBrushsize = (read d) }) "PX")
      "Thickness of the line in pixels"
  ]

parseargs :: String -> [String] -> Options
parseargs progname argv =
  case getOpt Permute options argv of
    (o, [], [])  -> foldl (flip id) defaultOptions o
    (_, _, errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: " ++ progname ++ " [OPTION...]"

frames :: Int -> Int -> String -> [[(Float, Float)]]
frames wsize shift input = go $ map parse . lines $ input
  where
  parse = (\[a, b, _, _] -> (read a :: Float, read b :: Float)) . words
  go [] = []
  go xs = take wsize xs : go (drop shift xs)

drawFrame :: Image -> Image -> (Float, Float) -> [(Float, Float)] -> IO ()
drawFrame canvas brush (base, maxamp) frame =
  withImage (copyImage canvas) $ \img -> do
    (_, height) <- imageSize img
    let
      yscale = fromIntegral (height `div` 2) / maxamp
      ypos y = (height `div` 2) - round ((y - base) * yscale)
      pairs [_] = []
      pairs (a:b:xs) = (a, b):pairs (b:xs)
    setBrush img brush
    mapM_ (\(p1, p2) -> drawLine p1 p2 (unPCREOption brushed) img) $ pairs $ zip [0..] (map (ypos . snd) frame)
    savePngByteString img >>= B.putStr

main = do
  progname <- getProgName
  opts <- fmap (parseargs progname) getArgs
  let wsize = optWsize opts * optSPS opts
  let (shift, rem) = optSPS opts `divMod` optFPS opts
  print shift
  when (rem /= 0) $ return $ error "sps must be multiple of fps"
  let norm = (optBase opts, optMaxV opts)
  canvas <- newImage (wsize, optHeight opts)
  brush <- newImage (optBrushsize opts, optBrushsize opts)
  fillImage (rgb 0 255 0) brush
  getContents >>= mapM_ (drawFrame canvas brush norm) . frames wsize shift
