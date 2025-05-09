module Main where

import Control.Monad
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Text.Printf

import qualified Data.ByteString as B
import Graphics.GD

data Options = Options { optVerbose :: Bool
                       , optSPS :: Int
                       , optFPS :: Int
                       , optMaxV :: Float
                       , optWsize :: Int
                       , optBrushsize :: Int
                       , optFontFile :: String
                       , optTapeMode :: Bool
                       } deriving Show
defaultOpts = Options  { optVerbose = False
                       , optSPS = 150
                       , optFPS = 25
                       , optMaxV = 2.0
                       , optWsize = 3
                       , optBrushsize = 2
                       , optFontFile =
                         "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
                       , optTapeMode = False
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
  , Option ['m'] ["maxamp"]
      (ReqArg (\d opts -> opts { optMaxV = (read d) }) "mV")
      "max amplitude in mV"
  , Option ['w'] ["window"]
      (ReqArg (\d opts -> opts { optWsize = (read d) }) "Sec")
      "Size of window in seconds"
  , Option ['b'] ["brushsize"]
      (ReqArg (\d opts -> opts { optBrushsize = (read d) }) "PX")
      "Thickness of the line in pixels"
  , Option ['f'] ["font"]
      (ReqArg (\d opts -> opts { optFontFile = d }) "Path")
      "Path to the TTF file"
  , Option ['t'] ["tape"]
      (NoArg (\opts -> opts { optTapeMode = True }))
      "Path to the TTF file"
  ]

parseargs :: String -> [String] -> Options
parseargs progname argv =
  case getOpt Permute options argv of
    (o, [], [])  -> foldl (flip id) defaultOpts o
    (_, _, errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: " ++ progname ++ " [OPTION...]"

-- ------------------------------------------------------------------

data Sample = Sample { sTime :: Float
                     , sVal  :: Float
                     , sQrs  :: Bool
                     , sAnom :: Bool
                     } deriving Show

frames :: Int -> Int -> String -> [[(Int, Sample)]]
frames wsize shift input =
  go $ zip [0..] (lead ++ (map (parse . words) (lines input)))
  where
  lead = [Sample { sTime = 0.0
                 , sVal = if t > (wsize - 60) && t < (wsize - 30)  -- TODO
                            then 1.0 else 0.0
                 , sQrs = False
                 , sAnom = False} | t <- [0..wsize]]
  parse = (\[a, b, c, d] -> Sample { sTime = read a :: Float
                                   , sVal = read b :: Float
                                   , sQrs = c /= "0"
                                   , sAnom = d /= "0"
                                   })
  go [] = []
  go xs = take wsize xs : go (drop shift xs)

drawFrame :: Options -> Image -> Image -> [(Int, Sample)] -> IO ()
drawFrame opts canvas brush frame =
  withImage (copyImage canvas) $ \img -> do
    (width, height) <- imageSize img
    let
      (zframe, cursor) = if optTapeMode opts
                 then (zip [0..] frame, width - 1)
                 else (zip [0..] $ drop n frame ++ take n frame, width - n)
        where
        n = width - (fst (head frame) `mod` width)
      yscale = fromIntegral (height `div` 2) / optMaxV opts
      ypos y = (height `div` 2) - round (y * yscale)
      pairs [_] = []
      pairs (a:b:xs) = (a, b):pairs (b:xs)
      hasqrs n = any (sQrs . snd) $ drop (width - n) frame
      qrstms = map (sTime . snd) . filter (sQrs . snd) $ frame
      avbpm [] = "&#x26a0;"  -- "warning sign"; can use 2661 "white heart"
      avbpm [_] = show $ round (60.0 / fromIntegral (width `div` optSPS opts))
      avbpm (x:y:xs) = show $ round (60.0 / go (y - x) (y:xs))
        where
        go av [x] = av
        go av (a:b:xs) = go (((b - a) * 0.5) + (av * 0.5)) (b:xs)
      qrses = map fst . filter (sQrs . snd . snd) $ zframe
      lbl sec = printf "%02d:%02d" (mm :: Int) (ss :: Int)
        where
        (hm, ss) = (round sec) `divMod` 60
        (dh, mm) = hm `divMod` 60
        (dd, hh) = hm `divMod` 24  -- We don't display hours now, but we may
      lbls = [(n, lbl (sTime s)) | (n, (m, s)) <- zframe,
                                   m `mod` (optSPS opts) == 0]
    setBrush img brush
    drawLine (cursor, 0) (cursor, height) (rgb 127 127 127) img
    mapM_ (\(n, lb) -> drawString (optFontFile opts) 12.0 0.0 (n, height - 10)
                                  lb (rgb 255 255 255) img) lbls
    drawString (optFontFile opts) 20.0 0.0 (width - 100, 30)
               (avbpm qrstms) (rgb 255 255 255) img
    when (hasqrs (optSPS opts `div` 5))
         $ fmap (const ())
         $ drawString (optFontFile opts) 24.0 0.0 (width - 30, 30)
                      "&#x2665;" (rgb 255 0 0) img
    mapM_ (\(p1, p2) -> drawLine p1 p2 (unPCREOption brushed) img)
          $ pairs $ map (\(n, (_, s)) -> (n, ypos (sVal s))) zframe
    mapM_ (\x -> setPixel (x, 3) (rgb 255 255 255) img) qrses
    savePngByteString img >>= B.putStr

-- --------------------------------------------------------------------

main = do
  progname <- getProgName
  opts <- fmap (parseargs progname) getArgs
  let
    wsize = optWsize opts * optSPS opts  -- one pixel per sample
    (shift, rem) = optSPS opts `divMod` optFPS opts
    gridstep = optSPS opts `div` 25  -- 25 small sqares per second
    -- gridstep is 1/25 sec horizontally and 0.1 mV vertically
    hsize = 2 * round (optMaxV opts * 10.0 * fromIntegral gridstep)
    grcol x = if x `mod` 5 == 0 then rgb 127 127 127 else rgb 63 63 63
  when (rem /= 0) $ return $ error "sps must be multiple of fps"
  canvas <- newImage (wsize, hsize)
  mapM_ (\x -> drawLine (x, 0) (x, hsize) (grcol x) canvas) [0,gridstep..wsize]
  mapM_ (\y -> drawLine (0, y) (wsize, y) (grcol y) canvas) [0,gridstep..wsize]
  brush <- newImage (optBrushsize opts, optBrushsize opts)
  fillImage (rgb 0 255 0) brush
  getContents >>= mapM_ (drawFrame opts canvas brush) . frames wsize shift
