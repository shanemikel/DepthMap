{-# LANGUAGE OverloadedStrings #-}
module Utils where

import           System.IO
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Binary.Strict.Get
import qualified Data.Binary.Strict.BitGet  as BG
import           Data.Binary.Put
import           Data.Word
import           Path
import qualified Data.Array.Repa            as R
import           Data.Array.Repa
                 ( Array
                 , DIM2
                 , Z(Z)
                 , (:.)((:.))
                 , (!)
                 )
import           Formatting
import           Numeric

type Image = Array R.U DIM2 Word8

data BitmapHeader = BitmapHeader
  { bmpLength   :: Int
  , bmpOffset   :: Int
  , bmpBitDepth :: Int
  , bmpWidth    :: Int
  , bmpHeight   :: Int
  } deriving (Show)

parseHeader :: Get BitmapHeader
parseHeader = do
  10 `replicateM` getWord8
  length <- fromIntegral <$> getWord32be
  offset <- fromIntegral <$> getWord32be
  width  <- fromIntegral <$> getWord32be
  height <- fromIntegral <$> getWord32be
  2 `replicateM` getWord8
  bitDepth <- fromIntegral <$> getWord32be
  return $ BitmapHeader length offset bitDepth width height

exceptT :: Monad m => Either e a -> ExceptT e m a
exceptT = ExceptT . return

infix 1 ?>>!
(?>>!) :: Monad m => Bool -> e -> ExceptT e m ()
(?>>!) True  err = throwE err
(?>>!) False _   = return ()

loadImage :: Handle -> ExceptT String IO Image
loadImage fileHandle = do
  info       <- liftIO $ B.hGet fileHandle 54
  parsedInfo <- exceptT . fst $ runGet parseHeader info

  B.length info /= 54 ?>>!
    "Bad file format"

  bmpOffset parsedInfo /= 40 || bmpBitDepth parsedInfo /= 8 ?>>!
    "Unsupported image format. Please specify an 8-bit (grayscale) BMP image."

  let colorTableSize = (bmpLength parsedInfo) - 54
      width          = abs (bmpWidth parsedInfo)
      height         = abs (bmpHeight parsedInfo)
      size           = width * height
  colorTable <- liftIO $ B.hGet fileHandle colorTableSize

  colorTableSize /= (B.length colorTable) ?>>!
    "Error reading image file"

  return $ R.fromListUnboxed (Z:.height:.width) (B.unpack colorTable)
  
getWidth :: Image -> Int
getWidth image = R.listOfShape (R.extent image) !! 0

getHeight :: Image -> Int
getHeight image = R.listOfShape (R.extent image) !! 1

printImage :: Image -> IO ()
printImage image =
  forM_ [0..getHeight image - 1] $ \ y -> do
    forM [0..getWidth image - 2] $ \ x ->
      fprint ((left 2 '0' %. hex) % " ") (image !(Z:.y:.x))
    fprint (left 2 '0' %. hex) (image !(Z:.y:.(getWidth image - 1)))
    putStrLn ""
