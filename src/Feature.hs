module Feature where

class IntPair a where
  square :: Int -> a

newtype Point = Point { pointToPair :: (Int, Int) } deriving (Show, Eq)
pointX :: Point -> Int
pointX (Point (x, _)) = x
pointY :: Point -> Int
pointY (Point (_, y)) = y

newtype Delta = Delta { deltaToPair :: (Int, Int) } deriving (Show, Eq)
deltaX :: Delta -> Int
deltaX (Delta (x, _)) = x
deltaY :: Delta -> Int
deltaY (Delta (_, y)) = y
squareDelta :: Int -> Delta
squareDelta n = Delta (n, n)

instance IntPair Delta where
  square n = Delta (n, n)

newtype Size = Size { sizeToPair :: (Int, Int) } deriving (Show, Eq)
sizeX :: Size -> Int
sizeX (Size (x, _)) = x
sizeY :: Size -> Int
sizeY (Size (_, y)) = y
squareSize :: Int -> Size
squareSize n = Size (n, n)

instance IntPair Size where
  square n = Size (n, n)

newtype Slice = Slice { sliceToPair :: (Point, Point) } deriving (Show, Eq)
topLeft :: Slice -> Point
topLeft (Slice (tl, _)) = tl
bottomRight :: Slice -> Point
bottomRight (Slice (_, br)) = br

sliceOfPoint :: Delta -> Point -> Slice
sliceOfPoint (Delta (dx, dy)) (Point (x, y)) =
  Slice (Point (x - dx, y - dy), Point (x + dx, y + dy))

data Feature = Feature
  { featureCoords  :: Point
  , featureContext :: Delta
  } deriving (Show, Eq)

sliceOfFeature :: Feature -> Slice
sliceOfFeature (Feature coords context) = sliceOfPoint context coords

featureAssoc :: Size -> Delta -> Feature -> [Feature]
featureAssoc imageSize maxOrthoDisp leftFeature = do
  let xMinBound = max (x - featureDx - spaceDx) 0
      xMaxBound = min (x + featureDx + spaceDx) (width - 1)
      yMinBound = max (y - featureDy - spaceDy) 0
      yMaxBound = min (y + featureDy + spaceDy) (height - 1)
  y <- [yMinBound + featureDy..yMaxBound - featureDy]
  x <- [xMinBound + featureDx..xMaxBound - featureDx]
  return $ Feature
    { featureCoords  = Point (x, y)
    , featureContext = featureContext leftFeature
    }
  where (x, y) = pointToPair (featureCoords leftFeature)
        (featureDx, featureDy) = deltaToPair (featureContext leftFeature)
        (spaceDx, spaceDy) = deltaToPair maxOrthoDisp
        (width, height) = sizeToPair imageSize

features :: Size -> Delta -> [Feature]
features imageSize featureContext = do
  y <- [featureDy..height - featureDy - 1]
  x <- [featureDx..width - featureDx - 1]
  return $ Feature
    { featureCoords  = Point (x, y)
    , featureContext = featureContext
    }
  where (width, height) = sizeToPair imageSize
        (featureDx, featureDy) = deltaToPair featureContext
