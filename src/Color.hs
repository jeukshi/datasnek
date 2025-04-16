module Color where

import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Printf (printf)

goldenRatio :: Double
goldenRatio = 0.618033988749895

generateColors :: Int -> [Text]
generateColors n =
    [ hslToHex (mod' (h0 + goldenRatio * fromIntegral i) 1 * 360) 0.65 0.5
    | i <- [0 .. n - 1]
    ]
  where
    h0 = 0.1 -- starting hue offset

-- Convert HSL values to hex color code
hslToHex :: Double -> Double -> Double -> Text
hslToHex h s l = T.pack $ "#" ++ rgbToHex (hslToRgb h s l)

-- Convert HSL to RGB
hslToRgb :: Double -> Double -> Double -> (Int, Int, Int)
hslToRgb h s l
    | s == 0 = (round (l' * 255), round (l' * 255), round (l' * 255))
    | otherwise = (round (r * 255), round (g * 255), round (b * 255))
  where
    l' = l
    h' = h / 60 -- Convert hue to [0, 6)
    c = (1 - abs (2 * l' - 1)) * s
    x = c * (1 - abs ((h' `mod'` 2) - 1))
    m = l' - c / 2

    (r', g', b')
        | h' >= 0 && h' < 1 = (c, x, 0)
        | h' >= 1 && h' < 2 = (x, c, 0)
        | h' >= 2 && h' < 3 = (0, c, x)
        | h' >= 3 && h' < 4 = (0, x, c)
        | h' >= 4 && h' < 5 = (x, 0, c)
        | otherwise = (c, 0, x)

    r = r' + m
    g = g' + m
    b = b' + m

    mod' a b = a - b * fromIntegral (floor (a / b))

-- Convert RGB to Hex string
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) = printf "%02x%02x%02x" r g b

-- Usage example:
colorMap :: [(Int, Text)]
colorMap = zip [0 ..] (generateColors 256)

-- Assign a color to a user based on user ID or index
assignColor :: Text -> Text
assignColor userId =
    let colorIndex = textToColorIndex userId `mod` 256
     in fromMaybe "#000000" (lookup colorIndex colorMap) -- Default black as fallback

textToColorIndex :: Text -> Int
textToColorIndex = T.foldl' (\acc c -> acc * 31 + fromEnum c) 0
