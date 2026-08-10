module UIComponents
  ( UIRect (..),
    ButtonTone (..),
    containsPoint,
    drawButton,
    drawPanel,
    drawAccentBar,
  )
where

import Graphics.Gloss
import UIText (drawUITextCentered)
import VisualTheme

data UIRect = UIRect
  { rectX :: !Float,
    rectY :: !Float,
    rectW :: !Float,
    rectH :: !Float
  }

data ButtonTone = Primary | Neutral | Danger | Disabled
  deriving (Eq)

containsPoint :: (Float, Float) -> UIRect -> Bool
containsPoint (mx, my) (UIRect x y w h) =
  mx >= x - w / 2 && mx <= x + w / 2 && my >= y - h / 2 && my <= y + h / 2

drawPanel :: UIRect -> Picture
drawPanel (UIRect x y w h) =
  Translate x y $
    Pictures
      [ Color (makeColorI 22 29 25 238) $ rectangleSolid w h,
        Color themeBorder $ rectangleWire w h
      ]

drawButton :: Maybe (Float, Float) -> UIRect -> ButtonTone -> String -> Picture
drawButton mouse rect@(UIRect x y w h) tone label =
  let hovered = maybe False (`containsPoint` rect) mouse && tone /= Disabled
      (fillBase, borderBase, textColor) = colors tone
      fill = if hovered then brighten fillBase else fillBase
      border = if hovered then themeBorderActive else borderBase
   in Translate x y $
        Pictures
          [ Color fill $ rectangleSolid w h,
            Color border $ rectangleWire w h,
            if hovered
              then Color (withAlpha 0.18 border) $ rectangleSolid (w - 8) (h - 8)
              else Blank,
            drawButtonLabel label w textColor
          ]

drawAccentBar :: Float -> Float -> Float -> Color -> Picture
drawAccentBar x y w cor =
  Color (withAlpha 0.35 cor) $
    Translate x y $
    rectangleSolid (w - 34) 6

colors :: ButtonTone -> (Color, Color, Color)
colors Primary = (themeSelection, themeAccent, themeText)
colors Neutral = (themePanelRaised, themeDivider, themeText)
colors Danger = (makeColorI 76 42 38 242, themeError, themeText)
colors Disabled = (makeColorI 36 38 36 210, themeTextDisabled, themeTextDisabled)

brighten :: Color -> Color
brighten baseColor =
  let (r, g, b, a) = rgbaOfColor baseColor
   in makeColor (min 1 (r + 0.08)) (min 1 (g + 0.08)) (min 1 (b + 0.08)) a

drawButtonLabel :: String -> Float -> Color -> Picture
drawButtonLabel label buttonWidth textColor =
  let glyphPixel
        | buttonWidth <= 52 = 2.6
        | buttonWidth <= 72 = 2.7
        | buttonWidth <= 96 = 2.8
        | otherwise = 3.0
   in drawUITextCentered 0 8 glyphPixel textColor label
