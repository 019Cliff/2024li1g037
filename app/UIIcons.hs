module UIIcons
  ( iconBase,
    iconCredits,
    iconWave,
    iconSpeed,
    iconDamage,
    iconRange,
    iconWarning,
  )
where

import Graphics.Gloss
import VisualTheme

iconBase, iconCredits, iconWave, iconSpeed, iconDamage, iconRange, iconWarning :: Float -> Color -> Picture
iconBase size colorValue =
  Color colorValue $ Pictures
    [ rectangleSolid (size * 0.58) (size * 0.48),
      Translate 0 (size * 0.32) $ Polygon [(-size * 0.38, 0), (0, size * 0.28), (size * 0.38, 0)],
      Color themeCanvas $ Translate 0 (-size * 0.04) $ rectangleSolid (size * 0.14) (size * 0.22)
    ]

iconCredits size colorValue =
  Color colorValue $ Pictures
    [ circleSolid (size * 0.34),
      Color themeCanvas $ circleSolid (size * 0.16),
      Color colorValue $ rectangleSolid (size * 0.09) (size * 0.62)
    ]

iconWave size colorValue =
  Color colorValue $ Line [(-size * 0.5, -size * 0.18), (-size * 0.24, size * 0.22), (0, -size * 0.18), (size * 0.24, size * 0.22), (size * 0.5, -size * 0.18)]

iconSpeed size colorValue =
  Color colorValue $ Polygon [(-size * 0.44, 0), (size * 0.1, size * 0.34), (size * 0.1, size * 0.12), (size * 0.46, size * 0.12), (-size * 0.1, -size * 0.36), (-size * 0.1, -size * 0.12), (-size * 0.46, -size * 0.12)]

iconDamage size colorValue =
  Color colorValue $ Polygon [(-size * 0.08, size * 0.5), (size * 0.35, size * 0.08), (size * 0.08, size * 0.08), (size * 0.08, -size * 0.5), (-size * 0.35, -size * 0.08), (-size * 0.08, -size * 0.08)]

iconRange size colorValue =
  Color colorValue $ ThickCircle (size * 0.32) (max 1.5 (size * 0.07))

iconWarning size colorValue =
  Color colorValue $ Pictures
    [ Polygon [(0, size * 0.5), (size * 0.48, -size * 0.4), (-size * 0.48, -size * 0.4)],
      Color themeCanvas $ Translate 0 (-size * 0.12) $ rectangleSolid (size * 0.08) (size * 0.34),
      Color themeCanvas $ Translate 0 (-size * 0.32) $ circleSolid (size * 0.055)
    ]
