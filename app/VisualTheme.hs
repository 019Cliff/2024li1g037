module VisualTheme
  ( themeCanvas,
    themePanel,
    themePanelRaised,
    themeBorder,
    themeBorderActive,
    themeDivider,
    themeText,
    themeTextSecondary,
    themeTextDisabled,
    themeAccent,
    themeSuccess,
    themeWarning,
    themeError,
    themeInfo,
    themeSelection,
    themeLifeHigh,
    themeLifeMedium,
    themeLifeCritical,
    themeElementFire,
    themeElementIce,
    themeElementResin,
    themeElementFear,
    themeElementPoison,
    themeElementElectric,
    themeTerrainGrass,
    themeTerrainPath,
    themeTerrainWater,
    themeTerrainAsphalt,
  )
where

import Graphics.Gloss

themeCanvas, themePanel, themePanelRaised :: Color
themeCanvas = makeColorI 18 28 24 255
themePanel = makeColorI 21 27 24 232
themePanelRaised = makeColorI 32 42 35 242

themeBorder, themeBorderActive, themeDivider :: Color
themeBorder = makeColorI 107 128 107 255
themeBorderActive = makeColorI 226 194 95 255
themeDivider = makeColorI 83 100 83 255

themeText, themeTextSecondary, themeTextDisabled :: Color
themeText = makeColorI 229 233 223 255
themeTextSecondary = makeColorI 170 181 163 255
themeTextDisabled = makeColorI 125 135 123 255

themeAccent, themeSuccess, themeWarning, themeError, themeInfo :: Color
themeAccent = makeColorI 226 194 95 255
themeSuccess = makeColorI 143 193 118 255
themeWarning = makeColorI 240 195 90 255
themeError = makeColorI 230 106 94 255
themeInfo = makeColorI 121 175 198 255

themeSelection :: Color
themeSelection = makeColorI 67 80 48 242

themeLifeHigh, themeLifeMedium, themeLifeCritical :: Color
themeLifeHigh = themeSuccess
themeLifeMedium = themeWarning
themeLifeCritical = themeError

themeElementFire, themeElementIce, themeElementResin :: Color
themeElementFire = makeColorI 175 88 58 255
themeElementIce = makeColorI 111 150 168 255
themeElementResin = makeColorI 145 107 61 255

themeElementFear, themeElementPoison, themeElementElectric :: Color
themeElementFear = makeColorI 170 132 210 255
themeElementPoison = makeColorI 101 168 92 255
themeElementElectric = themeAccent

themeTerrainGrass, themeTerrainPath, themeTerrainWater, themeTerrainAsphalt :: Color
themeTerrainGrass = makeColorI 70 94 56 255
themeTerrainPath = makeColorI 95 73 49 255
themeTerrainWater = makeColorI 67 112 133 255
themeTerrainAsphalt = makeColorI 48 52 50 255
