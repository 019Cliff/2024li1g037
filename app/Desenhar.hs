module Desenhar where

import Graphics.Gloss
import ImmutableTowers

desenha :: ImmutableTowers -> Picture
desenha _ = Translate (-450) 0 $ Scale 0.5 0.5 $ Text "Welcome to Immutable Towers!"
