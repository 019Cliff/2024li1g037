module Desenhar where

import Graphics.Gloss
import Graphics.Gloss.Juicy
import LI12425
import ImmutableTowers
import GHC.Float

-- | Função principal de desenho, varia conforme o modo de jogo
desenha :: ImmutableTowers -> IO Picture
desenha e@(ImmutableTowers jogo imgs modo _ _) = case modo of
  MenuInicial opcao -> return $ desenhaMenu imgs opcao
  Pausado -> return $ Translate (-200) 0 $ Scale 0.3 0.3 $ Color white $ Text "Jogo Pausado"
  EmJogo -> return $ Pictures
    [ mapaToPicture imgs (mapaJogo jogo)
    , desenhaTorres e
    , desenhaInimigos e
    , desenhaPortal e (head (portaisJogo jogo))
    , desenhaBase e (baseJogo jogo)
    ]
  TutorialFoto -> return $ getImagem ImagemTutorial imgs
  MostrarCreditos -> return $ getImagem ImagemCreditos imgs

-- | Desenha o menu inicial
desenhaMenu :: Imagens -> MenuInicialOpcoes -> Picture
desenhaMenu imgs opcaoSel = Pictures
  [ getImagem Fundo imgs
  , botao Play (-1000) opcaoSel Jogar
  , botao ButaoCreditos 0 opcaoSel Creditos
  , botao Exit 1000 opcaoSel Sair
  ]
  where
    botao img x sel alvo =
      let s = if sel == alvo then 0.3 else 0.2
          y = if sel == alvo then -950 else -1400
      in Scale s s (Translate x y (getImagem img imgs))

-- | Desenha todas as torres com o seu alcance
desenhaTorres :: ImmutableTowers -> Picture
desenhaTorres e =
  Pictures $ concatMap (\torre ->
    [ desenhaAlcanceTorre torre
    , desenhaTorre e torre
    ]) (torresJogo (jogo e))

desenhaTorre :: ImmutableTowers -> Torre -> Picture
desenhaTorre e torre =
  let (x, y) = posicaoTorre torre
      img = case projetilTorre torre of
        Projetil Resina _ -> TorreResina
        Projetil Gelo _   -> TorreGelo
        Projetil Fogo _   -> TorreFogo
  in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (scale 0.6 0.6 (getImagem img (imagens e)))

desenhaAlcanceTorre :: Torre -> Picture
desenhaAlcanceTorre torre =
  let (x, y) = posicaoTorre torre
      raio = alcanceTorre torre * calculaTamanhoBloco mapa01
  in Color (withAlpha 0.3 blue) $ Translate (converteX (realToFrac x)) (converteY (realToFrac y)) $ Circle raio

-- | Desenha todos os inimigos
desenhaInimigos :: ImmutableTowers -> Picture
desenhaInimigos e =
  Pictures $ map (desenhaInimigo e) (inimigosJogo (jogo e))

desenhaInimigo :: ImmutableTowers -> Inimigo -> Picture
desenhaInimigo e inimigo =
  let (x, y) = posicaoInimigo inimigo
  in Translate (converteX (realToFrac x))
 (converteY (realToFrac y)) (scale 0.3 0.3 (getImagem Inimigocima (imagens e)))

-- | Desenha o portal
desenhaPortal :: ImmutableTowers -> Portal -> Picture
desenhaPortal e portal =
  let (x, y) = posicaoPortal portal
  in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (scale 0.6 0.6 (getImagem PortalFoto (imagens e)))

-- | Desenha a base
desenhaBase :: ImmutableTowers -> Base -> Picture
desenhaBase e base =
  let (x, y) = posicaoBase base
  in Translate (converteX (realToFrac x)) (converteY (realToFrac y)) (scale 0.3 0.3 (getImagem BaseFoto (imagens e)))

-- | Converte um bloco de terreno em imagem
blocoToPicture :: Imagens -> Terreno -> Picture
blocoToPicture imgs t = case t of
  Relva -> getImagem Grass imgs
  Agua  -> getImagem Water imgs
  Terra -> getImagem Land imgs

-- | Desenha o mapa completo
mapaToPicture :: Imagens -> Mapa -> Picture
mapaToPicture imgs terreno =
  let bloco = calculaTamanhoBloco terreno
      largura = fromIntegral (length (head terreno))
      altura  = fromIntegral (length terreno)
      meioX = largura / 2
      meioY = altura / 2
  in Pictures [ Translate (x' * bloco + bloco / 2) (y' * bloco + bloco / 2)
              $ blocoToPicture imgs b
              | (y, linha) <- zip [0..] (reverse terreno)
              , (x, b) <- zip [0..] linha
              , let x' = x - meioX
              , let y' = y - meioY ]

-- | Calcula o tamanho de cada bloco
calculaTamanhoBloco :: Mapa -> Float
calculaTamanhoBloco terreno =
  let largura = fromIntegral (length (head terreno))
      altura  = fromIntegral (length terreno)
  in min (larguraJanela / largura) (alturaJanela / altura)

-- | Converte posição X para coordenada Gloss
converteX :: Double -> Float
converteX x = (-larguraJanela / 2) + (double2Float x * int2Float pixeis)

-- | Converte posição Y para coordenada Gloss
converteY :: Double -> Float
converteY y = (alturaJanela / 2) - (double2Float y * int2Float pixeis)

-- | Função que carrega as imagens do jogo
carregarImagens :: IO ImmutableTowers
carregarImagens = do
  fundo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/fundo_1_.bmp"
  grass <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/images.bmp"
  water <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/6da00a37f26551f688dcc04367d7c73c_1.bmp"
  land <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/terra_textura.bmp"
  torreResina <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  torreGelo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  torreFogo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  inimigo <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/enemy-clipart-little-monster-holding-a-gun-in-one-hand_546721_wh860_2_-removebg-preview.bmp"
  base <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/tower_image-removebg-preview.bmp"
  portal <- loadJuicy "/home/cliff/Desktop/2024li1g037/app/imagens/portal.bmp"

  let imgs = [ (Fundo, fundo), (Grass, grass), (Water, water), (Land, land), (TorreResina, torreResina), (TorreGelo, torreGelo), (TorreFogo, torreFogo), (Inimigocima, inimigo), (BaseFoto, base), (PortalFoto, portal)
             
             ]
      jogoInicial = Jogo
        { mapaJogo = mapa01
        , baseJogo = base01
        , portaisJogo = [portal01]
        , torresJogo = []
        , inimigosJogo = []
        , lojaJogo =
          [ (50, Torre (0,0) 20 4 2 2 5 (Projetil Resina (Finita 3)))
          , (50, Torre (0,0) 20 4 2 2 5 (Projetil Gelo   (Finita 2)))
          , (50, Torre (0,0) 20 4 2 2 5 (Projetil Fogo   (Finita 1)))
          ]
        }
  return $ ImmutableTowers jogoInicial imgs (MenuInicial Jogar) 0 Nothing
