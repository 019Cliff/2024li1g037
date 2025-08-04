module Main where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3
import Eventos
import Desenhar
import Tempo

data EstadoJogo = EstadoJogo
  { mapa :: Mapa
  , moedas :: Int
  , torres :: [Torre]
  , inimigos :: [Inimigo]
  , base :: Base
  , portais :: [Portal]
  , loja :: [TorreLoja]
  , torreSelecionada :: Maybe TorreLoja 
  }

lojaInicial :: [TorreLoja]
lojaInicial =
  [ TorreLoja TorreVermelha 50 red
  , TorreLoja TorreAzul 75 blue
  , TorreLoja TorreVerde 100 green
  ]

data TorreLoja = TorreLoja
  { tipoTorre :: TipoTorre
  , precoTorre :: Int
  , corTorre :: Color
  } deriving (Eq) 
data TipoTorre = TorreVermelha | TorreAzul | TorreVerde
  deriving (Eq, Show)

data EstadoMenu = MenuPrincipal | Jogando EstadoJogo | Jogar |  Sair 

data EstadoApp = EstadoApp
  { estadoAtual :: EstadoMenu
  , imgFundo :: Picture
  , imgJogar :: Picture
  , imgRelva :: Picture
  , imgTerra :: Picture
  , imgAgua :: Picture
  , imgBase :: Picture
  , imgPortal :: Picture
  , imgInimigo :: Picture
  , imgTorreVermelha :: Picture 
  , imgTorreAzul :: Picture     
  , imgTorreVerde :: Picture    
  , dimRelva :: (Float, Float)
  , dimTerra :: (Float, Float)
  , dimAgua :: (Float, Float)
  }


estadoInicialApp :: EstadoApp
estadoInicialApp = EstadoApp
  { estadoAtual = MenuPrincipal
  , imgFundo = undefined 
  , imgJogar = undefined
  }

estadoInicialJogo :: EstadoJogo
estadoInicialJogo = EstadoJogo
  { mapa = mapag
  , moedas = creditosBase baseg
  , torres = []
  , inimigos = map (\portal -> inicializaInimigo (posicaoPortal portal)) (portais estadoInicialJogo)
  , base = baseg
  , portais = [portalg]
  , loja = lojaInicial
  , torreSelecionada = Nothing
  }

inicializaInimigo :: Posicao -> Inimigo
inicializaInimigo pos = Inimigo
  { posicaoInimigo = pos
  , direcaoInimigo = calculaDirecaoInicial mapag pos
  , velocidadeInimigo = 3.0
  , vidaInimigo = 100.0
  , ataqueInimigo = 10.0
  , butimInimigo = 50
  , projeteisInimigo = []
  }

calculaDirecaoInicial :: Mapa -> Posicao -> Direcao
calculaDirecaoInicial mapa (x, y) =
  let (ix, iy) = (round x, round y)
      vizinhos = [(Norte, (ix, iy - 1)), (Sul, (ix, iy + 1)), (Este, (ix + 1, iy)), (Oeste, (ix - 1, iy))]
      terrenoValido (_, (cx, cy)) = cx >= 0 && cy >= 0 && cy < length mapa && cx < length (head mapa) && mapa !! cy !! cx == Terra
  in case filter terrenoValido vizinhos of
       [] -> Oeste 
       (direcao, _):_ -> direcao


janelaLargura, janelaAltura :: Int
janelaLargura = round (fromIntegral mapaLargura * tamanhoBloco)
janelaAltura = round (fromIntegral mapaAltura * tamanhoBloco)

-- Número de blocos no mapa
mapaLargura, mapaAltura :: Int
mapaLargura = 40
mapaAltura = 27

tamanhoBloco :: Float
tamanhoBloco = fromIntegral (min (1920 `div` mapaLargura) (1080 `div` mapaAltura))

janela :: Display
janela = InWindow "Immutable Towers" (janelaLargura, janelaAltura) (0,0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

fundoMenu :: IO Picture
fundoMenu = loadBMP "app/fundo_1_.bmp"

iconeJogar :: IO Picture 
iconeJogar = loadBMP "app/button-game-a-cartoon-congratulations-for-your-vector-30227498-removebg-preview.bmp"
-- Função principal
main :: IO ()
main = do
  imgFundo <- fundoMenu
  imgJogar <- iconeJogar
  imgRelva <- loadBMP "app/images.bmp"
  imgTerra <- loadBMP "app/terra_textura.bmp"
  imgAgua <- loadBMP "app/6da00a37f26551f688dcc04367d7c73c_1.bmp"
  imgBase <- loadBMP "app/tower_image-removebg-preview.bmp"
  imgPortal <- loadBMP "app/portal.bmp"
  imgInimigo <- loadBMP "app/enemy-clipart-little-monster-holding-a-gun-in-one-hand_546721_wh860_2_-removebg-preview.bmp"
  imgTorreVermelha <- loadBMP "app/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  imgTorreAzul <- loadBMP "app/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  imgTorreVerde <- loadBMP "app/DALL_E-2025-01-13-13.52.34-A-simple-gray-tower-designed-for-a-tower-defense-game-removebg-preview.bmp"
  let estadoInicial = EstadoApp 
        { estadoAtual = MenuPrincipal
        , imgFundo = imgFundo
        , imgJogar = imgJogar
        , imgRelva = imgRelva
        , imgTerra = imgTerra
        , imgAgua = imgAgua
        , imgBase = imgBase
        , imgPortal = imgPortal
        , imgInimigo = imgInimigo
        , imgTorreVermelha = imgTorreVermelha
        , imgTorreAzul = imgTorreAzul
        , imgTorreVerde = imgTorreVerde
        , dimRelva = (225, 225)
        , dimTerra = (980, 980)
        , dimAgua = (1366, 768)
        }
  play janela fundo fr estadoInicial desenhaApp reageEventosApp reageTempoApp

-- Desenho da aplicação
desenhaApp :: EstadoApp -> Picture
desenhaApp app = case estadoAtual app of
  MenuPrincipal -> desenhaMenu (imgFundo app) (imgJogar app)
  Jogando jogo
    | verificaFimDeJogo jogo -> Translate (-200) 0 $ Scale 0.5 0.5 $ Text "Fim de Jogo"
    | otherwise -> desenhaJogo jogo app
  _ -> Blank

desenhaMenu :: Picture -> Picture -> Picture
desenhaMenu imgFundo imgJogar = Pictures
  [ Scale escalaX escalaY imgFundo,
    Translate 0 (-350) (Scale escalaX escalaY imgJogar)
  ]
  where
    escalaX = fromIntegral janelaLargura / 1920
    escalaY = fromIntegral janelaAltura / 1080

desenhaLoja :: [TorreLoja] -> Maybe TorreLoja -> Int -> EstadoApp -> Picture
desenhaLoja loja torreSelecionada moedas app =
  Translate (fromIntegral janelaLargura / 2 - 100) (fromIntegral janelaAltura / 2 - 100) $
  Pictures $ zipWith (desenhaTorreLoja app) [0..] loja ++ [desenhaMoedas moedas]
  where
    desenhaTorreLoja :: EstadoApp -> Int -> TorreLoja -> Picture
    desenhaTorreLoja app i torreLoja =
      let offset = fromIntegral (i * 100)
          imgTorre = case tipoTorre torreLoja of
            TorreVermelha -> imgTorreVermelha app
            TorreAzul     -> imgTorreAzul app
            TorreVerde    -> imgTorreVerde app
          circuloSelecionado = case torreSelecionada of
            Just t | t == torreLoja -> Color yellow $ ThickCircle 35 5
            _ -> Blank
      in Pictures
           [ Translate offset 0 $ Scale 0.1 0.1 imgTorre, 
             Translate offset (-50) $ Scale 0.1 0.1 $ Text ("$" ++ show (precoTorre torreLoja)),
             Translate offset 0 circuloSelecionado
           ]

    desenhaMoedas :: Int -> Picture
    desenhaMoedas moedas =
      Translate 50 (-100) $ Scale 0.15 0.15 $ Text ("Moedas: " ++ show moedas)


desenhaJogo :: EstadoJogo -> EstadoApp -> Picture
desenhaJogo estado app = Pictures
  [ desenhaMapa app (mapa estado)
  , desenhaTorres (torres estado) app
  , desenhaInimigos (inimigos estado) app
  , desenhaBase (base estado) app
  , Pictures (map (`desenhaPortal` app) (portais estado))
  , desenhaPortal portalg app
  , desenhaLoja (loja estado) (torreSelecionada estado) (moedas estado) app 
  ]

desenhaMapa :: EstadoApp -> Mapa -> Picture
desenhaMapa app mapa = Translate offsetX offsetY $ Pictures $ concatMap (desenhaLinha app) (zip [0 ..] mapa)
  where
    offsetX = - fromIntegral janelaLargura / 2
    offsetY = - fromIntegral janelaAltura / 2
    desenhaLinha app (y, linha) = map (desenhaTerreno app y) (zip [0 ..] linha)
    desenhaTerreno app y (x, terreno) =
      Translate (fromIntegral x * tamanhoBloco) (fromIntegral y * tamanhoBloco) (desenhaTerrenoBase terreno app)

desenhaTerrenoBase :: Terreno -> EstadoApp -> Picture
desenhaTerrenoBase Relva app = 
  Scale escalaX escalaY (imgRelva app)
  where
    (largura, altura) = dimRelva app
    escalaX = tamanhoBloco / largura
    escalaY = tamanhoBloco / altura

desenhaTerrenoBase Terra app = 
  Scale escalaX escalaY (imgTerra app)
  where
    (largura, altura) = dimTerra app
    escalaX = tamanhoBloco / largura
    escalaY = tamanhoBloco / altura

desenhaTerrenoBase Agua app = 
  Scale escalaX escalaY (imgAgua app)
  where
    (largura, altura) = dimAgua app
    escalaX = tamanhoBloco / largura
    escalaY = tamanhoBloco / altura


desenhaTorres :: [Torre] -> EstadoApp -> Picture
desenhaTorres torres app = Pictures $ map (desenhaTorre app) torres
  where
    desenhaTorre :: EstadoApp -> Torre -> Picture
    desenhaTorre app torre =
      let (x, y) = posicaoTorre torre
          imgTorre = imgTorreVermelha app 
          escala = tamanhoBloco / 500 
      in Translate (x * tamanhoBloco) (y * tamanhoBloco) $ Scale escala escala imgTorre


comprarTorre :: Float -> EstadoJogo -> EstadoApp -> EstadoApp
comprarTorre x jogo app =
  let lojaAtual = loja jogo
      indiceTorre = floor ((x - (fromIntegral janelaLargura / 2 - 200)) / 100)
  in if indiceTorre >= 0 && indiceTorre < length lojaAtual
        then let torreSelecionada = lojaAtual !! indiceTorre
             in if moedas jogo >= precoTorre torreSelecionada
                   then app { estadoAtual = Jogando jogo { moedas = moedas jogo - precoTorre torreSelecionada } }
                   else app 
        else app 

        

colocarTorre :: Posicao -> EstadoJogo -> EstadoApp -> EstadoApp
colocarTorre pos jogo app =
  case torreSelecionada jogo of
    Just torreLoja ->
      let novaTorre = Torre
            { posicaoTorre = pos
            , danoTorre = 100 
            , alcanceTorre = 5
            , rajadaTorre = 1
            , cicloTorre = 1.0
            , tempoTorre = 0.0
            , projetilTorre = Projetil Fogo (Finita 3.0)
            }
      in app { estadoAtual = Jogando jogo
                { torres = novaTorre : torres jogo
                , moedas = moedas jogo - precoTorre torreLoja
                , torreSelecionada = Nothing 
                }
             }
    Nothing -> app 


desenhaInimigos :: [Inimigo] -> EstadoApp -> Picture
desenhaInimigos inimigos app = Pictures $ map (desenhaInimigo app) inimigos
  where
    desenhaInimigo :: EstadoApp -> Inimigo -> Picture
    desenhaInimigo app inimigo =
      let (x, y) = posicaoInimigo inimigo
          escala = tamanhoBloco / 860 
      in Translate (x * tamanhoBloco) (y * tamanhoBloco) $ Scale escala escala (imgInimigo app)



desenhaBase :: Base -> EstadoApp -> Picture
desenhaBase base app = Translate x y $ Scale (escalaX*2) (escalaY*3) (imgBase app)
  where
    (x, y) = (fst (posicaoBase base) * tamanhoBloco, snd (posicaoBase base) * tamanhoBloco)
    (larguraBase, alturaBase) = (471, 530)
    escalaX = tamanhoBloco / larguraBase
    escalaY = tamanhoBloco / alturaBase


desenhaPortal :: Portal -> EstadoApp -> Picture
desenhaPortal portal app = Translate x y $ Scale (escalaX*1.5) (escalaY*2) (imgPortal app)
  where
    (x, y) = (fst (posicaoPortal portal) * tamanhoBloco, snd (posicaoPortal portal) * tamanhoBloco)
    (larguraPortal, alturaPortal) = (500, 500) 
    escalaX = tamanhoBloco / larguraPortal
    escalaY = tamanhoBloco / alturaPortal

reageEventosApp :: Event -> EstadoApp -> EstadoApp
reageEventosApp (EventKey (MouseButton LeftButton) Down _ (x, y)) app =
  case estadoAtual app of
    MenuPrincipal
      | dentroDoBotao x y (-250, -270) (250, -170) -> app { estadoAtual = Jogando estadoInicialJogo }
      | dentroDoBotao x y (-250, -450) (250, -400) -> app { estadoAtual = Sair }
    Jogando jogo ->
      let lojaX = fromIntegral janelaLargura / 2 - 100
          lojaY = fromIntegral janelaAltura / 2 - 100
          indiceTorre = floor ((x - lojaX) / 100)
      in if y > lojaY - 30 && y < lojaY + 30 && indiceTorre >= 0 && indiceTorre < length (loja jogo)
           then let torreSelecionada = Just (loja jogo !! indiceTorre)
                in app { estadoAtual = Jogando jogo { torreSelecionada = torreSelecionada } }
           else if terrenoValido (mapa jogo) (x / tamanhoBloco, y / tamanhoBloco)
                   && maybe False (\t -> moedas jogo >= precoTorre t) (torreSelecionada jogo)
                   then colocarTorre (x / tamanhoBloco, y / tamanhoBloco) jogo app
                   else app
    _ -> app

reageEventosApp (EventKey (SpecialKey KeyEnter) Down _ _) app =
  case estadoAtual app of
    MenuPrincipal -> app { estadoAtual = Jogando estadoInicialJogo }
    _ -> app

reageEventosApp _ app = app


terrenoValido :: Mapa -> Posicao -> Bool
terrenoValido mapa (x, y) =
  let (ix, iy) = (round x, round y)
  in ix >= 0 && iy >= 0 && iy < length mapa && ix < length (head mapa) && (mapa !! iy !! ix) == Relva

dentroDoBotao :: Float -> Float -> (Float, Float) -> (Float, Float) -> Bool
dentroDoBotao x y (xmin, ymin) (xmax, ymax) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

reageTempoApp :: Float -> EstadoApp -> EstadoApp
reageTempoApp dt app =
  case estadoAtual app of
    Jogando jogo ->
      let inimigosAtualizados = atualizaInimigos dt (mapa jogo) (inimigos jogo)
          portalAtualizado = atualizaPortal dt inimigosAtualizados (head $ portais jogo)
          novaBase = atualizaJogo dt (torres jogo) inimigosAtualizados portalAtualizado (base jogo)
          novasTorres = map (atualizaTorre dt inimigosAtualizados) (torres jogo)
      in app { estadoAtual = Jogando jogo { inimigos = inimigosAtualizados
                                          , base = novaBase
                                          , torres = novasTorres
                                          , portais = [portalAtualizado]
                                          }
             }
    _ -> app

verificaFimDeJogo :: EstadoJogo -> Bool
verificaFimDeJogo jogo = terminouJogo Jogo
  { baseJogo = base jogo
  , inimigosJogo = inimigos jogo
  , mapaJogo = mapa jogo
  , torresJogo = torres jogo
  , lojaJogo = []
  , portaisJogo = []
  }

baseg :: Base
baseg = Base { posicaoBase = (-20.5,13), creditosBase = 100, vidaBase = 100.0 }

portalg :: Portal
portalg = Portal { posicaoPortal = (15, 8.5), ondasPortal = [] }

torreFogo :: Torre
torreFogo = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil1 }

torreGelo :: Torre
torreGelo = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil2 }

torreResina :: Torre
torreResina = Torre { posicaoTorre = (0, 1), alcanceTorre = 2.0, rajadaTorre = 3, cicloTorre = 1.0, danoTorre = 1.0, tempoTorre = 2.0, projetilTorre = projetil3 }

inimigog :: Inimigo
inimigog = Inimigo { posicaoInimigo = (-20, 10), direcaoInimigo = Este, velocidadeInimigo = 1.0, vidaInimigo = 100.0, ataqueInimigo = 10.0, butimInimigo = 50, projeteisInimigo = []}

mapag :: [[Terreno]]
mapag = [
    [t, t, t, t, t, t,t, t, t, t, t, t, t,t, t, t, t, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r ,r,r ,r, r, r, t, r, r, r, r, r, r, r, a, a ,a, r, r, r, r, r , r,r , r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, a, a, a, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, a, a, a,a, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, a, a, a, a, a, a, a, a, r, r, r, r, r, r, r, r, r, r],
    [t, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, r, r, r, r, r,r, r, r, r, r, r, r,r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r, r],
    [r, t, t, t, t, t,t, t, t, t, t, t, t,t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t, t]
    ]
    where
        t = Terra
        r = Relva
        a = Agua
