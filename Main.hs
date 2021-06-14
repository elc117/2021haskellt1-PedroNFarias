import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)

-- divide pela variedade de cores eu escolhi 6
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle (a ++ b ++ c ++ d ++ e ++ f)
  where a = [(i,0,0) | i <- lcg qtdTrue seed1 range]
        b = [(0,i,0) | i <- lcg qtdTrue seed2 range]
        c = [(0,0,i) | i <- lcg qtdTrue seed1 range]
        d = [(0,i,i) | i <- lcg qtdTrue seed2 range]
        e = [(i,0,i) | i <- lcg qtdTrue seed1 range]
        f = [(i,i,0) | i <- lcg qtdTrue seed2 range]
        qtd = 6 
        seed1 = 1
        seed2 = 2
        range = 255
        qtdTrue = round(fromIntegral n /qtd)

--Gera uma lista com um Tamanho de lista -> Seed -> Número máximo da lista
--Utiliza um método de progressão matemática
lcg :: Int -> Int -> Int -> [Int]
lcg 0 x nmax = []
lcg qtd x nmax = x1 : lcg (qtd-1) x1 nmax
  where x1 = mod (a * x + c) nmax
        a = 66
        c = 54321

--Função auxiliar, eu tava com muito problema se não usasse isso
convertIntToFloat :: [Int] -> [Float]
convertIntToFloat n = map fromIntegral n

--Recebe duas listas e coloca os quadrados nas coordenadas das listas
genRects :: [Int] -> [Int] -> [Rect]
genRects lis1 lis2  = [(x, w, h) | x <- listConcat]
  where (w,h) = (100,100)
        listConcat = zipWith (\x y -> (x,y)) (convertIntToFloat lis1) (convertIntToFloat lis2)


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
--add filter='url(#blur)'

-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles

--Aplica um blur de filtro
svgFil_blur :: String
svgFil_blur = printf "<filter id='blur'><feGaussianBlur in='SourceGraphic' stdDeviation='2' /></filter>\n"

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
  writeFile "rects.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd -- add ++svgFil_blur
        svgfigs = svgElements svgRect rects (map svgStyle palette)
        rects = genRects (lcg nrects seed1 y) (lcg nrects seed2 y) --Recebe duas listas randoms que podem ser modificadas as suas seeds
        palette = rgbPalette nrects
        nrects = 100 --Quantidade de quadrados
        (w,h) = (500,500) -- width,height da imagem SVG
        y = 500 --Tive que declarar como int pra evitar conflito
        seed1 = 78
        seed2 = 5

