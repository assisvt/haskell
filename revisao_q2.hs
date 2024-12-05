import Data.List (foldl')

-- Custo por unidade consumida
custoPorUnidade :: Double
custoPorUnidade = 2.50

-- Calcular o custo de cada residência
calcularCustos :: [Int] -> [Double]
calcularCustos consumos = map (\consumo -> fromIntegral consumo * custoPorUnidade) consumos

-- Selecionar residências com consumo acima de 100 unidades
filtrarAltosConsumos :: [Int] -> [Int]
filtrarAltosConsumos consumos = filter (> 100) consumos

-- Calcular o custo total (usando reduce -> foldl')
calcularCustoTotal :: [Int] -> Double
calcularCustoTotal consumos =
  foldl' (+) 0 (calcularCustos consumos)

-- Função principal
main :: IO ()
main = do
  let consumos = [50, 120, 80, 150, 200] -- Consumos de água das residências
  
  -- Cálculo do custo de cada residência
  let custos = calcularCustos consumos
  
  -- Filtro de consumos acima de 100 unidades
  let altosConsumos = filtrarAltosConsumos consumos
  
  -- Cálculo do custo total para consumos acima de 100 unidades
  let custoTotal = calcularCustoTotal altosConsumos

  -- Exibir resultados
  putStrLn $ "Custo por residência: " ++ show custos
  putStrLn $ "Consumo acima de 100: " ++ show altosConsumos
  putStrLn $ "Custo total: " ++ show custoTotal
