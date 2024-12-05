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

-- Calcular o custo total de uma lista de consumos
calcularCustoTotal :: [Int] -> Double
calcularCustoTotal consumos = foldl' (+) 0 (calcularCustos consumos)

-- Função principal
main :: IO ()
main = do
    let consumos = [50, 120, 80, 150, 200] -- Consumos de água das residências
    
    -- Cálculo do custo de cada residência
    let custos = calcularCustos consumos
    
    -- Filtro de consumos acima de 100 unidades
    let altosConsumos = filtrarAltosConsumos consumos
    
    -- Cálculo do custo total para todos os consumos
    let custoTotalTodos = calcularCustoTotal consumos
    
    -- Cálculo do custo total para consumos acima de 100 unidades
    let custoTotalAltos = calcularCustoTotal altosConsumos

    -- Exibir resultados
    putStrLn $ "Custo por residência: " ++ show custos
    putStrLn $ "Consumo acima de 100: " ++ show altosConsumos
    putStrLn $ "Custo total (todas as residências): " ++ show custoTotalTodos
    putStrLn $ "Custo total (apenas residências acima de 100 unidades): " ++ show custoTotalAltos
  {-
  let consumos = [50, 120, 80, 150, 200]
-- Resultado esperado:
-- Custo por residência: [125.0, 300.0, 200.0, 375.0, 500.0]
-- Consumo acima de 100: [120, 150, 200]
-- Custo total (todas as residências): 1500.0
-- Custo total (apenas residências acima de 100 unidades): 1175.0

  -}
