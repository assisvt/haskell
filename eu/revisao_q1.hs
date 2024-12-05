-- Função recursiva para calcular o total de árvores ao longo dos anos
totalPlantas :: Double -> Double -> Int -> Double
totalPlantas arvoresIniciais taxaCrescimento 0 = arvoresIniciais  -- Caso base: 0 anos
totalPlantas arvoresIniciais taxaCrescimento anos =
  totalPlantas (arvoresIniciais * (1 + taxaCrescimento)) taxaCrescimento (anos - 1)
{-
Copiar código
let consumos = [50, 120, 80, 150, 200]
-- Resultado esperado:
-- Custo por residência: [125.0, 300.0, 200.0, 375.0, 500.0]
-- Consumo acima de 100: [120, 150, 200]
-- Custo total: 1500.0
-}

