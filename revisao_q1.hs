-- Função recursiva para calcular o total de árvores ao longo dos anos
totalPlantas :: Double -> Double -> Int -> Double
totalPlantas arvoresIniciais taxaCrescimento 0 = arvoresIniciais  -- Caso base: 0 anos
totalPlantas arvoresIniciais taxaCrescimento anos =
  totalPlantas (arvoresIniciais * (1 + taxaCrescimento)) taxaCrescimento (anos - 1)

