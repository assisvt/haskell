import Data.List (foldl')

main :: IO ()
main = do
    let consumos = [50, 120, 80, 150, 200]
        custoPorUnidade = 2.50

        -- Calcular o custo por residência
        custosResidencias = map (\consumo -> fromIntegral consumo * custoPorUnidade) consumos

        -- Filtrar residências com consumo acima de 100 unidades
        consumosAcima100 = filter (> 100) consumos

        -- Calcular o custo total com reduce (foldl')
        custoTotal = foldl' (\acc consumo -> acc + fromIntegral consumo * custoPorUnidade) 0 consumos

    -- Imprimir resultados
    putStrLn $ "Custo por residência: " ++ show custosResidencias
    putStrLn $ "Consumo acima de 100: " ++ show consumosAcima100
    putStrLn $ "Custo total: " ++ show custoTotal
{-
RODAR main
Custo por residência: [125.0,300.0,200.0,375.0,500.0]
Consumo acima de 100: [120,150,200]
Custo total: 1500.0
-}
