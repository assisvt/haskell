filtrarDespesas :: [Float] -> [Float]
filtrarDespesas transacoes = filter (< 0) transacoes

filtrarReceitas :: [Float] -> [Float]
filtrarReceitas transacoes = filter (> 0) transacoes

calcularSaldo :: [Float] -> Float
calcularSaldo transacoes = foldl (+) 0 transacoes

ajustarDespesas :: [Float] -> [Float]
ajustarDespesas despesas = map (* 1.1) despesas

maiorTransacao :: [Float] -> Float
maiorTransacao transacoes = foldr max (head transacoes) transacoes

totalDespesas :: [Float] -> Float
totalDespesas despesas = foldr (+) 0 despesas

totalReceitas :: [Float] -> Float
totalReceitas receitas = foldr (+) 0 receitas

analiseTransacoes :: [Float] -> (Float, Float, Float, Float, Float)
analiseTransacoes transacoes = 
    let despesas = filtrarDespesas transacoes
        receitas = filtrarReceitas transacoes
        saldo = calcularSaldo transacoes
        despesasAjustadas = ajustarDespesas despesas
        maior = maiorTransacao transacoes
        totalDesp = totalDespesas despesas
        totalRec = totalReceitas receitas
    in (saldo, totalDesp, totalRec, maior, calcularSaldo despesasAjustadas)

-- Função main para rodar o código
main :: IO ()
main = do
    let transacoes = [-500.0, 1000.0, -200.0, 300.0, -100.0]
    let (saldo, totalDesp, totalRec, maior, despesasAjustadas) = analiseTransacoes transacoes
    putStrLn $ "Saldo total: " ++ show saldo
    putStrLn $ "Total de despesas: " ++ show totalDesp
    putStrLn $ "Total de receitas: " ++ show totalRec
    putStrLn $ "Maior transação: " ++ show maior
    putStrLn $ "Saldo das despesas ajustadas: " ++ show despesasAjustadas
