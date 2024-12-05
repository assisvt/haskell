-- Função para filtrar exercícios curtos (menos de 30 minutos)
filtrarCurtos :: [(Float, Float)] -> [(Float, Float)]
filtrarCurtos exercicios = filter (\(_, duracao) -> duracao < 30) exercicios

-- Função para filtrar exercícios longos (30 minutos ou mais)
filtrarLongos :: [(Float, Float)] -> [(Float, Float)]
filtrarLongos exercicios = filter (\(_, duracao) -> duracao >= 30) exercicios

-- Função para calcular as calorias queimadas por minuto em cada exercício
caloriasPorMinuto :: [(Float, Float)] -> [Float]
caloriasPorMinuto exercicios = map (\(cal, duracao) -> cal / duracao) exercicios

-- Função para calcular a média de calorias queimadas por minuto
mediaCaloriasPorMinuto :: [(Float, Float)] -> Float
mediaCaloriasPorMinuto exercicios = 
    let calMin = caloriasPorMinuto exercicios
        total = foldl (+) 0 calMin
    in total / fromIntegral (length calMin)

-- Função para identificar o exercício mais intenso (maior gasto calórico por minuto)
exercicioMaisIntenso :: [(Float, Float)] -> (Float, Float)
exercicioMaisIntenso exercicios = foldr (\x acc -> if (fst x / snd x) > (fst acc / snd acc) then x else acc) (head exercicios) exercicios

-- Função para filtrar exercícios que queimaram mais calorias que a meta
exerciciosAcimaDaMeta :: Float -> [(Float, Float)] -> [(Float, Float)]
exerciciosAcimaDaMeta meta exercicios = filter (\(cal, _) -> cal > meta) exercicios

-- Função principal de análise dos exercícios
analiseExercicios :: [(Float, Float)] -> Float -> ([(Float, Float)], [(Float, Float)], Float, (Float, Float), [(Float, Float)])
analiseExercicios exercicios meta = 
    let curtos = filtrarCurtos exercicios
        longos = filtrarLongos exercicios
        media = mediaCaloriasPorMinuto exercicios
        intenso = exercicioMaisIntenso exercicios
        acimaMeta = exerciciosAcimaDaMeta meta exercicios
    in (curtos, longos, media, intenso, acimaMeta)

-- Função main para testar
main :: IO ()
main = do
    let exercicios = [(200.0, 25.0), (300.0, 40.0), (150.0, 20.0), (500.0, 35.0), (100.0, 15.0)]
        metaCalorias = 150.0
    let (curtos, longos, media, intenso, acimaMeta) = analiseExercicios exercicios metaCalorias
    
    -- Exibe os resultados
    putStrLn "Exercícios curtos (menos de 30 minutos):"
    print curtos
    putStrLn "\nExercícios longos (30 minutos ou mais):"
    print longos
    putStrLn $ "\nMédia de calorias por minuto: " ++ show media
    putStrLn $ "\nExercício mais intenso (maior gasto calórico por minuto): " ++ show intenso
    putStrLn "\nExercícios com calorias queimadas acima da meta:"
    print acimaMeta
