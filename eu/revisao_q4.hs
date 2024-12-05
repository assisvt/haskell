-- Selecionar alunos aprovados
aprovados :: [Int] -> [Int]
aprovados notas = filter (>= 60) notas

-- Calcular a média geral (arredondada para inteiro)
mediaGeral :: [Int] -> Int
mediaGeral notas = sum notas `div` length notas

-- Calcular diferenças entre as notas e a média
diferencas :: [Int] -> [Int]
diferencas notas = 
    let media = mediaGeral notas
    in map (\nota -> nota - media) notas

-- Contar alunos acima da média usando recursão
contarAcimaDaMedia :: [Int] -> Int
contarAcimaDaMedia [] = 0
contarAcimaDaMedia (nota:notas) = 
    let media = mediaGeral (nota:notas)
    in if nota > media 
       then 1 + contarAcimaDaMedia notas
       else contarAcimaDaMedia notas

-- Combinar tudo
main :: IO ()
main = do
    let notas = [55, 70, 80, 45, 60]
    
    let listaAprovados = aprovados notas
    let listaDiferencas = diferencas notas
    let qtdAcimaDaMedia = contarAcimaDaMedia notas

    putStrLn $ "Aprovados: " ++ show listaAprovados
    putStrLn $ "Diferenças: " ++ show listaDiferencas
    putStrLn $ "Alunos acima da média: " ++ show qtdAcimaDaMedia
{-
Copiar código
let notas = [55, 70, 80, 45, 60]
-- Resultado esperado:
-- Aprovados: [70, 80, 60]
-- Diferenças: [-10, 10, 20, -35, -5] // ERRO: Diferenças: [-7,8,18,-17,-2]
-- Alunos acima da média: 2
-}
