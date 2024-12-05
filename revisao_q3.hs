-- Selecionar produtos sustentáveis com impacto ambiental ≤ 5
sustentaveis :: [Int] -> [Int]
sustentaveis impactos = filter (<= 5) impactos

-- Calcular os descontos
calcularDescontos :: [Int] -> [Int]
calcularDescontos impactos = map (\x -> if x <= 3 then 20 else 10) impactos

-- Calcular os preços finais com base nos descontos
calcularPrecos :: [Int] -> [Int] -> [Int]
calcularPrecos [] _ = []
calcularPrecos _ [] = []
calcularPrecos (preco:precos) (desconto:descontos) = 
    let precoFinal = preco - (preco * desconto `div` 100)
    in precoFinal : calcularPrecos precos descontos

-- Combinar tudo
main :: IO ()
main = do
    let impactos = [2, 4, 6, 3, 7]
    let precosBase = [100, 100, 100, 100, 100] -- Exemplo de preços base
    let produtosSustentaveis = sustentaveis impactos
    let descontos = calcularDescontos produtosSustentaveis
    let precosFinais = calcularPrecos (take (length produtosSustentaveis) precosBase) descontos

    putStrLn $ "Produtos sustentáveis: " ++ show produtosSustentaveis
    putStrLn $ "Descontos: " ++ show descontos
    putStrLn $ "Preços finais: " ++ show precosFinais
{-
Copiar código
let impactos = [2, 4, 6, 3, 7]
-- RODAR "main"
-- Resultado esperado:
-- Produtos sustentáveis: [2, 4, 3]
-- Descontos: [20%, 10%, 20%]
-- Preços finais (exemplo): [80, 90, 80]
-}
