calculoIMC :: Float -> Float -> Float
calculoIMC altura peso = imc
    where imc = peso/(altura^2)

classificaIMC :: Float -> String
classificaIMC imc
    | imc <= 18.45 = "Abaixo do peso"
    | imc <= 24.9 = "Peso normal"
    | imc <= 29.9 = "Sobrepeso"
    | imc > 30 = "Obesidade"

contaVerdadeiro:: [Bool] -> Int
contaVerdadeiro condicoes = length (filter (== True) condicoes)

contaCriticos :: [Bool] -> String
contaCriticos condicoesP
    | valoresVerdadeiros >= 3 = "Condicoes de alta prioridade"
    | valoresVerdadeiros <= 2 = "Condicoes de baixa prioridade"
    where valoresVerdadeiros = contaVerdadeiro condicoesP

prioridadeIdade :: Int -> String
prioridadeIdade idade
    | idade >= 65 = "Prioridade Alta"
    | idade >= 18 && idade < 65 = "Prioridade Baixa"
    | otherwise = "Menor de Idade"

avaliacaoPaciente :: Float -> Float -> [Bool] -> Int -> String
avaliacaoPaciente altura peso condicoes idade
    | classificaIMC imc == "Obesidade" || contaVerdadeiro condicoes > 3 || idade >= 65 = "Caso Urgente"
    | otherwise = "Caso Regular"
  where imc = calculoIMC altura peso

listaC = [True, False, True, False, True, False]

main :: IO ()
main = do
    print("Calculo IMC: ")
    print(calculoIMC 1.67 64 )
    print("Prioridade: ")
    print(prioridadeIdade 70)
    print("Avaliar paciente: ")
    print(contaCriticos listaC)
    print(avaliacaoPaciente 1.90 140 listaC  19)

    