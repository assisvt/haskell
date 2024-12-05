
filtrarPares :: [Int] -> [Int]
filtrarPares xs = filter even xs

-- Exemplo de uso:
-- filtrarPares [1, 2, 3, 4, 5, 6] retorna [2, 4, 6]


filtrarImpares :: [Int] -> [Int]
filtrarImpares xs = filter odd xs

-- Exemplo de uso:
-- filtrarImpares [1, 2, 3, 4, 5, 6] retorna [1, 3, 5]


filtrarPositivos :: [Int] -> [Int]
filtrarPositivos xs = filter (> 0) xs

-- Exemplo de uso:
-- filtrarPositivos [-2, -1, 0, 1, 2, 3] retorna [1, 2, 3]

dobrarValores :: [Int] -> [Int]
dobrarValores xs = map (*2) xs

-- Exemplo de uso:
-- dobrarValores [1, 2, 3, 4] retorna [2, 4, 6, 8]

triplicarValores :: [Int] -> [Int]
triplicarValores xs = map (*3) xs

-- Exemplo de uso:
-- triplicarValores [1, 2, 3, 4] retorna [3, 6, 9, 12]

incrementarValores :: [Int] -> [Int]
incrementarValores xs = map (+1) xs

-- Exemplo de uso:
-- incrementarValores [1, 2, 3, 4] retorna [2, 3, 4, 5]



somatorio :: [Int] -> Int
somatorio xs = foldr (+) 0 xs

-- Exemplo de uso:
-- somatorio [1, 2, 3, 4] retorna 10

produtoLista :: [Int] -> Int
produtoLista xs = foldr (*) 1 xs

-- Exemplo de uso:
-- produtoLista [1, 2, 3, 4] retorna 24

concatenarStrings :: [String] -> String
concatenarStrings xs = foldr (++) "" xs

-- Exemplo de uso:
-- concatenarStrings ["Hello", " ", "World", "!"] retorna "Hello World!"

produtoImpares :: [Int] -> Int
produtoImpares xs = foldr (*) 1 (map (*2) (filter odd xs))

-- Exemplo de uso:
-- produtoImpares [1, 2, 3, 4, 5] retorna 30 (porque dobra os ímpares: 2 * 6 * 10)