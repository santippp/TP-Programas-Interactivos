type Monomio = (Float, Int) -- Coeficiente y Exponente

type Polinomio = [Monomio]

-- Implementacion Monomio

crearM :: Int -> Float -> Monomio
crearM e c = (c, e)

obtenerExp :: Monomio -> Int
obtenerExp (c, e) = e

obtenerCoef :: Monomio -> Float
obtenerCoef (c, e) = c 

evalM :: Monomio -> Float -> Float
evalM (c, e) x = (x ** fromIntegral e) * c

-- Implementacion Polinomio

crearP :: Polinomio
crearP = []

obtenerGradoP :: Polinomio -> Int
obtenerGradoP [(c, e)] = e 
obtenerGradoP ((c, e) : xs) = if e > (obtenerGradoP xs) then e 
                                                   else obtenerGradoP xs

obtenerCoeficienteP :: Polinomio ->  Float
obtenerCoeficienteP [(c, e)] = c
obtenerCoeficienteP p = go p (obtenerGradoP p)
    where
        go [] _ = 0
        go ((c1, e1) : xs) e = if e1 == e then c1 + go xs e 
                                        else go xs e

sumar :: Monomio -> Polinomio -> Polinomio
sumar (c, e) [] = [(c,e)]
sumar (c1, e1) ((c2, e2) : xs)  
                        | e1 == e2 = ((c1 + c2), e1) : xs
                        | otherwise = (c2, e2) : sumar (c1, e1) xs
                        
evalP :: Polinomio -> Float -> Float
evalP [] _ = 0
evalP (x : xs) value = (evalM x value) + (evalP xs value)

-- Ejemplo: -2x^3 + 2x^3 + 5x^5 --> [(-2, 3), (2,3), (5,5)]

