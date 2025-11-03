import Parsing
import Control.Applicative ((<|>))


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
obtenerGradoP [] = 0
obtenerGradoP [(c, e)] = e 
obtenerGradoP ((c, e) : xs) = if e > (obtenerGradoP xs) then e 
                                                         else obtenerGradoP xs

obtenerCoeficienteP :: Polinomio -> Float
obtenerCoeficienteP [] = 0
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

-- PARSERS

numero :: Parser Float
numero = do char '-'
            n <- nat
            return (fromIntegral (-n))
         <|> do n <- nat
                return (fromIntegral n)

-- Parser para monomios
monomio :: Parser Monomio
monomio = do c <- numero
             char 'x'
             char '^'
             e <- nat
             return (c, e)
          <|> do c <- numero
                 char 'x'
                 return (c, 1)
          <|> do char 'x'
                 char '^'
                 e <- nat
                 return (1, e)
          <|> do char 'x'
                 return (1, 1)
          <|> do c <- numero
                 return (c, 0)

-- Parser para '+' ignorando espacios
mas :: Parser Char
mas = do space
         char '+'
         space
         return '+'

-- Parser para '-' ignorando espacios
menos :: Parser Char
menos = do space
           char '-'
           space
           return '-'

-- Parser para un monomio con su signo
monomioConSigno :: Parser Monomio
monomioConSigno = do mas
                     m <- monomio
                     return m
                  <|> do menos
                         (c, e) <- monomio
                         return (-c, e)
                  <|> monomio

-- Parser para polinomio completo (ignora espacios al principio y al final)

polinomio :: Parser Polinomio
polinomio = do space
               ms <- many monomioConSigno
               space
               return ms

-- Funciones auxiliares, sirve para verificar que el formato ingresado sea correcto.
parsearPolinomio :: String -> Maybe Polinomio
parsearPolinomio str = 
    case parse polinomio str of
        [(p, "")] -> Just p
        _         -> Nothing

parsearMonomio :: String -> Maybe Monomio
parsearMonomio str = 
    case parse monomio str of
        [(m, "")] -> Just m
        _         -> Nothing

-- Formato de salida

mostrarMonomio :: Monomio -> String
mostrarMonomio (c, 0) = show c
mostrarMonomio (1, 1) = "x"
mostrarMonomio (-1, 1) = "-x"
mostrarMonomio (c, 1) = show c ++ "x"
mostrarMonomio (1, e) = "x^" ++ show e
mostrarMonomio (-1, e) = "-x^" ++ show e
mostrarMonomio (c, e) = show c ++ "x^" ++ show e

mostrarPolinomio :: Polinomio -> String
mostrarPolinomio [] = "0"
mostrarPolinomio [m] = mostrarMonomio m
mostrarPolinomio (m:ms) = mostrarMonomio m ++ formatoResto ms
    where
        formatoResto [] = ""
        formatoResto ((c,e):xs) 
            | c >= 0    = " + " ++ mostrarMonomio (c,e) ++ formatoResto xs
            | otherwise = " - " ++ mostrarMonomio (-c,e) ++ formatoResto xs

-- Funcion auxiliar para leer floats

leerFloat :: String -> Maybe Float
leerFloat s = case reads s of        -- Reads devuelve una lista de posibles interpretaciones
                [(x, "")] -> Just x
                _         -> Nothing

-- Menú

main :: IO ()
main = menuPrincipal crearP 

menuPrincipal :: Polinomio -> IO ()
menuPrincipal poli = do
    putStrLn "\n=========================================="
    putStrLn "      TAD POLINOMIO - MENU PRINCIPAL"
    putStrLn "=========================================="
    putStrLn $ "Polinomio actual: " ++ mostrarPolinomio poli
    putStrLn "=========================================="
    putStrLn "1. Ingresar un polinomio"
    putStrLn "2. Obtener el grado del polinomio"
    putStrLn "3. Sumar un monomio al polinomio"
    putStrLn "4. Obtener el coeficiente principal"
    putStrLn "5. Evaluar el polinomio en un valor"
    putStrLn "6. Salir"
    putStrLn "=========================================="
    putStr "Seleccione una opcion: "
    opcion <- getLine
    procesarOpcion opcion poli

procesarOpcion :: String -> Polinomio -> IO ()
procesarOpcion "1" _ = opIngresarPolinomio
procesarOpcion "2" poli = opObtenerGrado poli
procesarOpcion "3" poli = opSumarMonomio poli
procesarOpcion "4" poli = opObtenerCoeficiente poli
procesarOpcion "5" poli = opEvaluarPolinomio poli
procesarOpcion "6" _ = do
    putStrLn "\n¡Hasta luego!"
    return ()
procesarOpcion _ poli = do
    putStrLn "\nElija una opción válida."
    menuPrincipal poli

-- Funciones del menú (opciones)

opIngresarPolinomio :: IO ()
opIngresarPolinomio = do
    putStrLn "\n--- INGRESAR POLINOMIO ---"
    putStr "\nIngrese el polinomio: "
    input <- getLine
    case parsearPolinomio input of
        Nothing -> do
            putStrLn "Formato invÃ¡lido, intente nuevamente."
            opIngresarPolinomio
        Just poli -> do
            putStrLn $ "Polinomio ingresado: " ++ mostrarPolinomio poli
            menuPrincipal poli

opObtenerGrado :: Polinomio -> IO ()
opObtenerGrado poli = do
    putStrLn "\n--- OBTENER GRADO DEL POLINOMIO ---"
    let grado = obtenerGradoP poli
    putStrLn $ "El grado del polinomio es: " ++ show grado
    esperarEnter
    menuPrincipal poli

opSumarMonomio :: Polinomio -> IO ()
opSumarMonomio poli = do
    putStrLn "\n--- SUMAR MONOMIO ---"
    putStr "\nIngrese el monomio a sumar: "
    input <- getLine
    case parsearMonomio input of
        Nothing -> do
            putStrLn "Formato inválido, intente nuevamente."
            opSumarMonomio poli -- si falla vuelve a llamarse
        Just mono -> do
            let nuevoPoli = sumar mono poli
            putStrLn $ "Nuevo polinomio: " ++ mostrarPolinomio nuevoPoli
            esperarEnter
            menuPrincipal nuevoPoli

opObtenerCoeficiente :: Polinomio -> IO ()
opObtenerCoeficiente poli = do
    putStrLn "\n--- OBTENER COEFICIENTE PRINCIPAL ---"
    let coef = obtenerCoeficienteP poli
    putStrLn $ "El coeficiente principal es: " ++ show coef
    esperarEnter
    menuPrincipal poli

opEvaluarPolinomio :: Polinomio -> IO ()
opEvaluarPolinomio poli = do
    putStrLn "\n--- EVALUAR POLINOMIO ---"
    putStr "Ingrese el valor de x: "
    input <- getLine
    case leerFloat input of
        Nothing -> do
            putStrLn "Ingrese un nÃºmero vÃ¡lido."
            opEvaluarPolinomio poli
        Just valor -> do
            let resultado = evalP poli valor
            putStrLn $ "P(" ++ show valor ++ ") = " ++ show resultado
            esperarEnter
            menuPrincipal poli

esperarEnter :: IO ()
esperarEnter = do
    putStr "\nPresionar Enter para continuar..."
    _ <- getLine -- no guarda la basura antes del enter
    return ()