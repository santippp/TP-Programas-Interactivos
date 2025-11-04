import Parsing
import Control.Applicative ((<|>))
import System.Process (callCommand)
import Data.List (sortBy)
import Data.Ord (comparing)

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

float' :: Parser Float
float' =
  do sign <- (char '-' >> return (-1))
                 <|> (char '+' >> return 1)
                 <|> return 1
     xs <- many1 digit
     frac <- (do char '.'
                 ys <- many1 digit
                 return (read ("0." ++ ys)))
             <|> return 0
     return $ sign * (read xs + frac)

-- Parser para monomios
monomio :: Parser Monomio
monomio = do c <- float'
             string "x^"
             e <- nat
             return (c, e)
          <|> do c <- float'
                 char 'x'
                 return (c, 1)
          <|> do string "x^"
                 e <- nat
                 return (1, e)
          <|> do char 'x'
                 return (1, 1)
          <|> do c <- float'
                 return (c, 0)
          <|> do string "-x^"
                 e <- nat
                 return (-1, e)
          <|> do string "-x"
                 return (-1, 1)


polinomio :: Parser Polinomio
polinomio = do ms <- many monomio
               return ms

-- Funciones auxiliares
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

parseaFloat :: String -> Maybe Float
parseaFloat str = 
    case parse float' str of
        [(m, "")] -> Just m
        _         -> Nothing

-- Imprimir Monomio

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

ordenar :: Polinomio -> Polinomio
ordenar p = sortBy (comparing snd) p 

imprimirPolinomio :: Polinomio -> String
imprimirPolinomio p = mostrarPolinomio (reverse (ordenar p))

-- Menu

main :: IO ()
main = menuPrincipal crearP 

menuPrincipal :: Polinomio -> IO ()
menuPrincipal poli = do
    clearScreen
    putStrLn "Menu Principal"
    putStrLn "1. Ingresar un polinomio"
    putStrLn "2. Obtener el grado del polinomio"
    putStrLn "3. Sumar un monomio al polinomio"
    putStrLn "4. Obtener el coeficiente principal"
    putStrLn "5. Evaluar el polinomio en un valor"
    putStrLn "6. Mostrar polinomio"
    putStrLn "7. Salir"
    putStr "\nSeleccione una opcion: "
    opcion <- getLine
    procesarOpcion opcion poli

procesarOpcion :: String -> Polinomio -> IO ()
procesarOpcion "1" _ = opIngresarPolinomio
procesarOpcion "2" poli = opObtenerGrado poli
procesarOpcion "3" poli = opSumarMonomio poli
procesarOpcion "4" poli = opObtenerCoeficiente poli
procesarOpcion "5" poli = opEvaluarPolinomio poli
procesarOpcion "6" poli = mostrarPolinomioIO poli
procesarOpcion "7" _ = do
    putStrLn "\nFin del programa"
    return ()
procesarOpcion _ poli = do
    putStrLn "\nElija una opción valida."
    menuPrincipal poli

-- Funciones del menu (opciones)

opIngresarPolinomio :: IO ()
opIngresarPolinomio = do
    putStr "\nIngrese el polinomio: "
    input <- getLine
    case parsearPolinomio input of
        Nothing -> do
            putStrLn "Formato invalido"
            opIngresarPolinomio
        Just poli -> do
            putStrLn $ "Polinomio ingresado: " ++ imprimirPolinomio poli
            esperarEnter
            menuPrincipal poli

opObtenerGrado :: Polinomio -> IO ()
opObtenerGrado poli = do
    let grado = obtenerGradoP poli
    putStrLn $ "El grado del polinomio es: " ++ show grado
    esperarEnter
    menuPrincipal poli

opSumarMonomio :: Polinomio -> IO ()
opSumarMonomio poli = do
    putStr "\nIngrese el monomio a sumar: "
    input <- getLine
    case parsearMonomio input of
        Nothing -> do
            putStrLn "Formato invalido"
            opSumarMonomio poli
        Just mono -> do
            let nuevoPoli = sumar mono poli
            mostrarPolinomioIO nuevoPoli
            esperarEnter
            menuPrincipal nuevoPoli

opObtenerCoeficiente :: Polinomio -> IO ()
opObtenerCoeficiente poli = do
    let coef = obtenerCoeficienteP poli
    putStrLn $ "El coeficiente principal es: " ++ show coef
    esperarEnter
    menuPrincipal poli

opEvaluarPolinomio :: Polinomio -> IO ()
opEvaluarPolinomio poli = do
    putStr "Ingrese el valor de x: "
    input <- getLine
    case parseaFloat input of
        Nothing -> do
            putStrLn "Ingrese un numero valido."
            opEvaluarPolinomio poli
        Just valor -> do
            let resultado = evalP poli valor
            putStrLn $ "P(" ++ show valor ++ ") = " ++ show resultado
            esperarEnter
            menuPrincipal poli

esperarEnter :: IO ()
esperarEnter = do
    putStr "\nPresionar Enter para continuar..."
    _ <- getLine
    return ()

mostrarPolinomioIO :: Polinomio -> IO ()
mostrarPolinomioIO p = do 
    putStrLn $ "\nEl polinomio es: " ++ imprimirPolinomio p
    esperarEnter
    menuPrincipal p

clearScreen :: IO ()
clearScreen = callCommand "clear"
