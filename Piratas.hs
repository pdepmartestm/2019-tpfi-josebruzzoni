import Text.Show.Functions
type Nombre = [Char]
type Valor = Integer
type Tesoro = (Nombre ,Valor)
type Botin = [ Tesoro ]
type Pirata = ( Nombre , Botin )
type Tripulacion = [ Pirata ] 
type Saqueo = (Tesoro -> Bool)
type Barco = (Nombre, Tripulacion, Saqueo)
type Isla = (Nombre, Tesoro)
type Ciudad = (Nombre, Botin)

-- Ej de un solo pirata: [ ("Jack Sparrow" , [ ("Arena",0) , ("Tesoro Dorado",10000)] ) ] 
--type Pirata = [ ([Char] , [([Char] ,Int)]) ]
--type Botin = [ ([Char] ,Int) ]
--type Tesoro = ([Char],Int)

--EJEMPLOS

ejemploBotin = [("Frasco de Arena",5) , ("Brujula",10000)]
jackSparrow = ("Jack Sparrow" , [("Frasco de Arena",0) , ("Brujula",10000)])
davidJones = ("David Jones" , [("Cajita musical",1)])
anneBonny = ("Anne Bonny", [("Doblones",100) , ("Frasco de Arena",1)])
elizabethSwann = ("Elizabeth Swann",[("Moneda del Cofre Muerto",100), ("Espada de hierro",50)])
willTurner = ("Will Turner", [("Cuchillo",5)])

perlaNegra = ("Perla Negra", [jackSparrow, anneBonny], saqueoValioso)
holandesErrante = ("Holandes Errante", [davidJones], saqueoConCorazon)

islaTortuga = ("Isla Tortuga", ("Frasco de Arena", 1))
islaDelRon = ("Isla del Ron", ("Botella de Ron", 25))


-- TESOROS PIRATAS

--Funcion de cantidad de tesoros
cantidadDeTesoros :: Pirata -> Int --Cantidad de tesoros que tiene el botin de un pirata
cantidadDeTesoros (nombre,botin) = length botin

--Funciones de Pirata Afortunado
valorDelTesoro :: Tesoro -> Integer --Obtiene el valor de un tesoro
valorDelTesoro (nombre,valor) = valor

valoresDelBotin :: Botin -> [Integer] --Arma una lista solo con los valores de cada tesoro del botin
valoresDelBotin botin = map valorDelTesoro botin

valorTotalDelBotin :: Botin -> Integer --Valor total al sumar todos los botines
valorTotalDelBotin botin = sum (valoresDelBotin botin)

esAfortunado :: Pirata -> Bool --Chequea si el pirata es o no afortunado (botin total > 10000)
esAfortunado (nombre,botin) = valorTotalDelBotin botin > 10000

--Funcion de mismo tesoro con valor diferente

esMismoTesoroConValorDistinto :: Tesoro -> Tesoro -> Bool --Compara los nombres de dos tesoros y los valores
esMismoTesoroConValorDistinto (nombre1,valor1) (nombre2,valor2) = (nombre1 == nombre2) && (valor1 /= valor2)

loTieneOtroPirataConDistintoValor :: Pirata -> Tesoro -> Bool
loTieneOtroPirataConDistintoValor (nombre,botin) unTesoro = any (esMismoTesoroConValorDistinto unTesoro) botin

tienenElMismoTesoroConValorDiferente :: Pirata -> Pirata -> Bool
tienenElMismoTesoroConValorDiferente  (nombre,botin) otroPirata = any (loTieneOtroPirataConDistintoValor otroPirata) botin

--Funcion de Tesoro mas valioso
masValiosoTesoro :: Pirata -> Integer --Valor mas alto entre todos los tesoros del botin de un pirata
masValiosoTesoro (nombre,botin) = maximum (valoresDelBotin botin)

--Funciones de ganar o perder tesoros
nuevoTesoro :: Pirata -> Tesoro -> Pirata --Agrega un tesoro al botin
nuevoTesoro (nombre,botin) tesoro = (nombre, botin ++ [tesoro])

esTesoroValioso :: Tesoro -> Bool --Determina si un tesoro es valioso (valor > 100)
esTesoroValioso (nombre,valor) = valor > 100

noEsTesoroValioso :: Tesoro -> Bool --Determina si un tesoro NO es valioso (valioso implica que: valor > 100)
noEsTesoroValioso unTesoro = not (esTesoroValioso unTesoro)

perderTesorosValiosos :: Pirata -> Pirata --Muestra al pirata sin los tesoros valiosos
perderTesorosValiosos (nombre,botin) = (nombre, filter (noEsTesoroValioso) botin)

comparoNombreDelTesoro :: Tesoro -> Tesoro -> Bool
comparoNombreDelTesoro (nombre1,valor1) (nombre2,valor2) = nombre1 /= nombre2

perderUnTesoroValioso :: Pirata -> Tesoro -> Pirata --Muestra el pirata sin el tesoro dado
perderUnTesoroValioso (nombrePirata,botin) unTesoro = (nombrePirata, filter (comparoNombreDelTesoro unTesoro) botin)

-- TEMPORADA DE SAQUEOS

-- Formas de saquear

saqueoValioso :: Tesoro -> Bool
saqueoValioso unTesoro = esTesoroValioso unTesoro

saqueoConCorazon :: Tesoro-> Bool
saqueoConCorazon unTesoro = False

saqueoEspecifico :: Nombre -> Tesoro -> Bool     --tiene prototipo distinto pero como la voy a aplicar parcialmente para que quede Tesoro -> Bool
saqueoEspecifico nombreTesoro (nombre,valor) = (==nombreTesoro) nombre
 
saqueoComplejo :: Nombre -> Tesoro -> Bool     -- idem saqueo específico
saqueoComplejo nombreTesoro unTesoro = (saqueoValioso unTesoro) || (saqueoEspecifico nombreTesoro unTesoro)

agregarTesoro :: (Tesoro->Bool)->Botin->Tesoro->Botin
agregarTesoro formaDeSaqueo unBotin unTesoro
    | formaDeSaqueo unTesoro = unTesoro:unBotin
    | otherwise = unBotin
    

saquear :: (Tesoro -> Bool) -> Tesoro -> Pirata -> Pirata
saquear formaDeSaqueo unTesoro (nombre,botin) = (nombre, agregarTesoro formaDeSaqueo botin unTesoro)

--saquear (nombre,botin) formaDeSaqueo unTesoro = (nombre, botin ++ (filter formaDeSaqueo [unTesoro]))

--saquear (nombre,botin) saqueoValioso unTesoro = (nombre,botin ++ (filter (saqueoValioso) [unTesoro]) )
--saquear (nombre,botin) saqueoValioso unTesoro = (nombre, agregarTesoro (saqueoValioso unTesoro) botin unTesoro)

--saquear (nombre,botin) (saqueoEspecifico "") unTesoro = (nombre,botin ++ (filter (saqueoEspecifico palabraclave) [unTesoro]))
--saquear (nombre,botin) (saqueoEspecifico "") unTesoro = (nombre, agregarTesoro (saqueoEspecifico "" unTesoro) botin unTesoro)

--saquear (nombre,botin) saqueoConCorazon unTesoro = (nombre,botin ++ (filter (saqueoConCorazon) [unTesoro]))
--saquear (nombre,botin) (saqueoComplejo "") unTesoro = (nombre,botin ++ (filter (saqueoComplejo palabraclave) [unTesoro]))


--Navegando los mares

-- funcion para incorporar pirata a tripulacion

agregarPirataABarco :: Barco -> Pirata -> Barco
agregarPirataABarco (nombre, tripulacion, saqueo) unPirata = (nombre, unPirata:tripulacion, saqueo)

--funcion para abandonar barco

abandonarBarco :: Barco -> Pirata -> Barco
abandonarBarco (nombre,tripulacion,saqueo) unPirata = (nombre, filter (/=unPirata) tripulacion, saqueo)

-- anclar en una isla

anclarEnIsla :: Isla -> Barco -> Barco
anclarEnIsla (nombreIsla,tesoro) (nombreBarco, tripu, saqueo) = (nombreBarco, map (saquear saqueo tesoro) tripu, saqueo)

-- atacar una ciudad

atacarCiudad :: Barco -> Ciudad -> Barco
atacarCiudad (nombreBarco, tripu, saqueo) (nombreCiudad, tesoros) = (nombreBarco, zipWith (saquear saqueo) tesoros tripu , saqueo)

















