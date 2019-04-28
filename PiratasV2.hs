import Text.Show.Functions

--type Tesoro = (Nombre ,Valor)
data Tesoro = UnTesoro {
    nombreTesoro::String,
    valorTesoro::Int }
    deriving Show

type Botin = [ Tesoro ]

--type Pirata = ( Nombre , Botin )
data Pirata = UnPirata {
    nombrePirata::String,
    botinPirata::Botin }
    deriving Show

type Tripulacion = [ Pirata ] 

--type Barco = (Nombre, Tripulacion, Saqueo)
data Barco = UnBarco {
    nombreBarco::String,
    tripulacionBarco::Tripulacion,
    formaDeSaqueoBarco::(Tesoro->Bool) }
    deriving Show

--type Isla = (Nombre, Tesoro)
data Isla = UnaIsla {
    nombreIsla::String,
    tesoroIsla::Tesoro}
    deriving Show

--type Ciudad = (Nombre, Botin)
data Ciudad = UnaCiudad {
    nombreCiudad :: String,
    botinCiudad :: Botin }
    deriving Show


-- Ej de un solo pirata: [ ("Jack Sparrow" , [ ("Arena",0) , ("Tesoro Dorado",10000)] ) ] 
--type Pirata = [ ([Char] , [([Char] ,Int)]) ]
--type Botin = [ ([Char] ,Int) ]
--type Tesoro = ([Char],Int)

--EJEMPLOS

--ejemploBotin = [("Frasco de Arena",5) , ("Brujula",10000)]

--jackSparrow = ("Jack Sparrow" , [("Frasco de Arena",0) , ("Brujula",10000)])
--davidJones = ("David Jones" , [("Cajita musical",1)])
--anneBonny = ("Anne Bonny" ,[("Doblones",100) , ("Frasco de Arena",1)])
--elizabethSwann = ("Elizabeth Swann",[("Moneda del Cofre Muerto",100), ("Espada de hierro",50)])
--willTurner = ("Will Turner", [("Cuchillo",5)])

--perlaNegra = ("Perla Negra", [jackSparrow, anneBonny], saqueoValioso)
--holandesErrante = ("Holandes Errante", [davidJones], saqueoConCorazon)

--islaTortuga = ("Isla Tortuga", ("Frasco de Arena", 1))
--islaDelRon = ("Isla del Ron", ("Botella de Ron", 25))

ejemploBotin = [(UnTesoro "Frasco de Arena" 5) , (UnTesoro "Brujula" 10000)]

jackSparrow = (UnPirata "Jack Sparrow" [(UnTesoro "Frasco de Arena" 0) , (UnTesoro "Brujula" 10000)])
davidJones = (UnPirata "David Jones" [(UnTesoro "Cajita musical" 1)])
anneBonny = (UnPirata "Anne Bonny" [(UnTesoro "Doblones" 100) , (UnTesoro "Frasco de Arena" 1)])
elizabethSwann = (UnPirata "Elizabeth Swann" [(UnTesoro "Moneda del Cofre Muerto" 100), (UnTesoro "Espada de hierro" 50)])
willTurner = (UnPirata "Will Turner" [(UnTesoro "Cuchillo" 5)])

--perlaNegra = (UnBarco "Perla Negra" [jackSparrow, anneBonny] saqueoValioso)
--holandesErrante = (UnBarco "Holandes Errante" [davidJones] saqueoConCorazon)

islaTortuga = (UnaIsla "Isla Tortuga" (UnTesoro "Frasco de Arena" 1))
islaDelRon = (UnaIsla "Isla del Ron" (UnTesoro "Botella de Ron" 25))

-- TESOROS PIRATAS

--Funcion de cantidad de tesoros
cantidadDeTesoros :: Pirata -> Int --Cantidad de tesoros que tiene el botin de un pirata
cantidadDeTesoros unPirata = length (botinPirata unPirata)

--Funciones de Pirata Afortunado
valorDelTesoro :: Tesoro -> Int --Obtiene el valor de un tesoro
valorDelTesoro unTesoro = valorTesoro unTesoro

valoresDelBotin :: Botin -> [Int] --Arma una lista solo con los valores de cada tesoro del botin
valoresDelBotin unBotin = map valorDelTesoro unBotin

valorTotalDelBotin :: Botin -> Int --Valor total al sumar todos los botines
valorTotalDelBotin botin = sum (valoresDelBotin botin)

esAfortunado :: Pirata -> Bool --Chequea si el pirata es o no afortunado (botin total > 10000)
esAfortunado unPirata = valorTotalDelBotin (botinPirata unPirata) > 10000

--Funcion de mismo tesoro con valor diferente

esMismoTesoroConValorDistinto :: Tesoro -> Tesoro -> Bool --Compara los nombres de dos tesoros y los valores
esMismoTesoroConValorDistinto unTesoro otroTesoro = ((nombreTesoro unTesoro) == (nombreTesoro otroTesoro)) && ((valorTesoro unTesoro) /= (valorTesoro otroTesoro))

loTieneOtroPirataConDistintoValor :: Pirata -> Tesoro -> Bool
loTieneOtroPirataConDistintoValor unPirata unTesoro = any (esMismoTesoroConValorDistinto unTesoro) (botinPirata unPirata)

tienenElMismoTesoroConValorDiferente :: Pirata -> Pirata -> Bool
tienenElMismoTesoroConValorDiferente  unPirata otroPirata = any (loTieneOtroPirataConDistintoValor otroPirata) (botinPirata unPirata)

--Funcion de Tesoro mas valioso
masValiosoTesoro :: Pirata -> Int --Valor mas alto entre todos los tesoros del botin de un pirata
masValiosoTesoro unPirata = maximum (valoresDelBotin (botinPirata unPirata))

--Funciones de ganar o perder tesoros
nuevoTesoro :: Pirata -> Tesoro -> Pirata --Agrega un tesoro al botin
--nuevoTesoro unPirata unTesoro = (UnPirata (nombrePirata unPirata) unTesoro:(botinPirata unPirata))
--no funca la ultima







