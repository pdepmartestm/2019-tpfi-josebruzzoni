import Text.Show.Functions
type Nombre = [Char]
type Staff = Int
type Salado = Int
type Ingredientes = Int
type Temperatura = Int
type Comida = (Nombre,Salado,Ingredientes,Temperatura)
type Comidas = [Comida]
type Restaurante = (Nombre,Staff,Comidas)
type Restaurantes = [Restaurante]
type Condicion = (Restaurante -> Bool)
type Critico = (Nombre,Condicion)

resto1= ("Parolaccia", 32, [("pollo al verdeo",5,4,50),("arroz con atun",34,5,10),("ensalada rusa",5,33,3)])

antonEgo = ("Anton Ego", condicionAntonEgo)
colleteTatou = ("Collete Tatou", condicionColleteTatou)
skinner = ("Skinner", condicionSkinner)


--Funciones auxiliares

obtenerSalado :: Comida -> Salado
obtenerSalado (nombre,salado,ingredientes,temp) = salado

obtenerIngredientes :: Comida -> Ingredientes
obtenerIngredientes (nom,salado,ingredientes,temp) = ingredientes

listaDeSalado :: Comidas -> [Salado]
listaDeSalado comidas = map obtenerSalado comidas

listaDeIngredientes :: Comidas -> [Ingredientes]
listaDeIngredientes comidas = map obtenerIngredientes comidas

cantidadDeSalTotal :: Comidas -> Int
cantidadDeSalTotal comidas = sum (listaDeSalado comidas)

comidaEsSalada :: Comida -> Bool
comidaEsSalada (nom,salado,ingredientes,temp) = salado > 10

comidaEsCaliente :: Comida -> Bool
comidaEsCaliente (nom,salado,ingredientes,temp) = temp > 40

--Condiciones

condicionAntonEgo :: Restaurante -> Bool
condicionAntonEgo (nombre,staff,comidas) = (cantidadDeSalTotal comidas) < 30

condicionColleteTatou :: Restaurante -> Bool
condicionColleteTatou (nombre,staff,comidas) = (all (comidaEsSalada) comidas) && (all (comidaEsCaliente) comidas)

condicionSkinner :: Restaurante -> Bool
condicionSkinner (nombre,staff,comidas) = all (>staff) (listaDeIngredientes comidas)

--Funcion para obtener lista

obtenerLista :: Critico -> Restaurantes -> Restaurantes
obtenerLista (nombre,condicion) restos = filter condicion restos
