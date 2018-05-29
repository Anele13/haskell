
import Text.Show.Functions

import Data.List




data TipoCliente = Cliente{nombre :: String, resistencia :: Int, amigos :: [TipoCliente], bebidas:: [Bebida]} deriving (Show)

type Bebida = TipoCliente-> TipoCliente
type Accion = Bebida


rodri = Cliente "rodri" 55 [] [tomar_tintico]
marcos = Cliente "marcos" 40 [rodri] [(tomar_klusener "guinda")]
cristian = Cliente "cristian" 2 [] [tomar_grogXd, tomar_jarraLoca]
ana = Cliente "ana" 120 [marcos, rodri] [tomar_jarraLoca]

--instance Show (a->b) where  show tomar_tintico = "Tintico"



agregar_amigo:: TipoCliente -> TipoCliente -> TipoCliente
agregar_amigo amigo cliente 
   | ((nombre cliente) == (nombre amigo)) || (any (((==) (nombre amigo)).nombre) (amigos cliente)) = cliente
   | otherwise = cliente {amigos = amigo:(amigos cliente)}


comoesta:: TipoCliente -> String
comoesta cliente | (resistencia cliente)>50 = "Fresco" | (length (amigos cliente)) > 1 = "Piola" | otherwise = "Duro"

bajarresistencia::Int -> TipoCliente -> TipoCliente
bajarresistencia reduccion cliente = cliente {resistencia = (resistencia cliente) - reduccion}


sumar:: Int -> TipoCliente -> Int
sumar valor _ = valor + 5



-- TRAGOS

tomar_grogXd:: TipoCliente -> TipoCliente
tomar_grogXd cliente = cliente {resistencia = 0}

tomar_jarraLoca:: TipoCliente -> TipoCliente
tomar_jarraLoca cliente = bajarresistencia 10 cliente {amigos = map (bajarresistencia 10) (amigos cliente)}

tomar_klusener:: String ->TipoCliente -> TipoCliente
tomar_klusener sabor cliente = bajarresistencia (length sabor) cliente

tomar_tintico:: TipoCliente -> TipoCliente
tomar_tintico cliente = cliente{resistencia= (resistencia cliente) + foldl sumar 0 (amigos cliente)}

tomar_soda:: Int -> TipoCliente -> TipoCliente
tomar_soda fuerza cliente = cliente{nombre=  "e"++ (take fuerza (cycle "r"))++"p"++(nombre cliente)}




rescatarse:: Int -> TipoCliente-> TipoCliente
rescatarse horas cliente | horas > 3 = cliente{resistencia = (resistencia cliente) + 200} | otherwise = cliente{resistencia = (resistencia cliente) + 100}











----------------------------------------------------------  PARTE II -----------------------------------------------------

-------  OBJETIVO 1 ---------------
--tomar una bebida
beber:: (Bebida) ->  TipoCliente  -> TipoCliente
beber f cliente  = (f cliente){bebidas= (bebidas cliente)++[f]}

--tomar tragos
tomarTragos:: [Bebida] -> TipoCliente -> TipoCliente
tomarTragos lista_tragos cliente = foldr beber cliente lista_tragos

-- dame otro trago
dameOtro:: TipoCliente -> TipoCliente
dameOtro cliente = beber (last (bebidas cliente)) cliente  



-------  OBJETIVO 2 ---------------
-- cuales puede tomar
cliente_hace:: TipoCliente -> (Accion) -> TipoCliente
cliente_hace cliente f  = f cliente


bebidasPermitidas:: TipoCliente-> Bebida -> Bool
bebidasPermitidas cliente bebida =  0 < (resistencia (bebida cliente))


cualesPuedeTomar :: TipoCliente -> [Bebida] -> [Bebida]
cualesPuedeTomar cliente lista_tragos = filter (bebidasPermitidas cliente) lista_tragos


cuantasPuedeTomar:: TipoCliente -> [Bebida] -> Int
cuantasPuedeTomar cliente lista_tragos = genericLength (cualesPuedeTomar cliente lista_tragos)


-------  OBJETIVO 3 ---------------
-- Itinerarios
data TipoItinerario = Itinerario{nombre_itinerario :: String, duracion_estimada :: Float, acciones :: [(Accion)]} deriving (Show)

robertoCarlos = Cliente "Roberto Carlos" 165 [] []

mezclaExplosiva = Itinerario{nombre_itinerario="Mezcla Explosiva",duracion_estimada= 2.5, acciones = [beber tomar_grogXd,beber tomar_grogXd, beber (tomar_klusener "huevo"), beber (tomar_klusener "frutilla")]}
itinerarioBasico = Itinerario{nombre_itinerario="Basico",duracion_estimada= 5, acciones = [beber tomar_jarraLoca, beber (tomar_klusener "chocolate"), (rescatarse 3), beber (tomar_klusener "huevo")]}
salidaDeAmigos = Itinerario{nombre_itinerario="Salida de amigos", duracion_estimada=1, acciones = [beber (tomar_soda 1), beber tomar_tintico, (agregar_amigo robertoCarlos), beber tomar_jarraLoca]}


-------  OBJETIVO 4 ---------------
-- conocer intensidad de itinerario
intensidad:: TipoItinerario -> Float
intensidad itinerario = genericLength(acciones itinerario) / (duracion_estimada itinerario)


hacerItinerario:: TipoItinerario -> TipoCliente -> TipoCliente
hacerItinerario itinerario cliente = foldl cliente_hace cliente (acciones itinerario) 


compararIntensidad:: TipoItinerario -> TipoItinerario -> TipoItinerario
compararIntensidad iViejo iNuevo = if intensidad iViejo > intensidad iNuevo then iViejo else iNuevo

buscarMasInteso::[TipoItinerario] -> TipoItinerario
buscarMasInteso l = foldr compararIntensidad (head l) (tail l)

hacerItinerarioMasIntenso:: [TipoItinerario] -> TipoCliente -> TipoCliente
hacerItinerarioMasIntenso lista_itinerarios cliente = hacerItinerario (buscarMasInteso lista_itinerarios) cliente

{-
itinerario:: TipoCliente->TipoCliente
itinerario cliente = (tomar_klusener "huevo" (rescatarse 3 (tomar_klusener "chocolate" (tomar_jarraLoca cliente))))
-}

-------  OBJETIVO 5 ---------------

concatenar:: [TipoCliente] -> TipoCliente -> [TipoCliente]
concatenar ltotal amigo = ltotal ++ amigos amigo


amigosDeAmigos:: Int -> [TipoCliente]-> [TipoCliente]
amigosDeAmigos 1 lamigos = foldl concatenar [] lamigos
--amigosDeAmigos n lamigos = amigosDeAmigos (n-1) (foldl concatenar [] lamigos)

amigosDeAmigos n lamigos = (foldl concatenar [] lamigos)++(amigosDeAmigos (n-1) (foldl concatenar [] lamigos))


tomar_jarra_popular:: Int -> TipoCliente -> TipoCliente
tomar_jarra_popular 0 cliente = cliente
tomar_jarra_popular n cliente = foldr agregar_amigo cliente (amigosDeAmigos n (amigos cliente))


-------- PRUEBAS ---------------
{-
3B) Rodri hace una salida de amigos y debe quedar con un amigo


hacerItinerario salidaDeAmigos rodri

Cliente {nombre = "erpRoberto Carlos", resistencia = 170, amigos = [Cliente {nombre = "rodri", resistencia = 45, amigos = [], bebidas = [<function>]}], bebidas = []}








-}
