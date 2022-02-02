{-
Nombre: Chalco,Aylen
Legajo: 167778-0
-}

module Lib where
import Text.Show.Functions

laVerdad = True

{-En el sistema vamos a trabajar con series, de las cuales queremos saber cuál es el
nombre de la misma, quiénes actúan en ella (y el orden de importancia), su
presupuesto anual, cantidad de temporadas estimadas, el rating promedio que tiene
y si está cancelada o no.
También, de cada actor o actriz conocemos el nombre, cuál es su sueldo
pretendido (anual) y qué restricciones tiene a la hora de trabajar.
Por ejemplo ​ , sabemos que el sueldo pretendido de Paul Rudd es de 41 millones al
año y que sus restricciones son ​ "no actuar en bata"​ y ​ "comer ensalada de
rucula todos los dias"​ .-}
type NombreSerie = String
type Actores = [Actor]
type Presupuesto = Int
type Temporadas = Int
type Rating = Int

data Serie = UnaSerie {
	nombreSerie :: NombreSerie,
	actores :: Actores,
	presupuestoAnual :: Presupuesto,
	cantidadTemporadas :: Temporadas,
	ratingPromedio :: Rating,
	cancelada :: Bool

} deriving (Show,Eq)


type NombreActor = String
type Sueldo = Int
type Restriccion = String

data Actor = UnActor {
	nombreActor:: NombreActor,
	sueldo :: Sueldo,
	restricciones :: [Restriccion]

} deriving (Show,Eq)

------Punto 1------

{-a. Si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo que quieren cobrar
todos los actores.-}

estaEnRojo :: Serie -> Bool
estaEnRojo = not.cubreSueldos 

cubreSueldos :: Serie -> Bool
cubreSueldos serie = presupuestoAnual serie >= sueldoActores serie

sueldoActores :: Serie -> Int
sueldoActores = sum.map sueldo.actores


{-b. Si una serie es problemática, esto ocurre si tienen más de 3 actores o actrices con más de 1
restricción-}


esProblematica :: Serie -> Bool
esProblematica = (>3).cantidadActoresConRestriccionesMayorA 1

cantidadActoresConRestriccionesMayorA :: Int -> Serie -> Int
cantidadActoresConRestriccionesMayorA numero = length.filter (tieneRestriccionesMayorA numero).actores

tieneRestriccionesMayorA :: Int -> Actor -> Bool
tieneRestriccionesMayorA numero =(>numero).length.restricciones

------Punto 2------

type Productor = Serie -> Serie

{-a. con favoritismos​ : elimina a los dos primeros actores de la serie y los reemplaza por sus
actores favoritos.-}

favoritisimos :: Actores -> Productor
favoritisimos actoresFavoritos serie = serie{actores = reemplazaActores actoresFavoritos (eliminaPrimeros 2 (actores serie))}

eliminaPrimeros :: Int -> Actores -> Actores
eliminaPrimeros numero actores = undefined

reemplazaActores :: Actores -> Actores -> Actores
reemplazaActores actoresFavoritos = (actoresFavoritos ++)


{-b. tim burton​ : es un caso particular de un productor con favoritismos, siempre reemplaza a los
primeros dos actores por johnny depp y helena bonham carter, cuyos sueldos pretendidos
anuales son $20000000 y $15000000 respectivamente, y no tienen ninguna restricción.-}

johnnyDeep :: Actor
johnnyDeep = UnActor{
	nombreActor = "Johnny Deep",
	sueldo = 20000000,
	restricciones= []
}

helenaBonhamCarter :: Actor
helenaBonhamCarter = UnActor {
	nombreActor = "Helena Bonham Carter",
	sueldo = 15000000,
	restricciones= []
}

timBurton :: Productor
timBurton = favoritisimos [johnnyDeep,helenaBonhamCarter]


{-c. gatopardeitor​ : no cambia nada de la serie.-}

gatoPardeitor :: Productor
gatoPardeitor = id


{-d. estireitor​ : duplica la cantidad de temporadas estimada de la serie.-}

estireitor :: Productor
estireitor serie = serie {cantidadTemporadas = multiplicarTemporadasPor 2 (cantidadTemporadas serie)}

multiplicarTemporadasPor:: Int -> Temporadas -> Temporadas
multiplicarTemporadasPor numero = (*numero)

{-e. desespereitor​ : hace un combo de varios productores.-}

desespereitor:: Productor
desespereitor = componer []

componer [] = id
componer (x:xs) = x . componer xs

{-f. canceleitor​ : si la serie está en rojo o el rating baja de una cierta cifra, la serie se cancela.-}

canceleitor :: Rating -> Productor
canceleitor rating serie = serie {cancelada = criterioDeCancelacion estaEnRojo serie || criterioDeCancelacion (ratingDebajoDe rating) serie} 

criterioDeCancelacion :: (Serie -> Bool) -> Serie -> Bool
criterioDeCancelacion f serie = f serie

ratingDebajoDe:: Rating -> Serie-> Bool
ratingDebajoDe numero =(< numero).ratingPromedio 


------Punto 3------

{-Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:

- Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario es (4 - cantidad
de temporadas estimadas) * 2

- Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 - cantidad de actores
que tienen restricciones).

Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más allá de cómo diesen
el bienestar por longitud y por reparto.-}


bienestar :: Serie -> Int
bienestar serie | estaCancelada serie = 0
				| otherwise = calculodeBienestar serie

estaCancelada :: Serie -> Bool
estaCancelada = cancelada

calculodeBienestar :: Serie -> Int
calculodeBienestar serie = sum [bienestarPorLongitud serie, bienestarPorReparto serie]

bienestarPorLongitud :: Serie -> Int 
bienestarPorLongitud serie | tieneTemporadasMayorA 4 serie = 5
						   | otherwise = calculoPorLongitud serie

tieneTemporadasMayorA :: Int -> Serie -> Bool
tieneTemporadasMayorA numero = (>numero).cantidadTemporadas

calculoPorLongitud :: Serie -> Int 
calculoPorLongitud serie = (4 - cantidadTemporadas serie) * 2

bienestarPorReparto :: Serie -> Int
bienestarPorReparto serie | tienenActoresMenorA 10 serie = 3
						  | otherwise = calculoPorReparto serie

tienenActoresMenorA :: Int -> Serie -> Bool
tienenActoresMenorA numero = (<numero).length.actores

calculoPorReparto :: Serie -> Int
calculoPorReparto serie = 10 - cantidadActoresConRestriccionesMayorA 0 serie


------Punto 4------

{-4. Dada una lista de series y una lista de productores, 
aplicar para cada serie el productor que la haga más efectiva: es decir, el que le deja más bienestar.-}

aplicarProductoresEfectivos :: [Productor] ->[Serie]-> [Serie]
aplicarProductoresEfectivos productores = map.(flip aplicarQuienDejaMasBienestar) productores 

aplicarQuienDejaMasBienestar :: Serie -> [Productor]-> Serie
aplicarQuienDejaMasBienestar serie = aplicarProductor serie.dejaMayorBienestar serie

aplicarProductor :: Serie -> Productor -> Serie
aplicarProductor serie productor = productor serie

dejaMayorBienestar :: Serie -> [Productor] -> Productor
dejaMayorBienestar serie productores =  foldl1 (elMejorDeDosProductores serie) productores

elMejorDeDosProductores :: Serie -> Productor -> Productor -> Productor
elMejorDeDosProductores serie prod1 prod2 | bienestarQueDeja prod1 serie >= bienestarQueDeja prod2 serie = prod1
										  | otherwise = prod2 

bienestarQueDeja :: Productor -> Serie -> Int
bienestarQueDeja productor = bienestar.productor


{-dejaMayorBienestar :: Serie -> [Productor] -> Productor
dejaMayorBienestar serie productores = foldl (bienestarMayorEntre productores) productores

bienestarMayorEntre :: [Productor] -> Productor -> Productor -> Productor
bienestarMayorEntre productores prod1 prod2 | bienestarQueDeja prod1 productores >= bienestarQueDeja prod2 productores = prod1
										    | otherwise = prod2 -}

------Punto 5------

--a. Construir la serie ​ twd​ , que tiene actores infinitos (inventar los datos).

twd = UnaSerie {
	nombreSerie = "twd",
	actores = [helenaBonhamCarter:repeat johnnyDeep],
	presupuestoAnual = 10,
	cantidadTemporadas = 3,
	ratingPromedio =10,
	cancelada = True
}

{-b. ¿Puede Haskell dar un resultado si a ​ twd​ la agarra ​ tim burton​ ? Justificar conceptualmente.
	Si, Ya que tim burton, solo tomara los primeros 2 actores, los reemplazara, y luego seguira mostrando 
	la lista de actores, si bien esta lista es infinita, tim burton hara su funcion, y haskell podra resolverlo
	gracias a lazzy evaluation-}


{-c. ¿Puede Haskell dar un resultado si a ​ twd​ la agarra ​ canceleitor​ ? Justificar conceptualmente.
	Si,(De acuerdo al codigo que yo hice)
	Por mas que la lista de actores sea infinita, y por ende sus sueldos tambien, en este caso nunca terminaria
	de sumar los sueldos y ver si el presupuesto de la serie alcanza a pagarlos. Pero aunque esta condicion nunca se resolveria,
	si el rating de la serie es menor a cierto numero, con esto ya nos daria un True, y la serie se cancelaria. -}


------Punto 6------

{-6. Saber si una serie es controvertida
es cuando no se respeta que los más importantes cobren más que los menos importantes. 
Recordar que los actores de la serie están ordenados por importancia de mayor a menor
(Cuando el que esta a la izquierda cobra menos que el que esta a la derecha) -}

esControvertida :: Serie -> Bool
esControvertida = not.seRespetanLosCobros

seRespetanLosCobros :: Serie -> Bool
seRespetanLosCobros = sueldosOrdenadosdeMayoraMenor.actores

sueldosOrdenadosdeMayoraMenor :: Actores -> Bool
sueldosOrdenadosdeMayoraMenor = estanOrdenados.map sueldo

estanOrdenados :: [Sueldo] -> Bool
estanOrdenados [] = True
estanOrdenados (x:y:xs) = (x<=y) && estanOrdenados (y:xs)

