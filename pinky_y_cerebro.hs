import           Data.List
import           Text.Show.Functions

data Animal = Animal
  { ci :: Integer
  , especie :: String
  , capacidades :: [Capacidad]
  } deriving (Show, Eq)

data Experimento = Experimento
  { transformacion :: Transformacion
  , transformacion2 :: Transformacion
  , criterioDeExito :: CriterioExito
  } deriving (Show)

--Types

type Capacidad = String

type Transformacion = Animal -> Animal

type CriterioExito = Animal -> Bool

-- ANIMALES

pinki = Animal {ci = 172, especie = "raton", capacidades = ["pinkihablar", "pinkicomer", "pinkipensar", "destruir el mundo", "hacer planes desalmados"]}

cerebro = Animal {ci = 180, especie = "raton", capacidades = ["hablar","destruir el mundo","pensar"]}

cavallo = Animal {ci = 500, especie = "economista", capacidades = [ "mega devaluar", "crear corralitos", "estafar jubilados", "mentir"]}

menem = Animal {ci = 1400, especie = "diablo", capacidades = ["seducir al pueblo", "ganar 2 elecciones", "vender un pais"]}

dini = Animal {ci = 1750, especie = "semi-dios", capacidades = repeat "pinkisoy"}

-- EXPERIMENTOS

experimento1 = Experimento {transformacion = pinkificar, transformacion2 = inteligenciaSuperior 10, criterioDeExito = antropomorfico }
experimento2 = Experimento {transformacion = superpoderes, transformacion2 = inteligenciaSuperior 10, criterioDeExito = antropomorfico }
experimento3 = Experimento {transformacion = inteligenciaSuperior 20, transformacion2 = inteligenciaSuperior 10, criterioDeExito = noTanCuerdo }

--PUNTO 2

inteligenciaSuperior :: Integer -> Transformacion
inteligenciaSuperior incremento animal  = animal { ci = (ci animal) + incremento }

pinkificar :: Transformacion
pinkificar animal = animal {capacidades = []}

superpoderes :: Transformacion
superpoderes animal 
    | esDeEspecie "elefante" animal = agregarCapacidad "no tenerle miedo a los ratones" animal
    | esDeEspecie "raton" animal && (ci animal) > 100 = agregarCapacidad "hablar" animal 
    | otherwise = animal

esDeEspecie :: String -> Animal -> Bool    
esDeEspecie unaEspecie animal = unaEspecie == (especie animal) 

agregarCapacidad :: Capacidad -> Transformacion  
agregarCapacidad capacidad animal  = animal {capacidades = (capacidad : (capacidades animal)) }

--PUNTO 3

antropomorfico :: CriterioExito
antropomorfico animal = tieneCapacidad "hablar" animal  && (ci animal) > 60

tieneCapacidad :: Capacidad -> CriterioExito
tieneCapacidad capacidad animal  = elem capacidad (capacidades animal)

noTanCuerdo :: CriterioExito
noTanCuerdo =  (tieneMasQue 2) . (capacidadesPinkieskas) 

capacidadesPinkieskas :: Animal -> [Capacidad]
capacidadesPinkieskas animal = filter (comienzaCon "pinki") (capacidades animal)

tieneMasQue :: Int -> [Capacidad] -> Bool
tieneMasQue cantidad capacidades = (length capacidades) > cantidad

comienzaCon :: String -> Capacidad -> Bool
comienzaCon palabra capacidad = (partirCapacidad palabra capacidad) == palabra

partirCapacidad :: String -> Capacidad -> Capacidad
partirCapacidad palabra capacidad = take (length palabra) capacidad

--PUNTO 4

experimentoExitoso :: Experimento -> CriterioExito
experimentoExitoso experimento =  (criterioDeExito experimento) . (transformacion2 experimento) . (transformacion experimento)

--PUNTO 5

coeficienteAnimalesExitosos :: [Animal] -> Experimento -> [Integer]
coeficienteAnimalesExitosos animales experimento = map ci (animalesExitosos animales experimento)

animalesExitosos :: [Animal] -> Experimento -> [Animal]
animalesExitosos animales experimento = filter (experimentoExitoso experimento) animales

especiePrimerMitadAnimalesExitosos :: [Animal] -> Experimento -> [String]
especiePrimerMitadAnimalesExitosos animales =  obtenerEspecies . obtenerMitad . (animalesExitosos animales) 

obtenerEspecies :: [Animal] -> [String]
obtenerEspecies animales = map especie animales

obtenerMitad :: [Animal] -> [Animal]
obtenerMitad animales = take (calcularMitad animales) animales

calcularMitad :: [Animal] -> Int
calcularMitad animales = (length animales) `div` 2

capacidadesAnimalesExitososYRatones :: [Animal] -> Experimento -> [Int]
capacidadesAnimalesExitososYRatones animales = listaSumaCapacidades . obtenerCapacidades . (animalesExitososYRatones animales)

obtenerCapacidades :: [Animal] -> [[Capacidad]]
obtenerCapacidades animales = map capacidades animales

listaSumaCapacidades :: [[Capacidad]] -> [Int]
listaSumaCapacidades capacidades = map length capacidades

animalesExitososYRatones :: [Animal] -> Experimento -> [Animal]
animalesExitososYRatones animales experimento = filter (esExitosoYRaton experimento) (animales)

esExitosoYRaton :: Experimento -> Animal -> Bool
esExitosoYRaton experimento animal =  (experimentoExitoso experimento animal) && (esDeEspecie "raton" animal)