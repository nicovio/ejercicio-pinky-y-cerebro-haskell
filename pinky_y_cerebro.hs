import           Data.List
import           Text.Show.Functions

data Animal = Animal
  { ci :: Integer
  , especie :: String
  , capacidades :: [String]
  } deriving (Show, Eq)

data Experimento = Experimento
  { transformacion :: Animal -> Animal
  , transformacion2 :: Animal -> Animal
  , criterioDeExito :: Animal -> Bool
  } deriving (Show)

-- ANIMALES

pinky = Animal {ci = 172, especie = "raton", capacidades = ["pinkyhablar", "pinkycomer", "destruir el mundo", "hacer planes desalmados"]}

cerebro = Animal {ci = 180, especie = "raton", capacidades = ["hablar","destruir el mundo","pensar"]}

cavallo = Animal {ci = 500, especie = "economista", capacidades = [ "mega devaluar", "crear corralitos", "estafar jubilados", "mentir"]}

menem = Animal {ci = 1400, especie = "diablo", capacidades = ["seducir al pueblo", "ganar 2 elecciones", "vender un pais"]}

dini = Animal {ci = 1750, especie = "semi-dios", capacidades = cycle["ganar", "ser el mejor en todo", "programar"]}

-- EXPERIMENTOS

experimento1 = Experimento {transformacion = pinkyficar, transformacion2 = inteligenciaSuperior 10, criterioDeExito = antropomorfico }
experimento2 = Experimento {transformacion = superpoderes, transformacion2 = inteligenciaSuperior 10, criterioDeExito = antropomorfico }
experimento3 = Experimento {transformacion = inteligenciaSuperior 20, transformacion2 = inteligenciaSuperior 10, criterioDeExito = noTanCuerdo }

--PUNTO 2

inteligenciaSuperior :: Integer -> Animal -> Animal
inteligenciaSuperior incremento animal  = animal { ci = (ci animal) + incremento }

pinkyficar :: Animal -> Animal
pinkyficar animal = animal {capacidades = []}

superpoderes :: Animal -> Animal
superpoderes animal 
    | (especie animal) == "elefante" = agregarCapacidad animal "no tenerle miedo a los ratones"
    | (especie animal) == "raton" && (ci animal) > 100 = agregarCapacidad animal "hablar"
    | otherwise = animal
 
agregarCapacidad :: Animal -> String -> Animal  
agregarCapacidad animal capacidad = animal {capacidades = (capacidad : (capacidades animal)) }

--PUNTO 3

antropomorfico :: Animal -> Bool
antropomorfico animal = tieneCapacidad animal "hablar" && (ci animal) > 180

tieneCapacidad :: Animal -> String -> Bool
tieneCapacidad animal capacidad = any (==capacidad) (capacidades animal)

noTanCuerdo :: Animal -> Bool
noTanCuerdo animal =  length (filter (comienzaCon "pinky") (capacidades animal)) > 1

comienzaCon :: String -> String -> Bool
comienzaCon palabra capacidad = (fst (splitAt (length palabra) capacidad)) == palabra

--PUNTO 4

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento =  (criterioDeExito experimento) . (transformacion2 experimento) . (transformacion experimento)

--PUNTO 5

reporte1 :: [Animal] -> Experimento -> [Integer]
reporte1 animales experimento = map ci (animalesExitosos animales experimento)

animalesExitosos :: [Animal] -> Experimento -> [Animal]
animalesExitosos animales experimento = filter (experimentoExitoso experimento) animales

reporte2 :: [Animal] -> Experimento -> [String]
reporte2 animales experimento =  map especie (genericTake (obtenerMitad (animalesExitosos animales experimento))  (animalesExitosos animales experimento))

obtenerMitad :: [Animal] -> Integer
obtenerMitad animales = round ((genericLength animales)  / 2.0)

reporte3 :: [Animal] -> Experimento -> [Int]
reporte3 animales experimento = map length (map (capacidades) (filter ((=="raton").especie)(animalesExitosos animales experimento)))

