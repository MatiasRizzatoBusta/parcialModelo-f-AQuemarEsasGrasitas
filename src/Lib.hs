module Lib where
import Text.Show.Functions
laVerdad = True

data Gimnasta = UnGimnasta{
    gimnasta :: String,
    edad :: Float,
    peso :: Float,
    coefTonificacion :: Float
}deriving (Show)

pancho = UnGimnasta "Francisco" 40 120 1
andres = UnGimnasta "Andy" 22 80 6
----------------------------------- Punto 1 -----------------------------------
estaSaludable :: Gimnasta->Bool
estaSaludable gimnasta = (noEsObeso.peso) gimnasta && ((>5).coefTonificacion) gimnasta

noEsObeso :: Float->Bool
noEsObeso  = (>100)
----------------------------------- Punto 2 -----------------------------------
quemarCalorias :: Float->Gimnasta->Gimnasta
quemarCalorias calAQuemar gimnasta |(not.estaSaludable) gimnasta = bajoPeso (calAQuemar/150) gimnasta
                                   |((>30).edad) gimnasta && calAQuemar >200 = bajoPeso 1 gimnasta
                                   |otherwise = bajoPeso (calAQuemar/((peso gimnasta)*(edad gimnasta))) gimnasta

bajoPeso :: Float->Gimnasta->Gimnasta
bajoPeso calAQuemar gimnasta = gimnasta{peso=(peso gimnasta) - calAQuemar}
----------------------------------- Punto 3 -----------------------------------
type Ejercitar = Gimnasta->Gimnasta

calculoCalorias :: Float->Float->Float->Float
calculoCalorias velOInc minutos = (*minutos).(*velOInc)--uso composicion parcial
-- donde velOInc es la velocidad o la inclinacion

caminataEnCinta :: Float->Ejercitar
caminataEnCinta minutos = quemarCalorias (calculoCalorias 5 minutos 1)
--quema 1 cal a una vel constante de 5 cada x minutos

entrenamientoEnCinta :: Float->Ejercitar
entrenamientoEnCinta minutos = quemarCalorias (calculoCalorias (velMax minutos) minutos 1)

velMax :: Float->Float
velMax minutos = 6+(6 + (minutos/5)) --arranca en 6 y le sumo la vel maxima idk copie la formula del ejemplo

pesas :: Float->Float->Ejercitar
pesas peso minutos gimnasta |minutos > 10 = tonifica peso gimnasta
                            |otherwise = gimnasta

tonifica :: Float->Gimnasta->Gimnasta
tonifica peso gimnasta = gimnasta{coefTonificacion = (coefTonificacion gimnasta) + (peso * 0.1)}

colina :: Float->Float->Ejercitar
colina inclinacion minutos = quemarCalorias (calculoCalorias inclinacion minutos 2)

montania :: Float->Float->Ejercitar --Le pongo 10 para cuando llegue a tonifica,aumente la tonificacion en
montania inclinacionInicial minutos = (tonifica 10).(colina (inclinacionInicial + 3) (minutos/2)).(colina inclinacionInicial (minutos/2))
----------------------------------- Punto 4 -----------------------------------
data Rutina = UnaRutina{
    nombre :: String,
    duracionTotal :: Int,
    ejercicios :: [Ejercitar]
}deriving (Show)

rutinaTincho = UnaRutina "rutina tincho" 60 [caminataEnCinta 20,entrenamientoEnCinta 20,pesas 50 20,colina 4 20,montania 4 20]

{-
haceRutina :: Gimnasta->Rutina->Gimnasta
haceRutina gym rutina = aplicoEjercicios (ejercicios rutina) ( (duracionTotal rutina)/(length (ejercicios rutina)) ) gym

aplicoEjercicios :: [Ejercitar]->Float->Gimnasta->Gimnasta
aplicoEjercicios (x:xs) minutos gimnasta =  aplicoEjercicios xs minutos (x minutos gimnasta)

-}

haceRutina :: Gimnasta->Rutina->Gimnasta
haceRutina gimnasta rutina = foldr ($) gimnasta  (ejercicios rutina)

resumenRutina :: Rutina->Gimnasta->(String,Float,Float)
resumenRutina rutina gym = (nombre rutina,totalPesoPerdido gym rutina,calculoIncrementoTonif gym (haceRutina gym rutina ))

calculoIncrementoTonif :: Gimnasta->Gimnasta->Float
calculoIncrementoTonif g1 gf = (coefTonificacion gf) -(coefTonificacion g1)--accedo a= la tonif final y le resto el inicial

totalPesoPerdido :: Gimnasta->Rutina->Float
totalPesoPerdido gym rutina = (peso (haceRutina gym rutina)) - (peso gym)--accedo al peso final y le resto el inicial

----------------------------------- Punto 5 -----------------------------------
cualesDejanSaludable :: [Rutina]->Gimnasta->[Rutina]
cualesDejanSaludable listaRutina gimnasta = filter (estaSaludable .(haceRutina gimnasta)) listaRutina
