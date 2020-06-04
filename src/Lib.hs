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
type Ejercitar = Float->Gimnasta->Gimnasta

caminataEnCinta :: Ejercitar
caminataEnCinta minutos = quemarCalorias (1*5*minutos)
--quema 1 cal a una vel constante de 5 cada x minutos

entrenamientoEnCinta :: Ejercitar
entrenamientoEnCinta minutos = quemarCalorias (1*((velMax minutos)/2)*minutos)

velMax :: Float->Float
velMax minutos = 6+(6 + (minutos/5)) --arranca en 6 y le sumo la vel maxima idk copie la formula del ejemplo

pesas :: Float->Ejercitar
pesas peso minutos gimnasta |minutos > 10 = tonifica peso gimnasta
                            |otherwise = gimnasta

tonifica :: Float->Gimnasta->Gimnasta
tonifica peso gimnasta = gimnasta{coefTonificacion = (coefTonificacion gimnasta) + (peso * 0.1)}

colina :: Float->Ejercitar
colina inclinacion minutos = quemarCalorias (2*minutos*inclinacion)

montania :: Float->Ejercitar --Le pongo 10 para cuando llegue a tonifica,aumente la tonificacion en
montania inclinacionInicial minutos = (tonifica 10).(colina (inclinacionInicial+3) (minutos/2)).(colina inclinacionInicial (minutos/2))


