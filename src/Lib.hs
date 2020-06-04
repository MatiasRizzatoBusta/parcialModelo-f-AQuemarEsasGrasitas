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
bajoPeso calAQuemar gimnasta = gimnasta{peso=(peso gimnasta) -calAQuemar}
