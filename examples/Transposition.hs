import Heqet.Euterpea

-- Definimos la melodía una octava más abajo (octava 2 o 3)
melodia = line [c 5 hn, f 5 hn, g 5 wn, c 5 wn]
melodia_transpuesta = transpose (-12) melodia
score = fromScore ["Oboe" |% (melodia :=: melodia_transpuesta)]

main = putStrLn $ renderScore score
