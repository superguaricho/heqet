import Heqet.Euterpea

melodia = line [c 4 hn, f 4 hn, g 4 wn, c 4 wn]
score = fromScore ["Violin" |% melodia]

main = writeFile "Melody.ly" (renderScore score)

-- main =  putStrLn $ renderScore score
