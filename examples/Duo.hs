module Main where

import Heqet.Euterpea

melody :: Music Pitch
melody = line [e 5 hn, d 5 hn, c 5 hn, b 4 hn, c 5 wn]

bassPart :: Music Pitch
bassPart = line [c 3 hn, f 3 hn, g 3 wn, c 3 wn]

main :: IO ()
main = do
    let score = fromScore 
            [ "Violin" |% melody
            , "Cello"  |% bassPart
            ]
    writeFile "Duo.ly" (renderScore score)
    putStrLn "Generado Duo.ly"
