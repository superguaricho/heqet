{-# LANGUAGE FlexibleInstances, Rank2Types, UndecidableInstances, DeriveDataTypeable #-}

module Heqet.Input.Euterpea.Voices where

import Heqet.Types
import qualified Euterpea as E
import Control.Lens
import Data.Ratio
import Data.Typeable
import Data.List (intersperse)
import Data.Char (toLower)

data VMusic 
    = VSeq     VMusic VMusic
    | VPar     VMusic VMusic
    | VNote    PitchClass Octave Duration
    | VRest    Duration
    | VEmpty
    deriving (Show)

type Voice = VMusic

data Part = Part
    { partName    :: String
    , partVoices  :: [Voice]
    } deriving (Show)

type Score = [Part]

part :: String -> E.Music E.Pitch -> (String, E.Music E.Pitch)
part name m = (name, m)

(|%) :: String -> E.Music E.Pitch -> (String, E.Music E.Pitch)
n |% m = (n, m)
infixr 7 |%

voiceDur :: Voice -> Duration
voiceDur (VSeq v1 v2) = voiceDur v1 + voiceDur v2
voiceDur (VPar v1 v2) = max (voiceDur v1) (voiceDur v2)
voiceDur (VNote _ _ d) = d
voiceDur (VRest d) = d
voiceDur VEmpty = 0

fromVoice :: E.Music E.Pitch -> Voice
fromVoice m = fromVoice' 1 0 m

fromVoice' :: Rational -> E.AbsPitch -> E.Music E.Pitch -> Voice
fromVoice' r t (E.Prim (E.Note dur p)) = 
    let (pc, oct) = E.pitch (E.absPitch p + t)
    in VNote (pcToHeqet pc) oct (dur / r)
fromVoice' r _ (E.Prim (E.Rest dur)) = VRest (dur / r)
fromVoice' r t (m1 E.:+: m2) = VSeq (fromVoice' r t m1) (fromVoice' r t m2)
fromVoice' r t (m1 E.:=: m2) = VPar (fromVoice' r t m1) (fromVoice' r t m2)
fromVoice' r t (E.Modify (E.Transpose dt) m) = fromVoice' r (t + dt) m
fromVoice' r t (E.Modify (E.Tempo dr) m) = fromVoice' (r * dr) t m
fromVoice' r t (E.Modify _ m) = fromVoice' r t m

fromPart :: String -> E.Music E.Pitch -> Part
fromPart name m = Part name [normalizeV (fromVoice m)]

fromScore :: [(String, E.Music E.Pitch)] -> Score
fromScore = map (uncurry fromPart)

pcToHeqet :: E.PitchClass -> PitchClass
pcToHeqet p = case p of
    E.C -> C ; E.Cs -> Cs ; E.D -> D ; E.Ds -> Ds ; E.E -> E ; E.F -> F
    E.Fs -> Fs ; E.G -> G ; E.Gs -> Gs ; E.A -> A ; E.As -> As ; E.B -> B
    E.Cf -> B ; E.Ef -> Ds ; E.Bf -> As ; E.Gf -> Fs ; E.Af -> Gs ; E.Df -> Cs

toLily :: Voice -> String
toLily = toLily' . normalizeV

toLily' :: Voice -> String
toLily' (VSeq v1 v2) = let s1 = toLily' v1; s2 = toLily' v2 in 
    if null s2 then s1 else s1 ++ " " ++ s2
toLily' (VPar v1 v2) = 
    "<< { " ++ toLily' v1 ++ " } \\\\ { " ++ toLily' v2 ++ " } >>"
toLily' (VNote pc oct d) = lilyPitch pc oct ++ lilyDur d
toLily' (VRest d) = if d == 0 then "" else "r" ++ lilyDur d
toLily' VEmpty = ""

lilyPitch :: PitchClass -> Octave -> String
lilyPitch pc oct = 
    let base = case pc of
            C -> "c" ; Cs -> "cis" ; D -> "d" ; Ds -> "dis" ; E -> "e" ; F -> "f"
            Fs -> "fis" ; G -> "g" ; Gs -> "gis" ; A -> "a" ; As -> "ais" ; B -> "b"
        lilyOct = oct - 3  -- Lilypond oct 0 (c) es C3. Euterpea oct 4 es C4 (c').
    in case compare lilyOct 0 of
        EQ -> base  -- C3 (c)
        GT -> base ++ replicate lilyOct '\''  -- octavas arriba: c' (C4), c'' (C5)...
        LT -> base ++ replicate (abs lilyOct) ','  -- octavas abajo: c, (C2), c,, (C1)...

lilyDur :: Duration -> String
lilyDur d 
    | d == 1 % 4 = "4"
    | d == 1 % 2 = "2"
    | d == 1 % 1 = "1"
    | d == 2 % 1 = "\\breve"
    | d == 1 % 8 = "8"
    | d == 1 % 16 = "16"
    | d == 3 % 4 = "2."
    | d == 3 % 8 = "4."
    | otherwise = "4"

normalizeV :: Voice -> Voice
normalizeV (VSeq v1 v2) = VSeq (normalizeV v1) (normalizeV v2)
normalizeV v@(VPar _ _) = normalizePar v
normalizeV v = v

normalizePar :: Voice -> Voice
normalizePar (VPar v1 v2) = VPar (normalizeFill v1 d1 d2) (normalizeFill v2 d2 d1)
  where
    d1 = voiceDur v1
    d2 = voiceDur v2
normalizePar v = v

normalizeFill :: Voice -> Duration -> Duration -> Voice
normalizeFill v _ totalDur
    | dur >= totalDur = v
    | otherwise = VSeq v (VRest rest)
  where
    dur = voiceDur v
    rest = totalDur - dur

clefFromName :: String -> Clef
clefFromName name = let n = map toLower (filter (`elem` ['a'..'z'] ++ ['A'..'Z']) name)
    in case n of
    "violin"    -> Treble
    "viola"     -> Alto
    "cello"    -> Bass
    "bass"      -> Bass
    "contrabass"-> Bass
    "flute"     -> Treble
    "oboe"      -> Treble
    "clarinet"  -> Treble
    "bassoon"   -> Bass
    "trumpet"   -> Treble
    "horn"      -> Treble
    "trombone"  -> Bass
    "tuba"      -> Bass
    "piano"     -> Treble
    "melody"    -> Treble
    "tenor"     -> Treble
    "soprano"   -> Treble
    "alto"      -> Treble
    _           -> Treble

clefToLily :: Clef -> String
clefToLily Treble = "treble"
clefToLily Bass = "bass"
clefToLily Tenor = "tenor"
clefToLily Alto = "alto"
clefToLily _ = "treble"

partToLily :: Part -> String
partToLily part = 
    let clef = clefToLily (clefFromName (partName part))
    in case partVoices part of
        [v] -> "  \\new Staff { \\clef " ++ clef ++ " " ++ toLily v ++ " }"
        vs  -> "  \\new Staff <<\n" 
               ++ unlines (zipWith voiceDef [1..] vs)
               ++ "  >>"
  where
    voiceDef n v = "    \\new Voice { \\voice" ++ show n ++ " " ++ toLily v ++ " }"

scoreTemplate :: String -> String
scoreTemplate parts = unlines
    [ ""
    , "\\version \"2.24.0\""
    , "\\language \"english\""
    , "\\score { <<"
    , parts
    , ">>"
    , "  \\layout { }"
    , "  \\midi { }"
    , "}"
    ]

renderPart :: Part -> String
renderPart p = partToLily p

renderScore :: Score -> String
renderScore parts = scoreTemplate (unlines (map renderPart parts))
