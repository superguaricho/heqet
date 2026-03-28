{-# LANGUAGE FlexibleInstances, Rank2Types, UndecidableInstances, DeriveDataTypeable, TypeSynonymInstances #-}
module Heqet.Euterpea (
    module Euterpea,
    module Heqet.Input.Euterpea.Voices,
    toPart
) where

import Euterpea
import Heqet.Input.Euterpea.Voices

-- | Alias para crear una parte con nombre, compatible con el flujo de Heqet
toPart :: String -> Music Pitch -> (String, Music Pitch)
toPart = part
