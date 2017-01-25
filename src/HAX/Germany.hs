{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, TypeFamilies
, FlexibleContexts
 #-}

-- | This module imports everything needed for German Tax calculation & Accounting.
module HAX.Germany (
  module HAX.Germany.Einkommensteuer
  ,module HAX.Germany.Gewerbe
  ,module HAX.Germany.NatuerlichePerson
  ,module HAX.Germany.GmbH
  ,module HAX.Germany.Subjekte
) where
import HAX.Germany.Subjekte
import HAX.Germany.NatuerlichePerson
import HAX.Germany.GmbH
import HAX.Germany.Einkommensteuer
import HAX.Germany.Gewerbe
