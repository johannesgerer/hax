{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, TypeFamilies
, FlexibleContexts
 #-}

-- | This module provides the 'Body' for German GmbH
-- as required to calculate income and business tax.
--
-- 'Body's are defined in "Accounting".
--
-- It uses the 'Auto' type from "Germany.NatuerlichePerson"
module HAX.Germany.GmbH where

import HAX.Accounting
import HAX.Bookkeeping
import HAX.Common
import HAX.Germany.Subjekte
import HAX.Germany.Gewerbe

-- * GmbH ('Body' und 'Gewerbetreibender' Instances)

instance Body GmbH where
    nominalAccounts _ = gewerbeAccounts ++ verlustVortragsKonten ++ 
                        [ verdeckteGewinnausschuettungen
                        , verdeckteEinlagen
                        , giroKonto
                        , gewinnVerlustVortraege
                        ]
    bodyMonthly action = do
      runGewerbe gewerbeMonatlich =<< readBody gGewerbe
      action
      atYearEnd $ do
        kst <- koerperschaftSteuer
        closingTx [ verdeckteGewinnausschuettungen
                  , verdeckteEinlagen
                  , jahresErgebnis
                  ]
          $ BalancingTx "GmbH KSt und Abschluss"
          gewinnVerlustVortraege
          [(giroKonto, -kst)]

instance Gewerbetreibender GmbH where
  gwStFreibetrag _ = 0
  gewerbeGewinn = jahresErgebnis'
  -- nicht abziebares
  
  -- https://www.smartsteuer.de/portal/lexikon/N/Nichtabziehbare-Aufwendungen-gem_-10-KStG.html
  -- bezieht sich glaube ich hauptsächlich auf gewinne/verluste von
  -- santeilen, die die körperschaft an anderen gesellschaften hält.
                    + haben verdeckteGewinnausschuettungen
                    - soll verdeckteEinlagen

-- * Konten

verdeckteGewinnausschuettungen = "vGA"
verdeckteEinlagen = "vElg"
gewinnVerlustVortraege = "GVV"

-- * koerperschaftSteuer
--
-- https://www.smartsteuer.de/portal/lexikon/E/Einkommensermittlung.html
-- 
-- https://www.smartsteuer.de/portal/lexikon/K/Koerperschaftsteuertarif.html
koerperschaftSteuer :: AccountingRW GmbH Amount
koerperschaftSteuer = do
  gewinn <- gwGewinn <$> (runGewerbe gewerbeSteuer =<< readBody gGewerbe )
  -- -abzugsfähige Spenden und Beiträge (§ 9 Abs. 1 Nr. 2 KStG)
  -- -abzugsfähige Großspenden (§ 9 Abs. 1 Nr. 2 Satz 4 und 5 KStG)
  -- +/- Einkommensteuerzurechnung in Organschaftsfällen

  kst <- fixed $ singleResult "KoerperschaftSteuer + Soli" $
         0.15 * (1+solidaritaetsFaktor) * nachVerlustAbzug "KSt" gewinn

  return kst



