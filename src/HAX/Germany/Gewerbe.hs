{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, TypeFamilies
, FlexibleContexts
 #-}

-- | This module provides the Gewerbesteuer calculations and
-- 'Gewerbetreibender' type class
--
-- auch interessante Übersicht zum Gesamtverständnis beitragend:
-- http://de.wikipedia.org/wiki/Schachtelprivileg
--
-- http://de.wikipedia.org/wiki/Zinsschranke
-- Zinsschranke greift erst bei 3 Mio negativem Zinssaldo
module HAX.Germany.Gewerbe where

import HAX.Accounting
import HAX.Bookkeeping
import HAX.Common
import Control.Monad.Reader
import HAX.Germany.Subjekte

-- * Gewerbetreibender

class Gewerbetreibender gt where
  gwStFreibetrag :: gt -> Amount
  -- | Nach den Vorschriften des EStG oder KStG ermittelte Gewinn
  --              
  -- https://www.smartsteuer.de/portal/lexikon/E/Einkommensermittlung.html
  -- https://www.smartsteuer.de/portal/lexikon/Z/Zinsschranke.html
  gewerbeGewinn :: AccountingReadOnly (Gewerbe gt) Amount


-- * Types

type AccGW treib l w = Acc (Gewerbe treib) l w
type BetragGW treib l w = Acc (Gewerbe treib) l w Amount

type AccGWRW treib = AccountingRW (Gewerbe treib)
type BetragGWRW treib = AccountingRW (Gewerbe treib) Amount

type AccGWRO treib = AccountingReadOnly (Gewerbe treib)
type BetragGWRO treib = AccountingReadOnly (Gewerbe treib) Amount

-- * Konten

sonstigerUeberschuss     = "jUeb"
mieteUnbeweglich = "MiU"
mieteBeweglich   = "MiB"
schuldZinsen       = "Zins"
jahresErgebnis  = "jErg"


gewerbeAccounts = [jahresErgebnis] ++ gewerbeErgebnisAccounts
gewerbeErgebnisAccounts = [sonstigerUeberschuss
                          , mieteUnbeweglich
                          , mieteBeweglich
                          , schuldZinsen
                          ]

-- * Gewerbesteuer

jahresErgebnis' = sum <$> mapM haben gewerbeErgebnisAccounts
                 
                   
-- https://www.smartsteuer.de/portal/lexikon/E/Einkuenfte-aus-Gewerbebetrieb.html
-- auch gut? http://www.recht-finanzen.de/contents/1369-einkommensteuer-verlustrucktrag-und-verlustvortrag

data GewerbeSteuer = GWSt { gwGewinn :: Amount
                          , gwMessbetrag :: Amount
                          , gwSteuer :: Amount
                          }

instance Show GewerbeSteuer where
  show (GWSt g m s) = printf "GWSt {gwGewinn = %v, gwMessbetrag = %v, gwSteuer = %v}"
                      g m s
                              
keineGewerbeSteuer = GWSt 0 0 0 

gwMesszahl = 0.035


-- | Bemessungsgrundlage für die GewSt, ist der Gewinn
-- zuzügl. Hinzurechnungen (§ 8 GewStG) und abzügl. Kürzungen (§ 9
-- GewStG)
--
-- https://www.smartsteuer.de/portal/lexikon/G/Gewerbeertrag.html#D063040100012
gewerbeErtrag :: Gewerbetreibender treib => Amount -> BetragGWRO treib
gewerbeErtrag gewinn = singleResult "Gewerbeertrag" $ withBody $ \g -> do
  ertrag <- return gewinn
            + hinzuRechnungen
            - kuerzungen
  abrundenAuf100 <$> nachVerlustAbzug "GewSt" ertrag

abrundenAuf100 x = fromInteger $(100*) $ floor $ x/100
  
-- | Hinzurechnungen (§ 8 GewStG)
--
-- http://de.wikipedia.org/wiki/Gewerbesteuer_(Deutschland)#Hinzurechnungen
hinzuRechnungen :: BetragGWRO treib 
hinzuRechnungen = singleResult "Hinzurechnungen" $ do
  summe <- soll schuldZinsen
           + 0.5 * soll mieteUnbeweglich
           + 0.2 * soll mieteBeweglich
  return $ 0.25 * positivePart (summe - 100000)

-- | Kürzungen (§ 9 GewStG)
kuerzungen :: BetragGWRO treib 
kuerzungen = return 0

-- | Berechnung und Verrechnung der Relevanten größen
-- 
-- https://www.smartsteuer.de/portal/lexikon/G/Gewerbesteuer.html
--
gewerbeSteuer :: Gewerbetreibender treib => AccGWRW treib GewerbeSteuer
gewerbeSteuer = withBody $ \g -> do
  gewinn <- fixed gewerbeGewinn
  ertrag <- fixed $ gewerbeErtrag gewinn
  let verbleibenderErtrag =  ertrag - min (positivePart ertrag)
                             (gwStFreibetrag $ gwTreibender g)
      result = GWSt gewinn (positivePart $ gwMesszahl * verbleibenderErtrag)
               $ gwMessbetrag result * (gwHebesatz g)
  
  logLedger $ printf "%v\n" (show result)  

  closingTx gewerbeErgebnisAccounts $ BalancingTx "Gewerbesteuer" 
    jahresErgebnis [(giroKonto, negate $ gwSteuer result)]

  return result


runGewerbe :: Gewerbetreibender treib => AccGWRW treib a -- ^ action to run 
              -> Gewerbe treib
              -> AccountingRW treib a
runGewerbe f gewerbe = withRWT (\e -> e{eBody=gewerbe}) f

                      
-- * Monatliche Actions


-- | Berechnet die Lohnkosten für einen Angestellten
--
-- http://de.wikipedia.org/wiki/Lohnnebenkosten
lohnKostenMtl :: AccGWRW treib ()
lohnKostenMtl = withBody $ \g -> do
  let lohn p = let gehalt = pBruttoGehaltMtl p in
          gehalt + when' (pVersicherungsPflicht p)
          (gehalt * (0.073 -- kranken
                     + 0.0945 -- renten
                     + 0.01025 -- pflege
                     + 0.015 -- arbeitslosen
                     + 0.016 -- unfall
                     + 0.0015 -- insolvenz
                     -- TODO mehr ?
                    ))
      kosten = sum $ lohn <$> gwAngestellte g
  fromTo kosten  "Lohnkosten" giroKonto sonstigerUeberschuss

-- | monatliche Action eines Gewerbes
gewerbeMonatlich :: AccGWRW treib ()
gewerbeMonatlich = withBody $ \g -> do
  lohnKostenMtl
  autoGewerbeMtl
  fromTo (gwMonatlMietkosten g) "Miete" giroKonto mieteUnbeweglich

                           
-- | Accountiung Action für das Gewerbe
autoGewerbeMtl :: AccGWRW treib ()
autoGewerbeMtl = withBody $ \g ->
  let k = kostenAutoMonatl <$> gwWaegen g in
  tx $ BalancingTx "Autokosten" giroKonto
  [(mieteBeweglich,sum $ akGewerbeLeasing <$> k)
  ,(sonstigerUeberschuss,sum $ akGewerbe <$> k)]
