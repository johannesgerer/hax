{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
, TypeSynonymInstances
, FlexibleInstances
, TypeFamilies
, FlexibleContexts
 #-}

-- | This module provides the 'Body' instances for German Natürliche
-- Person as required to calculate income and business tax.
--
-- 'Body's are defined in "Accounting".
module HAX.Germany.NatuerlichePerson where

import HAX.Accounting
import HAX.Bookkeeping
import HAX.Common
import Control.Monad.Reader
import HAX.Germany.Subjekte
import HAX.Germany.Gewerbe
import HAX.Germany.Einkommensteuer


    
-- * Gewerbetreibender Instance

instance Gewerbetreibender NatuerlichePerson where
  gwStFreibetrag _ = 24500
  gewerbeGewinn = jahresErgebnis'
             

-- * Natürliche Person ('Body' und 'Gewerbetreibender' Instances)

instance Body NatuerlichePerson where
    nominalAccounts p = maybe [] (const gewerbeAccounts) (pGewerbe p) 
                        ++ verlustVortragsKonten 
                        ++ aufwandsKonten ++ ertragsKonten ++ 
                        [privatVermoegen
                        ,giroKonto
                        ]
    bodyMonthly action = withBody $ \p -> do
      gehalt
      sozialAbgaben
      autoPrivatMtl
      fromTo (kinderGeld p) "Kindergeld" steuerfreieEinnahmen giroKonto
      maybe (return ())
        (runGewerbe gewerbeMonatlich) $ pGewerbe p

      action

      atYearEnd $ do
      -- Gewerbe
      -- TODO Thesaurierungsbegünstigung 34a EStG
      -- https://www.smartsteuer.de/portal/lexikon/T/Tarifbegrenzung-fuer-nicht-entnommene-Gewinne-bei-der-Einkommensteuer.html
        gewerbeSteuer <- maybe (return keineGewerbeSteuer)
                         (runGewerbe gewerbeSteuer) $ pGewerbe p
        when (gwGewinn gewerbeSteuer /=0) $
          fromTo (gwGewinn gewerbeSteuer) "Einkünfte aus Gewerbebetrieb"
            uebrigeSummeDerEinkuenfte privatVermoegen
    
        fromTo 100 "Arbeitsmittel und übrige Werbungskosten" privatVermoegen
          werbungskostensNichtSelbststaendigeArbeitOhneFahrten

        est <- einkommenSteuer gewerbeSteuer
        lst <- soll lohnSteuer
        closingTx (maybe [] (const [jahresErgebnis]) (pGewerbe p)
                   ++ ertragsKonten ++ aufwandsKonten)
          $ BalancingTx "Steuern und Abschluss"
          privatVermoegen
          [(giroKonto,lst - est)]
          
        

-- http://de.wikipedia.org/wiki/Lohnnebenkosten
sozialAbgaben :: AccNatRW ()
sozialAbgaben = withBody $ \p -> when (pVersicherungsPflicht p) $ do
  let brutto = pBruttoGehaltMtl p
  tx $ BalancingTx "Sozialabgaben"
    giroKonto
    [(krankenundPflegeversicherungBasisbeitraege,
      brutto * (0.082  -- Krankenversicherung
                -- TODO  0.04 davon sind KrankengeldAnspruch = Privatausgaben
                + 0.01025))  -- Pflegeversicherung
    ,(altersvorsorgeAufwendungenEigenanteil, brutto * 0.0945) -- gesetzl. Rentenversicherung
    ,(sonstigeVorsorgeAufwendungOhneBasisbeitraege, brutto * 0.015 ) --arbeitslosenversicherung
    ]

  fromTo (brutto * 0.0945) "Arbeitgeberanteil Rentenversicherung"
    steuerfreieEinnahmen altersvorsorgeAufwendungenArbeitgeberAnteil        

gehalt :: AccNatRW ()
gehalt = withBody $ \p -> do
  let geldwerterVorteil = geldwerterVorteilMonatl $ pWagen p
      brutto = pBruttoGehaltMtl p
      lohnsteuer = pLohnsteuerMtl p
  tx $ BalancingTx "Gehalt und evtl. Firmenwagen"
    arbeitslohn
    [(giroKonto, brutto - lohnsteuer)
    ,(privatAusgaben, geldwerterVorteil)
    ,(lohnSteuer, lohnsteuer)
    ]

                     
-- | Accountiung Action für die Privatperson
autoPrivatMtl :: AccNatRW ()
autoPrivatMtl = withBody $ \p -> 
  let kosten = akPrivat $ kostenAutoMonatl $ pWagen p in
  when (kosten /= 0) $ fromTo kosten "Autokosten" giroKonto privatAusgaben
                     
