{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
 #-}

module HAX.Germany.Subjekte where

import HAX.Accounting
import HAX.Bookkeeping
import HAX.Common

solidaritaetsFaktor = 0.055

-- * Rechtssubjekte und Gewerbebetrieb
  
data NatuerlichePerson = NatuerlichePerson
     { --  pSteuerklasse -- KEINEN EINFLUSS AUF STEUERLAST, nur auf Zeitpunkt der Zahlung
     pGeburt :: ADate
     , pGewerbe :: Maybe (Gewerbe NatuerlichePerson)
     , kinderMitKindergeldImHaushalt :: Bool
     , kinderMitKindergeld :: Amount
     , landOderForstwirt :: Bool
     , pSplitting :: Bool
     , krankenUndPflegeOhneZuschuesse :: Bool
       -- ^ Müssen Aufwendungen für die Krankenversicherung alleine
       -- getragen werden?
       -- 
       -- http://www.steuernetz.de/aav_steuernetz/lexikon/K-24101.xhtml?currentModule=home
     , krankenUndPflegeOhneZuschuessePartner :: Maybe Bool
     , kinderFreibetragsVerdopplung :: Bool
     , pWagen :: Auto
     , pBruttoGehaltMtl :: Decimal
     , pLohnsteuerMtl :: Decimal
     , pVersicherungsPflicht :: Bool
     -- ^ führt auch zur automatischen Abbuchung der
     -- Sozialversicherungsbeiträge
     , pAuswaertigeKinderInBerufsausbildung :: Decimal
     -- ^ in Berufsausbildung befindende, auswärtig untergebrachte,
     -- volljährigen Kinder, für die Anspruch auf einen Freibetrag
     -- oder Kindergeld besteht (33a Abs. 2)
     }
                       deriving Show


data GmbH = GmbH { gGewerbe :: Gewerbe GmbH
                 }

data Gewerbe body = Gewerbe { -- gwName :: String
                             gwAngestellte :: [NatuerlichePerson]
                            , gwTreibender :: body
                              -- ^ auf wessen Rechnung wird das Gewerbe betrieben
                            , gwHebesatz :: Amount
                            , gwMonatlMietkosten :: Amount
                            , gwWaegen :: [Auto]
                            }
          deriving Show

-- * Konten

giroKonto = "Giro" 



-- * Auto

data Auto = AutoMonatl { aKmPrivatOhneArbeitsfahrten :: Decimal
                         -- ^ ohne Fahrten zur Arbeit, welche auch zu den
                         -- Privaten Fahrten zählen, aber über
                         -- die anderen Felder berechnet werden ('privatKmMonatl').
                       , aKmArbeitsstaette :: Decimal -- ^ Entfernung Wohnort Arbeitsstätte 
                       , aKmGeschaeftl :: Decimal
                       , aArbeitsTage :: Decimal
                       , aFixKosten :: Amount
                       , aLeasing :: Amount
                       , aSpritKostenProKm :: Amount
                       , aListenPreis :: Amount
                       , aFirmenWagen :: FirmenWagen
                       }
          deriving Show
              
data FirmenWagen = Pauschal | Fahrtenbuch | Privat
     deriving Show
       

-- | Zusammensetzung der Kosten für ein 'Auto'. Dieses Object wirh
-- berechnet abhängig von Firmenwagennutzung etc. von
-- 'kostenAutoMonatl'
data AutoKosten = AutoKosten { akPrivat :: Amount,
                               akGewerbe :: Amount,
                               akGewerbeLeasing :: Amount
                             }

entfernungsPauschale :: Auto -> Amount
entfernungsPauschale a = 0.3 * 12 * aArbeitsTage a * aKmArbeitsstaette a
              
kmPrivatMonatl :: Auto -> Amount 
kmPrivatMonatl a = aKmPrivatOhneArbeitsfahrten a +
                   aArbeitsTage a * 2 * aKmArbeitsstaette a

              
-- | http://de.wikipedia.org/wiki/Fahrtenbuch
geldwerterVorteilMonatl :: Auto -> Amount
geldwerterVorteilMonatl a = g $ aFirmenWagen a
  where g Privat = 0
        g Pauschal = aListenPreis a * (
          0.01 + when' (12 * aArbeitsTage a > 46) 0.0003 * aKmArbeitsstaette a)
        g Fahrtenbuch = gesamtKostenAuto a * privat / (privat + aKmGeschaeftl a)
        privat = kmPrivatMonatl a

gesamtKostenAuto :: Auto -> Amount
gesamtKostenAuto = g . kostenAutoMonatl
  where g (AutoKosten a b c) = a+b+c

kostenAutoMonatl :: Auto -> AutoKosten
kostenAutoMonatl a = g  $ aFirmenWagen a
  where g Privat = AutoKosten
                   (aLeasing a + aFixKosten a + aSpritKostenProKm a * kmPrivatMonatl a)
                   (aSpritKostenProKm a * aKmGeschaeftl a)
                   0
        g _      = AutoKosten
                   0
                   (aFixKosten a + aSpritKostenProKm a * (
                       aKmGeschaeftl a + kmPrivatMonatl a))
                   $ aLeasing a

-- * Verlustvortäge und -abzüge

verlustVortragsKonten = [virtuell,
                         steuerlicheVerlustVortraege
                         ]
virtuell = "virt"
steuerlicheVerlustVortraege = "stVV"

-- | Gibt Verlustabzug zurück und registriert Verlustabzug und
-- -vortrag.
-- 
-- Berechnet wird nach den übereinstimmenden Regeln des EStG, KStG, GewStG
--
-- https://www.smartsteuer.de/portal/lexikon/G/Gewerbeverlust.html
--
-- https://www.smartsteuer.de/portal/lexikon/V/Verlustabzug.html
-- https://www.smartsteuer.de/portal/lexikon/G/Gesamtbetrag-der-Einkuenfte.html
--
-- https://www.smartsteuer.de/portal/lexikon/V/Verlustvortrag-und-ruecktrag.html#D063100100038
verlustAbzug :: String -- ^ Type
                -> Amount -- ^ ertrag
                -> AccountingReadOnly b Amount
verlustAbzug vType ertrag = do
  vortraege <- soll steuerlicheVerlustVortraege
  let vorabVerrechenbar = min 1000000 ertrag
      abzug = min vortraege $ vorabVerrechenbar + 0.6*(ertrag - vorabVerrechenbar)
  when (abzug /= 0) $ tell $ 
    fromTo abzug (vType ++ "Verlustvorträge") steuerlicheVerlustVortraege virtuell
  return $ positivePart abzug

-- | Gibt den Ertrag nach Abzug zurück. Ansonsten identisch 'verlustAbzug'
nachVerlustAbzug :: String -- ^ Type
                -> Amount -- ^ ertrag
                -> AccountingReadOnly b Amount
nachVerlustAbzug vType ertrag = (ertrag - ) <$> verlustAbzug vType ertrag
