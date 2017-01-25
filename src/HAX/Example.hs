{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
 #-}
module HAX.Example where

import HAX.Bookkeeping
import HAX.Report
import Data.Ix
import HAX.Accounting
import HAX.Assets
import HAX.Common
import HAX.Germany

start = date 1 2014
example1 = World (start,date 12 2015) [facebookE, markZuck]
            
facebookE = Entity "FB" ["AV"] facebookAction facebookGmbH

facebookGmbH = GmbH facebook

facebook = Gewerbe { gwAngestellte = [markZuckNat]
               , gwMonatlMietkosten = 9930
               , gwWaegen = [car]
               , gwTreibender  = facebookGmbH
               , gwHebesatz    = 3.5 -- Some town in Germany
               }


markZuck = Entity "Mark" ["Haus","Hausdarlehen","Cash" ] markZuckAction  markZuckNat

markZuckNat = NatuerlichePerson{ pGeburt = date 12 1960
                         , kinderMitKindergeldImHaushalt = True
                         , landOderForstwirt = False
                         , pSplitting = False
                         , krankenUndPflegeOhneZuschuesse = True
                         , krankenUndPflegeOhneZuschuessePartner = Nothing
                         , kinderMitKindergeld = 2
                         , kinderFreibetragsVerdopplung = False
                         , pAuswaertigeKinderInBerufsausbildung = 1
                         , pWagen = car
                         , pBruttoGehaltMtl = 4000
                         , pLohnsteuerMtl = 1300
                         , pVersicherungsPflicht = False
                         , pGewerbe = Nothing
                         }

car = AutoMonatl { aKmPrivatOhneArbeitsfahrten = (45000 - 7300 - 2 * 33 * 250)/12
                , aKmGeschaeftl =  7300/12
                , aKmArbeitsstaette = 32
                , aArbeitsTage = 250 / 12
                , aFixKosten = 208
                , aLeasing = 400
                , aSpritKostenProKm = 8/100 * 1.4
                , aListenPreis = 35600
                , aFirmenWagen = Pauschal
                }

markZuckAction = do

  onceAt start $ do
    tx $ BalancingTx "Anfangswerte"
      privatVermoegen
      [("Haus",240000)
      ,("FB",-430000)
      ]
  

  -- handle $ FixedPayment 600 start start 1 "SdE" "Cash" "Miete"
  -- -- -- balanceInterest $ IAccount ent LineOfCredit ir
  -- handle $ Loan 100000 (Annuity $ (0.11378694062058245-ir2)/12)
  --   "Cash" "Hausdarlehen" (IR ir2 "SdE") (date 1 2014) 1
  
  handle $ TransactionalAccount (IR verrechZins privatAusgaben)
    (IR verrechZins sonstigeEinkuenfteAusKapitalvermoegen)
    "FB" 12

  -- -- -- transferAll "clearing the shit" Cash Trash
  
  -- fromTo 10 "mntl. Entnahme" "FB" "Cash" 

  when (not $ pVersicherungsPflicht markZuckNat) $ do
    tx $ BalancingTx "Kranken- & Pflegeversicherung"
      privatAusgaben
      [(krankenundPflegeversicherungBasisbeitraege, 6184/12)
      ,(giroKonto, -632)
      ]
                                                    
        
  
  onceAt (date 12 2012) $ fromTo 449 "Erstattungsüberhaenge aus Kirchensteuer"
    erstattungsUeberhaenge privatVermoegen 
  return ()



    
facebookAction = do
  -- fromTo 10 "mntl. Entnahme" "Kasse" "MarkZuck" 
  onceAt start $ do
    tx $ BalancingTx "Anfangswerte"
      "AV"
      [("Mark",430000)
      ,(gewinnVerlustVortraege,-409000)
      ]

  handle $ TransactionalAccount (IR 0.082 schuldZinsen)
    (IR 0.005 sonstigerUeberschuss)
    giroKonto 12
  
  handle $ TransactionalAccount (IR verrechZins schuldZinsen)
    (IR verrechZins sonstigerUeberschuss)
    "Mark" 12
  
  tx $ BalancingTx "Ertrag"
    sonstigerUeberschuss
    [(giroKonto,
      (2109789   -- "Umsatz"
       +   7036   -- verrechnung Sachbezüge
       +   2993   -- sonstige Erträge
       -1266300   -- "Wareneinsatz"
       - 390930   -- Lohn
       +  48000   -- Lohn MarkZuck (wird im Gewerbe behandelt)
       +   3900   -- Lohn + Abgaben Putzfrau
       - 161250   -- LohnAbgaben
       - 162901   -- Raumkosten
       + 119160   -- miete (wird im Gewerbe behandelt)
       -   6924   -- Versicheurngen
       -   5783   -- Reparaturen
       -  22940   -- KFz
       +  12240   -- KFz + Sprit MarkZuck (wird im Gewerbe behandelt)
       -  44939   -- Werbekosten & Reise
       -   7755   -- Warenabgabe
       -  27803   -- Verschiedene betriebliche Kosten
       -    190   -- sonstige Verluste
       +    450   -- Kst Aufzinsung
       -    508   -- KFz Steuer
       -- -   8400   -- Zinsen
       -- +  17018   -- Zinsen MarkZuck
      )/12) 
    ,("AV", negate
      (1196       -- Abschreibungen
       )/12)
     ]

  handle $ Liquidity sonstigerUeberschuss giroKonto
    [ -27184.58
    , -64996.74
    , -16134.78
    , -11022.01
    , -1467.94
    , -4401.49
    ,  10538.55
    ,  7653.95
    ,  8071.67
    , -20976.63
    , -5815.15
    ,  167876.36
    ]

  return ()

       
       
    
ir2 = 0.0242
verrechZins = 0.046
