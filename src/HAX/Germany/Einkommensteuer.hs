{-# LANGUAGE 
 NoMonomorphismRestriction
, OverloadedStrings
 #-}

-- | 
-- TODO: (lohnsteuern) werbungskosten
-- 
-- Beachten: Lebensversicherungen
-- https://www.smartsteuer.de/portal/lexikon/L/Lebensversicherung.html#D063053300009
--
-- http://www.finanzen-versicherungen-blog.de/besteuerung-lebensversicherung-auszahlung-lebensversicherung-steuerfreie-lebensversicherung/
--
module HAX.Germany.Einkommensteuer where

import           HAX.Accounting
import           HAX.Bookkeeping
import           Control.Monad.Writer
import qualified Data.Map as M
import           HAX.Common
import           HAX.Germany.Subjekte
import           HAX.Germany.Gewerbe
-- * Konten
-- ** Vermögen
privatVermoegen = "Verm"
        

-- ** Aufwandskonten

ertragsKonten = [ erstattungsUeberhaenge
                , privateVerausserungsgeschaefte
                , einkuenfteAusBeteiligungenAnKapitalgesellschaftenKapEst
                , einkuenfteAusBeteiligungenAnKapitalgesellschaften
                , sonstigeEinkuenfteAusKapitalvermoegenKapEst
                , sonstigeEinkuenfteAusKapitalvermoegen
                , ausserordentlicheEinkuenfte
                , arbeitslohn
                , steuerfreieEinnahmen
                , uebrigeSummeDerEinkuenfte
                , einkuenfteAusGewerbebetrieb
                ]


einkuenfteAusGewerbebetrieb = "EGW"
steuerfreieEinnahmen = "StFrei" 

-- | positive oder negative summe aller Einkunftsarten außer aus
-- Kapitalvermögen und privaten Veräußerungsgeschäften
uebrigeSummeDerEinkuenfte = "SdE" 
arbeitslohn = "Lohn" 
werbungskostensNichtSelbststaendigeArbeitOhneFahrten = "WerbNSA" 
ausserordentlicheEinkuenfte = "ausserE" 
-- ^ gesetz. Renten, Pensionen, Bezüge, .. 
-- im Sinne des https://www.jurion.de/Gesetze/EStG/24a
-- 
-- https://www.smartsteuer.de/portal/lexikon/A/Altersentlastungsbetrag.html
einkuenfteAusBeteiligungenAnKapitalgesellschaftenKapEst = "EBKK"
-- ^ nicht solche, die der tariflichen/persönlichen ESt. unterliegen (z.B. aus
-- Betriebsvermögen)
einkuenfteAusBeteiligungenAnKapitalgesellschaften = "EBK"
-- ^ beachten: 40% steuerfrei (?)
sonstigeEinkuenfteAusKapitalvermoegenKapEst = "EKVE"
-- ^ nicht solche, die der tariflichen/persönlichen ESt. unterliegen
sonstigeEinkuenfteAusKapitalvermoegen = "EKV"
-- ^ einkuenfteAusKapitalvermoegen'hen nach 32d Abs 2.
-- http://www2.nwb.de/portal/content/ir/service/news/news_1413444.aspx
-- http://www.haufe.de/finance/finance-office-professional/rechtmaessigkeit-der-tariflichen-besteuerung-nach-32d-abs-2-nr-1b-estg_idesk_PI11525_HI5088406.html
--
-- https://www.smartsteuer.de/portal/lexikon/E/Einkuenfte-aus-Kapitalvermoegen.html
privateVerausserungsgeschaefte = "PV" 
erstattungsUeberhaenge = "eUeb" 
                            
-- * Aufwandskonten

aufwandsKonten = [ sonstigeVorsorgeAufwendungOhneBasisbeitraege
                 , krankenundPflegeversicherungBasisbeitraege
                 , privatAusgaben
                 , altersvorsorgeAufwendungenArbeitgeberAnteil
                 , altersvorsorgeAufwendungenEigenanteil
                 , werbungskostensNichtSelbststaendigeArbeitOhneFahrten
                 , lohnSteuer
                 ]
                         
lohnSteuer = "LSt" 

altersvorsorgeAufwendungenEigenanteil = "AvAE"
-- ^ gesetzliche Renten / Rürüp

riesterRentenEinzahlung :: IsString a => a
riesterRentenEinzahlung = error "riesterRente not implemented"
-- ^ not implemented!
--
-- riester rente (+ zulagen. aber nur bis 2100 €??. von
-- Steuerersparnis durch SA wird die Zulage abgezogen. Aber nur wenn
-- ergebnis positiv)
altersvorsorgeAufwendungenArbeitgeberAnteil = "AvAA" 

krankenundPflegeversicherungBasisbeitraege = "KPB" 
-- ^ Kranken- und Pflegeversicherung Basisbeitraege" --
-- nur 96% der Leistungen wenn Krankengeld enthalten

sonstigeVorsorgeAufwendungOhneBasisbeitraege = "sVA" 

privatAusgaben = "PA" 

-- * Types

type AccNat l w = Acc NatuerlichePerson l w
type Betrag l w = Acc NatuerlichePerson l w Amount

type AccNatRW = AccountingRW NatuerlichePerson
type BetragRW = AccountingRW NatuerlichePerson Amount

type AccNatRO = AccountingReadOnly NatuerlichePerson
type BetragRO = AccountingReadOnly NatuerlichePerson Amount

-- * Utility

kinderGeld :: NatuerlichePerson -> Amount
kinderGeld p = g $ kinderMitKindergeld p
  where g 1 = 184
        g 2 = g 1 + 184
        g 3 = g 2 + 190
        g n = g (n-1) + 215

-- | Calcuate the current age in years
alter :: Monoid w => AccNat l w Int
alter = do geb <- readBody pGeburt
           date <- reader eDate
           return $ yearSpan $ dateSpan geb date

-- | Calcuate in which year reaches a certain age
jahrInDemManSoAltWird :: Monoid w => Int -> AccNat l w Int
jahrInDemManSoAltWird x = readBody $ getYear . shift (months $ 12 * x) . pGeburt

                 
splitFaktor p = if pSplitting p then 2 else 1

-- * Berechnung
                
-- | Calculates of the Einkommensteuer for all possible variants,
-- selects the cheapest (Günstigervergleich) and applies it
-- (e.g. Verlust Vortäge).
einkommenSteuer :: GewerbeSteuer -> BetragRW
einkommenSteuer gewst = fixed $ pass $ do
  name <- reader eName
  moeglichkeiten <- forM (range (minBound,maxBound)) -- [V False False () ()]
    $ \v -> (,) v <$> listen (einkommenSteuerVariante gewst v + abgeltungsSteuer v)
  let (variante,(steuer,action)) =
        minimumBy (comparing $ fst.snd) moeglichkeiten

  (soli,actionSoli) <- listen $ solidaritaetsZuschlag gewst variante

  let actualAction = do logLedger $ printf "Die Günstigerprüfung ergibt: %v\n" $ show variante
                        logLedger $ printf "Ersparnisse gegenüber anderen Varianten: %v\n" $
                           show $ (roundTo 0.(steuer-).fst.snd) <$> moeglichkeiten
                        action
                        actionSoli
  return (steuer, const actualAction)


-- * Günstigervergleich
  
-- | Type representing the different possible variants of
-- Einkommensteuer calculation to be compared 
data Variante = V { mitKinderFreibetrag :: Bool
                  , teileinkuenfteVerfahren :: Bool
                  -- ^ wahlrecht nach § 32d Abs. 2 Nr. 3 EStG
                  -- https://www.smartsteuer.de/portal/lexikon/A/Aktien.html
                  -- https://www.smartsteuer.de/portal/lexikon/A/Abgeltungsteuer.html#D063109700011
                  -- ^ http://de.wikipedia.org/wiki/Teileink%C3%BCnfteverfahren
                  --
                  --  https://www.smartsteuer.de/portal/lexikon/E/Einkuenfte-aus-Kapitalvermoegen.html#D063026000028
                  --
                  -- damit ist nicht gemeint 32d Abs 6
                  -- https://www.smartsteuer.de/portal/lexikon/A/Abgeltungsteuer.html#D063109700006
                  -- http://www.steuernetz.de/aav_steuernetz/lexikon/K-36317.xhtml?currentModule=home
                  -- 
                  -- sondern 32 Abs. 2 Nr. 3 EStG
                  , riesterRente :: () -- ^ not implemented
                  , vorsorgeaufwendungenAlteRegel  :: () -- ^ not implemented
                  }
              deriving (Bounded,Eq,Ord,Ix,Show)

type VarBetragRO = Variante -> BetragRO
type VarBetrag l w = Variante -> Betrag l w

-- * Berechnung der Einkünfte 

-- firmenWagenNutzung :: 
                     
-- | https://www.smartsteuer.de/portal/lexikon/E/Einkuenfte-aus-Kapitalvermoegen.html#D063026000031
--
-- http://de.wikipedia.org/wiki/Sparer-Pauschbetrag
-- einkuenfteAusKapitalvermoegen' = withBody $ \p ->
--   assert (>=0) (positivePart $
--                 haben einkuenfteAusKapitalvermoegen - (splitFaktor p * 801))
--   "Verlust aus Kapitalvermoegen not implemented" 

-- ^ https://www.smartsteuer.de/portal/lexikon/E/Einkuenfte-aus-nichtselbststaendiger-Arbeit.html
--
-- https://www.smartsteuer.de/portal/lexikon/E/Entfernungspauschale.html
--
-- https://www.smartsteuer.de/portal/lexikon/W/Werbungskostenpauschbetrag.html
einkuefteAusNichtSelbststaendigerArbeit' = do
  einkuenfte <- haben arbeitslohn
  entfernungsPauschale' <- readBody $ entfernungsPauschale . pWagen
  sonstige <- soll werbungskostensNichtSelbststaendigeArbeitOhneFahrten
  let werbungskosten = max (entfernungsPauschale' + sonstige)
                       $ min (positivePart einkuenfte) 1000 -- pauschbetrag nur bis zur Höhe der Einküfte
  singleLog "Enternungspauschale: %v\n" entfernungsPauschale'
  singleLog "Werbungskosten (nicht-selbstständige Arbeit): %v\n" werbungskosten
  singleLog "Einkuefte Aus nicht-selbstständiger Arbeit: %v\n" einkuenfte
  return $ einkuenfte - werbungskosten
  
summeDerEinkuenfte :: VarBetragRO
summeDerEinkuenfte v = singleResult "Summe der Einkuenfte" $
                       uebrigeSummeDerEinkuenfte' v
                       + einkuefteAusNichtSelbststaendigerArbeit'
                       + haben ausserordentlicheEinkuenfte
-- http://www.gesetze-im-internet.de/estg/__3c.html                
-- 3 Nr. 40 EStG
uebrigeSummeDerEinkuenfte' :: VarBetragRO
uebrigeSummeDerEinkuenfte' v = singleResult "Summe der übrigen Einkuenfte" $
   haben uebrigeSummeDerEinkuenfte
   + haben sonstigeEinkuenfteAusKapitalvermoegen
   + haben einkuenfteAusBeteiligungenAnKapitalgesellschaften
   + 0.6 * when' (teileinkuenfteVerfahren v)
        (haben einkuenfteAusBeteiligungenAnKapitalgesellschaftenKapEst)

-- * Steuerberechnung

-- | Calculates the Einkommensteuer for a given variant
einkommenSteuerVariante :: GewerbeSteuer -> VarBetragRO
einkommenSteuerVariante gewst v = do
  zvE <- zuVersteuerndesEinkommen v

  tariflicheEinkommensteuer <-
    return $ steuerBetrag zvE

  geminderteTariflicheEinkommensteuer <-
    return tariflicheEinkommensteuer
    -- - Minderungsbetrag nach Punkt 11 Ziffer 2 des Schlussprotokolls zu Art. 23 DBA Belgien
    -- - ausländische Steuer nach § 34c Abs. 1 und 6 EStG, § 12 AStG
    
  festzusetzendeEinkommensteuer <-
    return geminderteTariflicheEinkommensteuer
    - gewStErmaessigung gewst tariflicheEinkommensteuer
    + kinderGeldAnrechnung v

  tell $ logLedger $ printf "festzusetzende Einkommensteuer: %v\n" 
    festzusetzendeEinkommensteuer

  return $ festzusetzendeEinkommensteuer

-- | https://www.smartsteuer.de/portal/lexikon/S/Solidaritaetszuschlag.html
--
-- TODO eigentlich unter berücksichtigung von Kinderfreibeträgen?
--
-- https://www.jurion.de/Gesetze/SolZG/3
--
-- http://www.steuerlinks.de/steuerlexikon/lexikon/gewerbesteueranrechnung.html
solidaritaetsZuschlag :: GewerbeSteuer -- ^ GewerbeSteuer
                         -> VarBetragRO
solidaritaetsZuschlag gewst v = singleResult "Solidaritätszuschlag" $ withBody $ \p -> do
  let soliVariante = v{mitKinderFreibetrag=True}
  fEst <- singleResult "Bemessungsgrundlage für Solidaritätszuschlag" $
          mute $ einkommenSteuerVariante gewst soliVariante
                 - kinderGeldAnrechnung soliVariante
  let zuschlag1 = fEst * solidaritaetsFaktor
      zuschlag2 = 0.2 * positivePart (fEst - (splitFaktor p * 972))
  return $ min zuschlag1 zuschlag2

-- | https://www.smartsteuer.de/portal/lexikon/S/Steuerermaessigung-bei-Einkuenften-aus-Gewerbebetrieb.html
--
-- 35 EStG
--
-- http://books.google.de/books?id=NY1oFSCGGM0C&pg=PA123&lpg=PA123&dq=35+estg+verlustausgleich&source=bl&ots=hXL1TzqbyG&sig=cky8FRnixRZVTOMzWWRh7X8A_6Q&hl=en&sa=X&ei=eHUDVODgGcbqaPXMgcAK&ved=0CEoQ6AEwBA#v=onepage&q=35%20estg%20verlustausgleich&f=false
gewStErmaessigung :: GewerbeSteuer -> Amount -- ^ geminderte Einkommensteuer
                  -> BetragRO
gewStErmaessigung gewst est = singleResult "Gewerbesteuerermäßigung" $ do
  einkGew <- positivePart $ haben einkuenfteAusGewerbebetrieb
  when' (einkGew > 0) $ do
    positiveEinkunfte <- sum <$> mapM (positivePart.haben) ertragsKonten
    let hoechstBetrag = einkGew / positiveEinkunfte * est
    return $ max hoechstBetrag $ max (gwSteuer gewst) $ gwMessbetrag gewst * 3.8
                                     
                    
  
-- | Berechnet den Tariflichen Einkommensteuerbetrag für das zu
-- versteuernde Einkommen
steuerBetrag :: Amount -> Amount
steuerBetrag zvE | zvE <= ks !! 0 = 0
                 | zvE <= ks !! 1 = (97458/100*y+1400)*y
                 | zvE <= ks !! 2 = (22874/100*z+2397)*z+971
                 | zvE <= ks !! 3 = 42/100*(zvE-ks !! 2)+13971
                 | True           = 45/100*(zvE-ks !! 3)+97067
  where y = (zvE-ks !! 0)/10000
        z = (zvE-ks !! 1)/10000
        ks = [8354,13469,52881,250730];

testSteuerBetrag = all (\(x,y) ->  y == steuerBetrag x)
                   [(4000,0.0)
                   ,(12000,639.9939990728)
                   ,(24000,3748.9578455914)
                   ,(48000,11975.5534967914)
                   ,(120000,42160.98)]
                  
-- | https://www.smartsteuer.de/portal/lexikon/A/Abgeltungsteuer.html
abgeltungsSteuer :: VarBetragRO
abgeltungsSteuer v = withBody $ \p -> do
  0.25 * positivePart (haben sonstigeEinkuenfteAusKapitalvermoegenKapEst
                       + when' (not $ teileinkuenfteVerfahren v)
                       (haben einkuenfteAusBeteiligungenAnKapitalgesellschaftenKapEst)
                       - (splitFaktor p * 801))


                       

-- | Calculate the "zu versteuerndes Einkommen" for a given variant
-- https://www.smartsteuer.de/portal/lexikon/E/Einkommensteuer.html
--
-- more references in the source code...
zuVersteuerndesEinkommen ::  VarBetragRO
zuVersteuerndesEinkommen v = do
  sdE <- summeDerEinkuenfte v

    -- https://www.smartsteuer.de/portal/lexikon/G/Gesamtbetrag-der-Einkuenfte.html
  gesamtbetragderEinkuenfte <-
    return sdE
    -- https://www.smartsteuer.de/portal/lexikon/P/Private-Veraeusserungsgeschaefte.html#D063064400040
    + assert (>= 0) (haben privateVerausserungsgeschaefte)
    "Verlust aus privaten Verausserungsgeschaeften not implemented" 
    - altersentlastungsBetrag v
    - entlastungsbetragfuerAlleinerziehende
  
  tell $ logLedger $ printf "Gesamtbetrag der Einkünfte: %s" gesamtbetragderEinkuenfte

  einkommen <-
    return gesamtbetragderEinkuenfte
    -- https://www.smartsteuer.de/portal/lexikon/G/Gesamtbetrag-der-Einkuenfte.html
    -- https://www.smartsteuer.de/portal/lexikon/S/Sonderausgaben.html#D063071200006
    + haben erstattungsUeberhaenge
    -- vorzutragender verlust nur bei negativen SdE (§ 10d Abs.1 EStG )
    - verlustAbzug "ESt" (if sdE < 0 then sdE else positivePart gesamtbetragderEinkuenfte)
    - sonderAusgaben
    - aussergewoehnlicheBelastungen
    -- - sonderAfA
    -- + zuzurechnendes Einkommen gem. § 15 Abs. 1 AStG

  zvE <-
    return einkommen
    -- https://www.smartsteuer.de/portal/lexikon/K/Kinderfreibetrag.html
    - kinderFreibetrag v
    -- - Härteausgleich
    
  tell $ logLedger $ printf "zu versteuerndes Einkommen: %v" zvE

  return zvE

mute = censor (const mempty)

  
-- * Sonderausgaben und Belastungen
                                                 
aussergewoehnlicheBelastungen = ausbildungsKosten

-- § 10 EStG (3)
-- http://de.wikipedia.org/wiki/Sonderausgabe_(Steuerrecht)
-- https://www.smartsteuer.de/portal/lexikon/S/Sonderausgaben.html
sonderAusgaben :: BetragRO
sonderAusgaben = allgemeineSonderAusgaben
                 + altersvorsorgeAufwendungen
                 + sonstigeVorsorgeAufwendung
  
-- | allgemeine Sonderausgaben (z.B. auch die
-- Kirchensteuer beinhalten)
--
-- Aktuel nur Pauschbetrag implementiert
allgemeineSonderAusgaben :: BetragRO
allgemeineSonderAusgaben =  singleResult "allg. Sonderaushaben" $
                            readBody $ \p -> 36 * splitFaktor p 
  
-- | http://www.ruv.de/de/r_v_ratgeber/altersvorsorge/foerderung_staat/5_basisrente_sonderausgabenabzug.jsp
--
-- http://www.steuernetz.de/aav_steuernetz/lexikon/K-23995.xhtml?currentModule=home
altersvorsorgeAufwendungen :: BetragRO
altersvorsorgeAufwendungen = singleResult "Altersvorsorgeaufwendungen" $ withBody $ \p -> do
  eigenanteil <- soll altersvorsorgeAufwendungenEigenanteil
  arbeitgeber <- soll altersvorsorgeAufwendungenArbeitgeberAnteil
  jahr <- reader $ fromIntegral . getYear . eDate 
  let satz = min 1 $ 0.76 + (jahr - 2013) * 0.02
  return $ satz * (min (splitFaktor p * 20000) $ eigenanteil + arbeitgeber)
    - arbeitgeber

-- | http://www.steuernetz.de/aav_steuernetz/lexikon/K-24101.xhtml?currentModule=home
--
-- TODO bis 2019 möglicher Günsitgervergleich mit Berechnungsmethode bis 2004?
sonstigeVorsorgeAufwendung :: BetragRO
sonstigeVorsorgeAufwendung = singleResult "sonstige Vorsorgeaufwendungen" $ withBody f
  where f p = do
          basis    <- soll krankenundPflegeversicherungBasisbeitraege
          sonstige <- soll sonstigeVorsorgeAufwendungOhneBasisbeitraege
          return $ max basis -- mindestens die Basisbeiträge
            $ min (hbSelbst + hbPartner) -- höchstens die Höchstebeträge
            $ basis + sonstige
          where hoechstBetrag ohneZus = if ohneZus then 2800 else 1900
                hbSelbst = hoechstBetrag $ krankenUndPflegeOhneZuschuesse p
                hbPartner = when' (pSplitting p) $ hoechstBetrag $
                            fromMaybe (error $ "krankenUndPflegeOhneZuschuessePartner is nothing")
                            $ krankenUndPflegeOhneZuschuessePartner p
  
-- * Sonstiges

entlastungsbetragfuerAlleinerziehende :: BetragRO
entlastungsbetragfuerAlleinerziehende = withBody $ \p -> do
  singleResult "Entlastungsbetrag für Alleinerziehende" $
    when' (kinderMitKindergeldImHaushalt p && not (pSplitting p)) 1308
                                                   

kinderFreibetrag :: VarBetragRO
kinderFreibetrag v = singleResult "Kinderfreibetrag" $
                     when' (mitKinderFreibetrag v) (fst <$> kinderFreibetragUndGeld)

kinderGeldAnrechnung :: VarBetragRO
kinderGeldAnrechnung v = singleResult "Kindergeld Anrechnung" $
                         when' (mitKinderFreibetrag v) (snd <$> kinderFreibetragUndGeld)
  
ausbildungsKosten :: BetragRO
ausbildungsKosten = singleResult "Ausbildungskosten" $ readBody $ \p ->
                         splitFaktor p * 924/2 * pAuswaertigeKinderInBerufsausbildung p

                         
-- | Berechnet den anzurechnenden Freibetrag und das abzuziehende Kindergeld
kinderFreibetragUndGeld :: Monoid w => Acc NatuerlichePerson l w (Amount, Amount)
kinderFreibetragUndGeld  = withBody $ \p -> do
  let faktor = if pSplitting p || kinderFreibetragsVerdopplung p
               then 1 else 0.5
  return (faktor * kinderMitKindergeld p * 7008, 12 * faktor * kinderGeld p)


-- | <https://www.smartsteuer.de/portal/lexikon/A/Altersentlastungsbetrag.html>
--
-- <https://www.jurion.de/Gesetze/EStG/24a>
altersentlastungsBetrag :: VarBetragRO
altersentlastungsBetrag v = withBody $ \p -> do
  grundlage <- haben arbeitslohn
               + positivePart (mute $ uebrigeSummeDerEinkuenfte' v)
  when (pSplitting p) $ error "altersentlastungsBetrag für Splitting not implemented"
  -- Im Fall der Zusammenveranlagung von Ehegatten ist die Anwendung
  -- des Altersentlastungsbetrages für jeden Ehegatten gesondert zu
  -- prüfen (H 24a [Altersentlastungsbetrag bei Ehegatten] EStH).
  
  (prozent, hoechst) <- eigenerAltersentlastungsSatz <$> jahrInDemManSoAltWird 65
  singleResult "Altersentlastungsbetrag" $
    return $ min hoechst $ grundlage * prozent

  where eigenerAltersentlastungsSatz jahr = if jahr > 2014 then (0,0)
                                            else altersentlastungsSaetze ! jahr
altersentlastungsSaetze :: Array Int (Amount,Amount)
altersentlastungsSaetze = assocArray [( 2005,(  40.0,  1900 )) 
                                     ,( 2006,(  38.4,  1824 ))
                                     ,( 2007,(  36.8,  1748 ))
                                     ,( 2008,(  35.2,  1672 ))
                                     ,( 2009,(  33.6,  1596 ))
                                     ,( 2010,(  32.0,  1520 ))
                                     ,( 2011,(  30.4,  1444 ))
                                     ,( 2012,(  28.8,  1368 ))
                                     ,( 2013,(  27.2,  1292 ))
                                     ,( 2014,(  25.6,  1216 ))
                                     ,( 2015,(  24.0,  1140 ))
                                     ,( 2016,(  22.4,  1064 ))
                                     ,( 2017,(  20.8,  988  ))
                                     ,( 2018,(  19.2,  912  ))
                                     ,( 2019,(  17.6,  836  ))
                                     ,( 2020,(  16.0,  760  ))
                                     ,( 2021,(  15.2,  722  ))
                                     ,( 2022,(  14.4,  684  ))
                                     ,( 2023,(  13.6,  646  ))
                                     ,( 2024,(  12.8,  608  ))
                                     ,( 2025,(  12.0,  570  ))
                                     ,( 2026,(  11.2,  532  ))
                                     ,( 2027,(  10.4,  494  ))
                                     ,( 2028,(  9.6,   456  ))
                                     ,( 2029,(  8.8,   418  ))
                                     ,( 2030,(  8.0,   380  ))
                                     ,( 2031,(  7.2,   342  ))
                                     ,( 2032,(  6.4,   304  ))
                                     ,( 2033,(  5.6,   266  ))
                                     ,( 2034,(  4.8,   228  ))
                                     ,( 2035,(  4.0,   190  ))
                                     ,( 2036,(  3.2,   152  ))
                                     ,( 2037,(  2.4,   114  ))
                                     ,( 2038,(  1.6,   76   ))
                                     ,( 2039,(  0.8,   38   ))
                                     ,( 2040,(  0.0,   0    ))
                                     ]
                          
