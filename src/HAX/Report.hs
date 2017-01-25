{-# LANGUAGE FlexibleInstances
, OverloadedStrings
 #-}

-- | This module allows to convert the final ledgers generated by
-- "Accounting" into strings for display.
--
-- This module is ditry and work in progress. __Look at the source to use it__
module HAX.Report where

import           HAX.Accounting
import           HAX.Bookkeeping
import           HAX.Common hiding ((!))
import           Data.Aeson hiding (Array)
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Data.List.Split
import           Data.Text (Text)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord
import           System.Directory
import           System.FilePath
import           Text.Blaze hiding (text)
import           Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 hiding (head,map,object,text)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Attributes hiding (id,title)
import qualified Text.PrettyPrint.Boxes as P
import           Text.PrettyPrint.Boxes hiding (left,(<>))
-- import           Network.Wai
-- import           Network.HTTP.Types.Status
-- import           Network.Wai.Handler.Warp

-- * HTML

-- | Write the ledger to html files
writeHtml :: FilePath -- ^ output dir
          -> World -> IO ()
writeHtml dir world = do
  -- copyFile "code.js" $ dir </> "code.js"
  createDirectoryIfMissing True dir
  res <- generate world
  let resHtml = toHtmlTable res
  BL.writeFile (dir </> "header.html") $ indexPage $ resHtml
  BL.writeFile (dir </> "index.html") $ indexPage $ do
    iframe ! src "header.html" ! width "100%" !
      customAttribute "scrolling" "no" ! A.style "position:fixed; border: 0px" ! height "37px"  $ ""
    resHtml
  BL.writeFile (dir </> "data.js") . toVar "data" . encode $ res

-- | convert ledger to Html Markup
toHtmlTable (FullLedger (FixedLedger balances logEntries) accounts) = do
  table ! A.id "data" $ do
    -- header
    tr $ mapM_ headerColumn headerFANs
    sequence_ $ zipWith f (elems logEntries) $ to2DTable balances
  where headerColumn (FAN entity acc) = th ! class_ (fromString entity)
                                        $ toMarkup entity >> br >> toMarkup acc
        f logs (d,row) = do
          mapM_ (logHtml headerFANs d $ length row) $ reverse logs
          tr ! (monthClass "regular" d) $ rowHtml headerFANs show "" (d,row)
        headerFANs = FAN "" "Month" : sortedAccountNames accounts ++ [FAN "" "Comment"]
  
monthClass cl d = class_ $ fromString $ cl ++" "++ if getMonth d == 12 then "endOfYear" else "duringYear"

-- | create one html row with comment and date
rowHtml headerFANs f comment drow = zipWithM_ g headerFANs $ toRow show (f.scale) drow ++ [comment]
  where g (FAN entity _) x = td ! class_ (fromString entity) $ toMarkup x

-- | convert log entries to markup
logHtml :: [FullAccountName] -> ADate -> Int -- ^ number of columns present
           -> EntityLogEntry -> Html
logHtml _ d len (ent,LComment s) = tr  ! (monthClass ("comment " ++ ent) d) $ do
                            td $ toMarkup $ show d
                            td ! colspan (toValue $ len + 1) $ fromString s
logHtml headerFANs d len (ent,LTx tx) = tr ! (monthClass ("transaction " ++ ent) d) $
                                    rowHtml headerFANs
                                    (\x -> if x == 0 then "" else show x) comment (d,row)
  where (row,comment) = transactionRow (1,len) tx
  
css x = link ! rel "stylesheet" ! type_ "text/css" ! href x
js x = script ! src x $ ""

-- | the html skeletton
indexPage htmlBody = renderHtml $ docTypeHtml $ do
     H.head $ do
         meta ! charset "UTF-8"
         js "http://code.jquery.com/jquery-1.11.1.min.js"
         -- js "htpp://ajax.googleapis.com/ajax/libs/webfont/1.4.7/webfont.js"
         -- js "http://d3js.org/d3.v3.min.js" ! charset "utf-8"
         css "http://cdn.datatables.net/1.10.2/css/jquery.dataTables.min.css"
         js "http://cdn.datatables.net/1.10.2/js/jquery.dataTables.min.js"
         js "data.js"
         js "../static/code.js"
         title "hax results" 
     body $ do H.table ! A.id "example" $ ""
               htmlBody


-- * JSON instances

toVar :: (Monoid a,IsString a) => a -> a -> a
toVar name value = "var " <> name <> " = " <> value
                   
instance ToJSON FullLedger where
  toJSON (FullLedger (FixedLedger balances logEntries) accounts) =
    object [ "accounts" .= toJSON (FAN "" "Month" : accs)
           , "balances" .=  (toRow toJSON (toJSON.scale) <$> to2DTable balances)
           , "dates"    .= range (bounds logEntries)
           , "log"    .= elems logEntries
           , "entities" .= ((\x -> fromString (fEntity $ head x) .= (fAccount <$> x))
                            <$> groupBy ((==) `on` fEntity) accs  :: [(Text, Value)])
           ]
    where accs = sortedAccountNames accounts

instance ToJSON FullAccountName
instance ToJSON LogEntry
instance ToJSON Tx
  
instance ToJSON Decimal where
  toJSON d = toJSON ( conv d :: Double)

instance ToJSON ADate where
  toJSON = toJSON . show


-- * String

scale = (roundTo 2.(/1000))

-- zeigeTransaktionen = True
-- zwischenlinien = zeigeTransaktionen

-- -- | Pretty-print the ledger of the world
-- showWorld :: World -> IO String
-- showWorld world = showLedger zeigeTransaktionen
--                   zwischenlinien scale <$> generate world 
    
-- -- | Pretty-print the results of 'Accounting.generate'.
-- showLedger :: (Show b) => Bool -> Bool ->
--               Scale b -> (AccountsMap,FixedLedger) -> String
-- showLedger showTxns showSeps scale (accounts,ledger) =
--   ("\n"++) $ renderTable $ (header:) $ seps $
--   toGroups showTxns scale ledger
--   where header :: [Row String]
--         header = [h False $ fEntity,h True $ fAccount]
--           where h m a = (if m then "mm/yy" else ""): 
--                        (a <$> sortedAccountNames accounts)
--                        ++ [if m then "Comment" else ""]
--         seps = if not showSeps then return.concat else id 
        

-- toGroups:: (Show b) => Bool -> Scale b -> 
--            FixedLedger -> [Group String]
-- toGroups showTxns scale (FixedLedger bals txns) =
--   zipWith groups txnGroups $ toRow show (show.scale) <$> to2DTable bals
--   where groups :: [LogEntry] -> Row String -> Group String
--         groups txns row =  (show <$> (txnRow =<< reverse txns)) ++ [row ++ ["Endsaldo"]]
--         (dateRange,accBounds) = (range1 $ bounds bals, bounds2 $ bounds bals)
--         txnGroups :: [[LogEntry]]
--         txnGroups = if showTxns then elems txns else repeat []
--         txnRow :: LogEntry -> [Row Amount]
--         txnRow (LTx txn) = [transactionRow accBounds txn]
--         txnRow (LComment txn) = [] -- impossible???

-- | generates a row containing the posting's amount at the column
-- corresponding to the posting's account
transactionRow :: (AccountNumber,AccountNumber) -- ^ account number bounds
                  -> Tx -> (Row Amount,Comment)
transactionRow accBounds txn = (postings,tComment txn)
  where postings = elems $ accumArray (+) 0 accBounds
                   $ tPostings txn
        col x = if x == 0 then ""
                else ("       "++) $ show $ scale x
        
-- * Helpers 

type Row e = [e]
type Group e = [Row e]
type Scale b = (Amount -> b)

-- | Renders a list of grouped rows
renderTable :: [Group String] -> String
renderTable groups = render $ punctuateH top seps
                     $ fmap renderCol (transpose $ intercalate  [vertSep] $ groups)
  where seps = vcat P.left $ map text $ replicate
               (length groups - 1 + length (head combined)) " | "
        renderCol xs = vcat P.left $ map text xs
          where rH h = [h,replicate (maximum $ length <$> (h:xs)) '-']
        combined = transpose $ concat groups
        vertSep = map (\c -> replicate (maximum $ length <$> c) '-') combined


toRow :: (r -> e) -> (a -> e) -> (r,Row a) -> Row e
toRow conv1 conv2 (d,row) = conv1 d : fmap conv2 row

-- | Convert a 2D array into a list of pairs, where the first
-- component contains the first index and the second the coresponding
-- row o the array
to2DTable :: (Ix r,Ix c) => Array (r,c) a -> [(r,Row a)]
to2DTable array = zip (range (r1,r2))
                    $ chunksOf (rangeSize (c1,c2)) $ elems array
  where ((r1,c1),(r2,c2)) = bounds array
