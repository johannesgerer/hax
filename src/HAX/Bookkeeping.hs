{-# LANGUAGE 
NoMonomorphismRestriction
 #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

-- | This module contains the accounting combinators that can be used
-- to __build complex accounting actions__.
-- 
-- All combinators are __guaranteed to only allow balanced transactions__ that adhere to the double-entry bookkeeping
-- standards.
--
-- The module "Accounting"
-- contains the functions to __run these actions__ and calculate the
-- resulting ledger.
module HAX.Bookkeeping
       (AccountNumber
       , Acc
       , AccPair
       , AccountName(AccountN)
       , AccountingRW
       , AccountingReadOnly
       , AccountsMap
       , AmountA
       , AmountRW
       , AssetName(..)
       , BalancingTx(..)
       , EntityName
       , Environment(..)
       , FixedLedger(..)
       , FullLedger(..)
       , LedgerBounds
       , FullAccountName(..)
       , Ledger(..)
       , LogEntry(..)
       , EntityLogEntry
       , Posting
       , Tx(tPostings,tComment)
       , Transfer
       , balancesSince
       , closingTx
       , balanceAt
       , logMsg
       , singleResult
       , singleLog
       , curDate
       , currentBalance
       , fixed
       , fromTo
       , fromToLimit
       , haben
       , onceAt
       , atYearEnd
       , sortedAccountNames
       , onceEvery
       , onlyAfter
       , schedule
       , soll
       , transferAll
       , tx
       , logLedger
       ) where

import HAX.Bookkeeping.Internal
import Control.Monad.Reader
import Data.Array
import HAX.Common

-- * Transactions

-- | Applies a balanced transaction to the ledger at the current date.
tx ::  BalancingTx-> AccountingRW s ()
tx balancingTx =  do
  UNSAFE_Ledger bals txns <- reader eLedger
  date  <- reader eDate
  name <- reader (fromMaybe "" . eName)
  balancedTx <- balanceTx balancingTx
  lift $ updateArray txns date ((name,LTx balancedTx):)
  mapM_ uNSAFE_addToBalance $ tPostings balancedTx


-- | as 'tx' but taking a list of accounts that should be emptied completely
closingTx :: [AccountName] -> BalancingTx -> AccountingRW s ()
closingTx accs btx = do
  postings <- forM accs $ \acc -> (,) acc . negate <$> currentBalance acc
  tx $ btx{txPostings = txPostings btx ++ postings}
                     
             
-- | Type for simple transfers between two accounts
type Transfer s = String -> AccountName -> AccountName -> AccountingRW s ()

-- | Apply a simple transaction
fromTo ::Amount -> Transfer s
fromTo amount comment from to = do
  tx $ BalancingTx comment to $ [(from,negate amount)]

-- | Apply a simple transaction, but ensure, that source does not
-- change sign
fromToLimit :: Amount -> Transfer s
fromToLimit amount comment from to = do
  am <- op <$> currentBalance from
  fromTo am comment from to
  where op = if amount > 0 then min amount else max amount

-- | Transfer all funds from one account to the other
transferAll :: Transfer s
transferAll comment from to = do am <- currentBalance from
                                 fromTo am comment from to

-- * Balances 

-- | Get the current balance of an account
currentBalance :: AccPair l w => AccountName -> Acc s l w Amount
currentBalance name = fst <$> readEntryForName name

-- | Get the balance of a <http://de.wikipedia.org/wiki/Soll Soll>
-- account (uses the amounts directly as stored in the ledger)
soll :: AccPair l w => AccountName -> Acc s l w Amount
soll = currentBalance

-- | Get the balance of a <http://de.wikipedia.org/wiki/Haben Haben> Account 
-- account (negates the internally stored amounts)
haben :: AccPair l w => AccountName -> Acc s l w Amount
haben = fmap negate . soll


-- | Get the balance at a certain date
balanceAt :: AccPair l w => ADate -> AccountName -> Acc s l w Amount
balanceAt d = uNSAFE_at d . currentBalance

-- | Get the balances since a certain date
balancesSince :: AccPair l w => ADate -> AccountName -> Acc s l w [Amount]
balancesSince since acc = do
  date  <- reader eDate
  acc' <- accountNumber acc
  start <- fst <$> timeInterval
  lift . lAccountHistory (max since start,date) (\t -> (t,acc'))
    =<< reader eLedger

-- * Date combinators
                    
-- | Get the current date
curDate :: Monoid w => Acc s l w ADate
curDate = reader eDate

-- | Restrict an accounting action to a certain date
onceAt :: Monoid w => ADate -> Acc s l w () -> Acc s l w ()
onceAt d a = do {d2 <- curDate ; when (d == d2) a }

-- | Execute an action periodically
onceEvery :: Monoid w => ASpan -- ^ Period
             -> ADate -- ^ Offset
             -> Acc s l w a -> Acc s l w ()
onceEvery period offset action = do { date <- curDate ;
     when (period `divides` (dateSpan offset date)) $ action >> return () }

-- | Execute an action at the end of every year
atYearEnd ::  Monoid w => Acc s l w a -> Acc s l w ()
atYearEnd = onceEvery 12 (month 12)

-- | Executes an action only after a certain date
onlyAfter :: Monoid w => ADate -> Acc s l w a -> Acc s l w ()
onlyAfter start action = do { date <- curDate ;
     when (date > start) $ action >> return () }
                                

-- | Perform an accounting action now, but run it with a modified (future)
-- date. E.g.
--
-- > schedule (date 12 2016) $ tx1
--
-- All changes that tx1 performs will be written to the ledger right
-- now, but only modify balances at 12/2016.
schedule :: Monoid w => ADate -> Acc s l w a -> Acc s l w a
schedule date action =do
  cdate <- curDate
  if (date < cdate ) then
    error $ printf
    "cannot schedule in the past. \"%v\" lies before today \"%v\""
    date cdate
  else uNSAFE_at date action

-- * Logging

-- | Write a log entry to stdout or to the ledger
logMsg :: Bool -- ^ True = to stdout, False = to ledger
          -> String -> AccountingRW s ()
logMsg toStdout s = do
  txns <- reader $ lUNSAFE_LogEntries . eLedger
  name <- reader (fromMaybe "" . eName)
  date <- reader eDate
  let pair = if toStdout then show (date,name)
             else name
      entry = printf ("(%s) "++s++"\n") pair
  lift $ if toStdout
         then putStrLn entry
         else updateArray txns date ((name,LComment s):)

-- | Write a log entry to the ledger
logLedger :: String -> AccountingRW s ()
logLedger = logMsg False


-- | within the ReadOnly Monad: register a single log entry consisting
-- of a formating string and a value
singleLog :: PrintfArg t => String -- ^ formatting string
             -> t -- ^ value
             -> AccountingReadOnly body ()
singleLog a b = tell $ logLedger $ printf a b

-- | within the ReadOnly Monad: return the value of an action and
-- register a single log entry describing the value, but only if it s
-- not zero
singleResult :: (PrintfArg a, Num a, Eq a) => String -- ^ formatting string
             -> AccountingReadOnly body a -- ^ action
             -> AccountingReadOnly body a
singleResult name action = do value <- action
                              when (value /=0 ) $ singleLog (name++": %v\n") value
                              return value
