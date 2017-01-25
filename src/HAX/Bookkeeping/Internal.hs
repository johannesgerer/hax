{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances
, FlexibleInstances
, TypeFamilies
, ConstraintKinds
, ExistentialQuantification
, MultiParamTypeClasses
, UndecidableInstances
, NoMonomorphismRestriction
, DeriveGeneric
 #-}
{-# OPTIONS_HADDOCK not-home #-}


-- | This module contains the internal type and functions not to be
-- used directly as most of them are unsafe, meaning that they allow
-- actions that violate double-entry contraints or actions
-- on accounts other than the current body's accounts (via
-- 'UNSAFE_AccountN').
--
-- Therefore, __do not use this module directly__, use "HAX.Bookkeeping" instead.
module HAX.Bookkeeping.Internal where

import           HAX.Common
import           Control.Monad.RWS.Strict
import           Data.Array
import           Data.Array.Unsafe
import           Data.Functor.Compose
import qualified Data.Map as M
import           GHC.Generics


-- * Account Names and Numbers

type AccountNumber = Int
type AssetName = String
type EntityName = String

-- | uniquely identifying name used to lookup the account numbers
data FullAccountName = FAN
                       { fEntity :: EntityName
                       , fAccount :: String
                       }
                 deriving (Eq, Ord,Show, Generic)

swapFAN :: FullAccountName -> FullAccountName
swapFAN (FAN a b) = FAN b a

-- | this type is used to make functions taking 'AccountNames' polymorphic. 
data AccountName = AccountN String
                   -- ^ account for the current entity. Only these
                   -- accounts should be accessible in accounting
                   -- actions
                 | UNSAFE_AccountN FullAccountName
                   -- ^ full account for internal use only
                 deriving (Eq, Ord,Show)

instance PrintfArg AccountName where
  formatArg (AccountN s) = formatString s
  formatArg (UNSAFE_AccountN _) = error "not implemented: formatArg (UNSAFE_AccountN)"
             
                          
-- | 'String's are automatically converted to (safe) 'AccoutN'ames, if
-- the -XOverloadedStrings extensions is active. (See "Data.String").
instance IsString AccountName where
  fromString = AccountN 

  
-- | The map from 'FullAccountName's to 'AccountNumber's used
-- internally to address the efficient 'Ledger' array storage
type AccountsMap = M.Map FullAccountName AccountNumber

-- | Extract accounts names order by their internal account numbers
sortedAccountNames :: AccountsMap -> [FullAccountName]
sortedAccountNames = fmap fst . sortBy (comparing snd) . M.toList


-- | extract the 'AccountNumber' for a 'FullAccountName' from an 'AccountsMap'
internalAccountNumber :: FullAccountName -> AccountsMap -> AccountNumber
internalAccountNumber name accs = fromMaybe (error $
                                             printf "Account '%s' not found\nAvailable Accounts:\n%s"
                                             (show name) $ unlines $ show <$> M.toList accs)
                           $ M.lookup name accs
  

-- * Postings and Transactions

type Posting = (AccountName,Amount)
type InternalPosting = (AccountNumber,Amount)


-- | A transaction that is already balanced. Such an object can only
-- be built from 'BalancingTx' using 'balanceTx' and is never needed
-- as function input. This format is used to log the transactions in
-- the 'Ledger' 's 'LogEntry'.
data Tx = UNSAFE_Tx {  tComment  :: Comment
                    , tPostings :: [InternalPosting]
                    }
        deriving (Show,Generic)

-- | A transaction involving only accounts relative to a body, and
-- that is self balancing through the use of an account for the
-- remains
data BalancingTx = BalancingTx { txComment :: Comment
                               , txRemains :: AccountName
                               , txPostings :: [Posting]
                               }

-- | Balance a 'BalancingTx' and prepend the entitiyName to the
-- comment. This is only used internally
balanceTx :: (Monoid w) => BalancingTx -> Acc s l w Tx
balanceTx (BalancingTx comment remains postings) = do
  name <- nameErr "Transactions are only allowed in the presence of an entity"
  fmap (UNSAFE_Tx $ printf "(%s) %s" name comment) $ 
    -- convert to account numbers: 
    mapM (\(name,amount) -> do number <- accountNumber name
                               return (number,amount) )
    -- add a balancing posting:
    $ (remains,negate $ sum $ snd <$> postings):postings 
    
-- * The Ledger

-- | Information that is logged while the ledger is built
data LogEntry = LTx Tx -- ^ Transactions of the current time period
              | LComment String -- ^ Random comment to be put into the ledger
                deriving (Generic,Show)

type EntityLogEntry = (EntityName,LogEntry)

type LedgerIndex = (ADate,AccountNumber)
type LedgerBounds = (LedgerIndex, LedgerIndex)

-- | This class defines what a 'Ledger' is:
class Ledger l where
  -- | it has bounds
  lBounds :: l -> IO LedgerBounds
  -- | single entries for a given account and date
  -- consisting of the account balance and the log
  -- entries for that date can be read.
  lReadEntry :: LedgerIndex -> l -> IO (Amount,[EntityLogEntry])
  -- | the account history can be read
  lAccountHistory :: (ADate,ADate) -> (ADate -> LedgerIndex) -> l -> IO [Amount]
  -- | it can be fixed into an immutable type
  lFix :: l -> IO FixedLedger
  -- | fix withou making a copy. This is has to be safe to use, if the
  -- mutable version is never modified after the freeze operation.
  lunsafeFix :: l -> IO FixedLedger

  
-- | 'LedgerRW' implements a writable (within the IO monad) 'Ledger'.
-- 
-- The total balance is always zero and no transactions that depend on
-- future values are allowed.  This is guaranteed, by not exporting
-- UNSAFE_Ledger and instead, the ledger is only changed using the
-- exported safe functions. E.g. `tx`, 'fromTo', ...
data LedgerRW = UNSAFE_Ledger
  { lUNSAFE_Bals :: IOArray LedgerIndex Amount
  -- ^ Balances for each Date and Account
  , lUNSAFE_LogEntries :: IOArray ADate [EntityLogEntry]
  -- ^ Transactions and Comments for each Date
  }
                
-- | This type implements the Ledger in immutable form, suitable as
-- the main result of the whole program or for accounting accounts
-- that are garantueed to not change the ledger.
data FixedLedger =  FixedLedger
  { lBals :: Array LedgerIndex Amount
  , lLogEntries :: Array ADate [EntityLogEntry]
  }

data FullLedger = FullLedger
  { flLedger :: FixedLedger
  , flAccounts :: AccountsMap
  }

  -- lEase :: Acc s l a -> Accounting s a -- run any kind of action in the full read-write monad
  -- this is never needed, as there is reason to restrict a type to the
  -- FixedLedger. Only the other way round: if an action changes
  -- something, it will have the LedgerRW type and cannot be `fixed`/

instance Ledger LedgerRW where
  lBounds (UNSAFE_Ledger ledger _) = getBounds ledger
  lReadEntry (date,acc) (UNSAFE_Ledger bals txns) =
    liftM2 (,) (readArray bals (date,acc)) $ readArray txns date
  lAccountHistory times f (UNSAFE_Ledger bals _) =
    getElems  =<< mapIndices times f bals
  lFix (UNSAFE_Ledger bals txns) = liftM2 FixedLedger (freeze bals) $ freeze txns
  lunsafeFix (UNSAFE_Ledger bals txns) = liftM2 FixedLedger (unsafeFreeze bals) $ unsafeFreeze txns

  
instance Ledger FixedLedger where
  lBounds (FixedLedger ledger _) = return $ bounds ledger
  lReadEntry (date,acc) (FixedLedger bals txns) =
    return (bals ! (date,acc), txns ! date)
  lAccountHistory times f (FixedLedger bals _) =
    return $ elems $ ixmap times f bals
  lFix = return
  lunsafeFix = return

-- * Accounting Environment

-- | Represents the environment an accounting action is run on.
data Environment body ledger = Env {
  eLedger :: ledger  -- ^ the ledger of the whole world (i.e. all bodys
  , eAccounts :: AccountsMap -- ^ map of all accounts managed by the ledger
  , eDate :: ADate -- ^ current date
  , eBody :: body  -- ^ current body 
  , eName :: Maybe EntityName -- ^ current body's name
  }

                              
-- * Accounting Actions

-- | The Accounting Monad
--
-- This monad is a stack of Reader Writer and IO monad.
--
-- Actions from this monad can read an immutable environment. This
-- environment however, contains references to mutable arrays (see
-- 'LedgerRW'), which can be modified through IO actions lifted into
-- this monad into this
-- monad.
--
-- The 'body' type variable will contain the type of the 'Accounting.Body' the
-- current accounting action is concerned with. 
type Acc body ledger writer = RWST (Environment body ledger) writer () IO
                    
-- | A specializations for read-write accounting actions with no
-- (i.e. trivial '()') writer output
type AccountingRW body = Acc body LedgerRW ()
-- | A specialization for read-only actions. These actions can however
-- produce read-write actions as output via the 'Writer' Monad. This
-- is used in 'fixed'.
type AccountingReadOnly body = Acc body FixedLedger (AccountingRW body ())

instance Monad m => Monoid (m ()) where
  mappend = (>>)
  mempty = return ()


-- | Short-cut class used in type signatures involving 'Acc' and its derivatives
class (Monoid w, Ledger l) => AccPair l w where

instance (Monoid w, Ledger l) => AccPair l w where
  

-- | type synonym for an accounting action that has an amount as result
type AmountRW body = AccountingRW body Amount
type AmountA body l w = Acc body l w Amount

-- | run a read only action and its genrated read-write output within
-- a general accounting action and pass on its result.
fixed :: AccountingReadOnly s a -> AccountingRW s a
fixed actionRO = do e <- ask
                    l' <- lift . lFix $ eLedger e
                    (res,actionRW) <- lift $ evalRWST actionRO e{eLedger = l'} ()
                    actionRW
                    return res

-- ################ Instances #######################
instance Monoid w => Eq (AmountA s l w) where
  (==) = error "Eq (AmountA s l) is impossible"
  
-- | Allows to use 'min' and 'max' and its derivaties directly on actions that
-- return an amount:
-- 
-- > min ( "Cash") (balanceAt date "Cash") :: AmountA s l
--
instance Monoid w => Ord (AmountA s l w) where
  min = liftM2 min
  max = liftM2 max
  (<=) = error "(<=) for (AmountA s l) is impossible"

-- | Allows to use '+','-','*','negate' directly on actions that
-- return an amount. Furthermore any numeral can be used directly as
-- (trivial) accounting action:
-- 
-- > soll "Cash" + 4 :: AmountA s l
--
instance Monoid w => Num (AmountA s l w) where
  (*) = liftM2 (*)
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

instance Monoid w => Fractional (AmountA s l w) where
  (/) = liftM2 (/)
  recip = fmap recip
  fromRational = return . fromRational

-- * Internal Helper Functions


ledgerBounds :: (Monoid w, Ledger l) => Acc s l w (LedgerIndex, LedgerIndex)
ledgerBounds = lift . lBounds =<< reader eLedger

readEntryForNumber ::  (Monoid w, Ledger l) => AccountNumber -> Acc s l w (Amount,[EntityLogEntry])
readEntryForNumber acc = do date  <- reader eDate
                            lift . lReadEntry (date,acc) =<< reader eLedger


accountsNumbers :: (Monoid w, Ledger l) => Acc s l w [AccountNumber]
accountsNumbers = range2 <$> ledgerBounds


timeInterval :: (Monoid w, Ledger l) => Acc s l w (ADate,ADate)
timeInterval = both fst <$> ledgerBounds

accountNumber :: Monoid w => AccountName -> Acc s l w AccountNumber
accountNumber (UNSAFE_AccountN acc) = internalAccountNumber acc <$> reader eAccounts
accountNumber (AccountN acc) = do
  entName <- reader $ nameErr $ printf "'AccountN %s' is not defined" acc
  internalAccountNumber (FAN entName acc) <$> reader eAccounts

-- | Tries to get the 'eName' of the current entity and throws an
-- error if it is Nothing.
nameErr msg = reader (fromMaybe err . eName)
  where err = error $ printf
              "There is no current entity. "++ msg

readEntryForName :: (Monoid w, Ledger l) => AccountName -> Acc s l w (Amount,[EntityLogEntry])
readEntryForName acc = readEntryForNumber =<< accountNumber acc


-- * Internal UNSAFE Functions

-- | perform an accounting action at any date
uNSAFE_at :: Monoid w => ADate -> Acc s l w a -> Acc s l w a
uNSAFE_at date = local (\e -> e{eDate=date})

uNSAFE_addToBalance :: InternalPosting -> AccountingRW s ()
uNSAFE_addToBalance (acc,amount) = do
  UNSAFE_Ledger bals _ <- reader eLedger
  date  <- reader eDate
  lift $ updateArray bals (date,acc) (+amount)

-- | add last month's balances to previous month's. This is performed
-- once for every time step in generate
uNSAFE_carryOver :: AccountingRW s ()
uNSAFE_carryOver = do
  start <- fst <$> timeInterval
  oldD <- reader $ (shift $ - 1) . eDate
  when (start <= oldD) $ accountsNumbers >>=
    (mapM_  $ \ac -> do
        oldB <- fst <$> uNSAFE_at oldD (readEntryForNumber ac)
        uNSAFE_addToBalance (ac,oldB))

                 
