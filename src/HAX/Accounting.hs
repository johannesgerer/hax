{-# LANGUAGE
 ExistentialQuantification
, NoMonomorphismRestriction
 #-}
-- | This module provides 
--
-- * the types to build a 'World' of accounting 'Entity's and
--
-- * functions to compute the ledger resulting from the entities'
-- accounting actions
--
-- * built from combinators found in "Bookkeeping" and "Assets".
--
-- Use "Report" to display the results.
module HAX.Accounting where

import HAX.Bookkeeping.Internal
import HAX.Bookkeeping
import Control.Monad.RWS.Strict
import HAX.Common
import qualified Data.Map as M

  
-- * Accounting Entities

data World = World { wLife :: (ADate,ADate) -- ^ time interval
                   , wEntites :: [Entity]
                   }


  
             
-- | An entity keeping accounts over its assets.
data Entity = forall body . Body body => Entity  {
  entName :: EntityName
  -- ^ the name of the entity. This will serve as an 'AccountName'
  -- when booking transaction between this entity an another entity.
  , entAssets :: [AssetName]
  -- ^ the entity's asset accounts
  , entMonthlyAction ::  AccountingRW body ()
  -- the entity's monthly accounting action
  , entBody :: body
  -- ^ the entity's 'Body', defining its nominal acounts.
  }

-- | The body of an entity. 
class Body body where
  -- | The only requirement is that each Body has a list of nominal
  -- accounts associated with it. See "Germany" for instances.
  nominalAccounts :: body -> [String]
  bodyMonthly :: AccountingRW body () -- ^ custom actin of the entity
                 -> AccountingRW body ()  -- ^ monthly action

-- * Running actions and generating ledgers

-- | Executes an accounting action in a simple environment (for
-- testing purposes) with no accounts.
--
-- >>> simple (date 12 2016) NatuerlichePerson{pGeburt=date 12 1960} alter
-- 56

simple :: ADate -> body -> AccountingRW body a -> IO a
simple date body action = do
  bals <- newArray ((date,1),(date,1)) 0
  txns <- newArray (date,date) []
  let ledger = UNSAFE_Ledger bals txns
  fst <$> evalRWST action (Env ledger M.empty date body Nothing) ()
  

-- | Generate the ledger of the 'World'. It will include all nominal
-- and asset accounts of all 'n' entities, as well as 'n*(n-1)'
-- transactional accounts between each pair of entities.
--
-- Furthermore, it will check the balances of these transactional
-- accounts at the end of every month using
-- 'checkTransactionalAcountSymmetry'.
generate ::  World -> IO FullLedger
generate (World times entities) = do
  let entityNames = entName <$> entities
      eAccounts (Entity name assets _ body) =
        assets ++ nominalAccounts body
        ++ (filter (name /=) entityNames)
      allAccounts = M.fromList $  zip accountList [1..]
      accountList = [ FAN (entName e) a | e <- entities, a <- eAccounts e ]
      nAccs = M.size allAccounts
  when (length accountList /= M.size allAccounts) $
    error $ "AccountNames are not unique: "++
    (unlines$show<$>accountList)
  printf "Created a ledger with %v accounts\n" nAccs
  bals <- newArray ((fst times,1),(snd times,nAccs)) 0
  txns <- newArray times []
  let ledger = UNSAFE_Ledger bals txns
      monthAction = do
        uNSAFE_carryOver
        checkTransactionalAcountSymmetry entityNames
        sequence_ [withRWT (\e -> e{eBody=body, eName=Just name})
                   $ bodyMonthly action
                  | (Entity name _ action body) <- entities ]
  result <- sequence [ runRWST monthAction
                         (Env ledger allAccounts date () Nothing) ()
                      | date <- range times  ]
  -- this is safe, as the ioarrays do not leave this function
  ledger' <- lunsafeFix ledger
  return $ FullLedger ledger' allAccounts

withRWT :: (r' -> r) -> RWST r w () m a -> RWST r' w () m a
withRWT f = withRWST $ \r s -> (f r,())
evalRWT :: Monad m => RWST r () () m a -> r -> m a
evalRWT action env = liftM fst $ evalRWST action env ()

-- * Helpers

-- | For each pair of entities, 'entity1' and 'entity2', there are two
-- transactional accounts, as each entity keeps its own books about
-- its transactions with the other entity. If no entity made a
-- mistake, the accounts should have opposite balances that cancel
-- each other. This function informs about a violation of this
-- property.
checkTransactionalAcountSymmetry :: (Monoid w, Ledger l) => [EntityName] -> Acc s l w ()
checkTransactionalAcountSymmetry entities = do
  let pairs = [  FAN e1 e2 | e1:rest <- tails entities,  e2 <- rest ]
      cb = currentBalance . UNSAFE_AccountN
  date <- reader eDate
  forM_ pairs $ \p -> do
    v1 <- cb p
    v2 <- negate <$> cb (swapFAN p)
    when (v1 /= v2) $ lift $ printf
      "symmtery violation at %v: %s %v /= %s %v\n"
      date (show p) v1 (show $ swapFAN p) v2
  

-- | constructs an action reading an information from the current body
readBody :: Monoid w => (s -> a) -> Acc s l w a
readBody f = reader $ f . eBody

-- | construct an action that depends on the body
withBody :: Monoid w => (s -> Acc s l w a) -> Acc s l w a
withBody = (reader eBody >>=)
