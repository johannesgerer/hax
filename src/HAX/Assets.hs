{-# LANGUAGE TypeSynonymInstances
, NoMonomorphismRestriction
, FlexibleInstances
 #-}

-- | This module provides a collection of accounting actions for
-- different asset/account types implemented via the 'Asset' class.
--
-- Use these actions to build more complex accounting actions for
-- 'Entity's from "Accounting".
--
-- They are implemented using the more basic combinators from
-- "Bookkeeping".
--
-- Many assets make use the 'InterestRate's defined below.
module HAX.Assets where

import HAX.Bookkeeping
import Control.Monad.Reader
import HAX.Common

-- * The Asset class

-- | An asset is anything that can be handled within an accounting
-- action.
class Asset a where
  handle :: a -> AccountingRW s ()
  -- ^ derives the action corresponding to the asset's characteristics
                             
-- * Transactional Account with asymmetric interest rates.

-- | This assets calculates interest according to its average positive
-- and negative account balances over the last time period and debits
-- it againt the 'InterestRate's 'iSource' accounts.
-- 
-- Other names: Revolving credit, line of credit, Kontokorrent
data TransactionalAccount = TransactionalAccount {
  taCredit :: InterestRate
  -- ^ credit interest for negative balances
  -- for one period (not neccessarily p.a.)
  , taDebit :: InterestRate
  -- ^ debit interest for positive balances
  -- for one period (not neccessarily p.a.)
  , taAcc :: AccountName  -- ^ asset's account name
  , taPeriod :: ASpan -- ^ period in months
  }
                            
instance Asset TransactionalAccount where
  handle (TransactionalAccount icredit idebit acc period) = do
    let avg m = sum m / fromIntegral (sMonths period)
        tx a b c = interestTx a (b ++ " für "++ show acc) acc (Just $ return $ avg c)
    date <- curDate
    onceEvery period (month 12) $ do
      (debit,credit) <- partition (>0) <$> balancesSince (shift (1-period) date) acc
      when ((sum $ debit ++ fmap negate credit) /= 0) $
        logLedger $ printf "%s: positive balances: %v, negative balances: %v"
        acc (PList debit) (PList credit)
      tx icredit "Sollzins" credit
      tx idebit  "Habenzins" debit
      return ()

-- *  Loan with fixed (annuity) or variable payments

-- | Select a payment schedule
data PaymentType = Linear Decimal
                   -- ^ Decreasing payments with fixed repay and
                   -- decreasing interest portions.
                   --
                   -- The number specifies the repay as a fraction of
                   -- the principal amount.
                 | Annuity Decimal
                 -- ^ Fixed payments with decreasing interest and
                 -- increasing repay portions.
                 --
                 -- The number specifies the initial repay as a
                 -- fraction of the principal amount.
                 deriving Show

data Loan = Loan { lPrincipal :: Amount -- ^ Principal amount payed to lPaymentAccount
                 , lPType :: PaymentType
                 , lPaymentAcc ::  AccountName  -- ^ account for the payments
                 , lLoanAcc    ::  AccountName  -- ^ account for the open balance
                 , lInterest :: InterestRate
                   -- ^ interest rate for one period (not neccessarily p.a.)
                 , lStart  :: ADate -- ^ payout date
                 , lPeriod :: ASpan -- ^ period of the interest payments
                 }

instance Asset Loan where
  handle (Loan principal pType paymentAcc loanAcc ir start period) = do
    date <- curDate
    let name = show loanAcc
        dur = duration pType ir
    onceAt start $ fromTo principal ("Principal for "++name) loanAcc paymentAcc
      >> lift (do printf "Loan %v runs for %v periods\n" name $ show $ roundTo 3 $ conv dur
                  -- printf "%s == %s" (show pType) $ show $ annuity dur (iRate ir) - (iRate ir)
              )
    onlyAfter start $ onceEvery period start $ do
      curInterest <- interestTx ir ("Interest on "++name) loanAcc Nothing
      let payment (Linear x) = x*principal + curInterest
          payment (Annuity x)= x*principal + interest ir principal
      fromToLimit (negate $ roundTo 3 $ payment pType) ("Rate for "++name) loanAcc paymentAcc

-- * Liquitidy Simulation

-- | This 'Asset' produces a list of twelve payments, one for each
-- month, that are shifted in such a way, that they sum up to zero
data Liquidity = Liquidity { lFrom :: AccountName
                           , lTo :: AccountName
                           , lPayments :: [Amount]
                           }

instance Asset Liquidity where
  handle (Liquidity from to pays) = do
    m <-reader (getMonth.eDate)
    fromTo (pays !! pred m) "Liquidity Simulation" from to
    where avg = sum (take 12 pays) / 12
    
  
-- ** Helpers

-- | Calculate the number of periods time until the loan is completely repaid.
duration :: PaymentType -> InterestRate -> Decimal
duration ptype ir = conv $ duration' ptype $ iRate ir
  where duration' (Linear x)  _    = conv $ 1/x
        duration' (Annuity x) 0    = conv $ 1/x
        duration' (Annuity x) rate = negate $ on logBase conv (1 + rate) (x/(rate + x))

-- | Calculate the annuity (as a fraction of the principal) for a
-- given number of periods interest rate.
annuity :: Decimal -- ^ number of periods
        -> Decimal -- ^ interest rate
        -> Amount
annuity duration' rate' = conv $ ((1 + rate) ** duration) * rate/(((1 + rate) ** duration) - 1)
  where rate = conv rate' :: Double
        duration = conv duration' :: Double



-- * Fixed Payments

-- | Recurring Fixed Payment 
data FixedPayment = FixedPayment {
  fPayment :: Amount
  , fStart :: ADate
  , fOffset :: ADate
  , fPeriod :: ASpan
  , fSource :: AccountName
  , fSink :: AccountName
  , fComment :: Comment
  }

instance Asset FixedPayment where
  handle (FixedPayment am start offset period source sink comment) =
    onlyAfter start $ onceEvery period offset $ fromTo am comment source sink

        
-- * Interest Rates 

-- | Interest rates together with the nominal account, the could be
-- identified with the source (for incoming interest payments) or sink
-- (for outgoing, i.e. negative interest payments).
--
-- For example, if the interest is an expense, 'iSource' could be
-- \"Expenses\". If the interest is capital yield, 'iSource' could be
-- \"Income\".
data InterestRate = IR {
  iRate :: Amount
  , iSource :: AccountName
    -- ^ this can be something like \"Earnings\" if the interest can
    -- be considered earnings or another entity, if
  }

-- | Calculate the interest for a given amount
interest :: InterestRate -> Amount -> Amount
interest ir am = roundTo 2 $ am * (iRate ir)


-- | Calculate the interest for a current balance of an account
currentInterest :: (Monoid w, Ledger l) => InterestRate-> AccountName -> AmountA s l w
currentInterest ir acc = interest ir <$> currentBalance acc

-- | Calculate and transfer the interest from the 'iSource' account to
-- some other account.
interestTx :: InterestRate -> String -- ^ comment
              -> AccountName -- ^ Sink account
              -> Maybe (AmountRW s)
              -- ^ optional: Use this amount instead of the balance of
              -- the sink account
              -> AmountRW s
interestTx ir comment acc maybeAm = do
   am <- maybe (currentInterest ir acc) (interest ir <$>) maybeAm
   when (am /= 0) $
     fromTo am comment (iSource ir) acc
   return am
                                    

