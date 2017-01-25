
import           Data.Ord
import qualified Data.Map.Strict as M
import           Control.Applicative
import           Control.Arrow
import           Text.Printf
import           Data.Function
import           Control.Monad.Writer.Lazy
import           Data.List hiding (init)
import           Prelude hiding (init)
import           Control.Monad.State.Lazy

type Account = String
type Amount = Double
type Comment = String
type Balances  =  M.Map Account Amount
type Month = Int
data Situation = Sit { sMonth :: Month
                     , sComment :: Comment
                     , sBals :: Balances }
type History  = [ Situation ]

type Evolution a = StateT History IO a 

instance Show Situation where
  show (Sit m c b) = printf ("%2d: %s\n %s") m c $ show $ M.toList b

showHistory :: Bool -- ^ end of month?
            -> History -> String
showHistory e = unlines . fmap show . reverse .
                (if e then endOfMonth else id)
        

accountError :: Account -> (b->c) -> Maybe b -> c
accountError a = maybe (error $ "Account '"++a++"' does not exist")
                   
getBal :: Account -> Evolution Double
getBal a = accountError a id . (M.lookup a) . sBals . head <$> get

           
getEndBalsSince :: Month -> Account -> Evolution [Double]
getEndBalsSince m a = fmap ((M.! a) . sBals) .
                      filter ((>= m).sMonth). endOfMonth <$> get

-- ^ ronud to number of given decimal places
roundTo :: Integer -> Double -> Double
roundTo i' = (/ i) . fromIntegral . round . (* i)
  where i = fromInteger $ 10 ^ i' 

cBals ::  (Balances -> Balances) -> Situation -> Situation
cBals f sit = sit{sBals = f $ sBals sit }
        
changeAmount ::  Account -> (Amount -> Amount) -> Balances -> Balances
changeAmount a f = M.alter (accountError a $ Just . roundTo 3 . f) a

changeSit :: Comment -> (Situation -> Situation) -> Evolution ()
changeSit c f  = modify $ \hist -> (f $ head hist){sComment=c} : hist

transfer :: Comment -> Account -> Account -> Amount -> Evolution ()
transfer c from to amount = if amount == 0 then return ()
                            else changeSit c $ cBals $
  changeAmount from (subtract amount) .
  changeAmount to   (+        amount)

incrMonth :: Evolution ()
incrMonth = changeSit "Neuer Monat" $ \sit -> sit { sMonth = sMonth sit + 1 }

curMonth :: Evolution Month
curMonth = sMonth.head <$> get

init :: [Account] -> History
init =  return . Sit 1 "" . M.fromList . fmap (flip (,) 0)

-- ^ limit History to end-of-month balances.  History needs to be sorted by month in  descending order
endOfMonth :: History -> History
endOfMonth = fmap head . groupBy ((==) `on` sMonth)

data InterestRate = InterestRate { iCredit :: Amount
                                 , iDebit :: Amount }

-- ^ transfer interest triggered by the average balances over the N last month
transferInterest :: Month -- ^ N
                 -> InterestRate -> Account -- ^ on this account
                 -> Account -- ^ to this account
                 -> Evolution ()
transferInterest n ir trigger counterparty = do
 -- get >>= liftIO . putStrLn . showHistory True
 -- liftIO $ putStrLn "stop"
 m <- curMonth
 (credit,debit) <- partition (>0) <$> getEndBalsSince (m-n+1) trigger
 transfer "Habenzins" "Einnahmen" counterparty $ (* (iCredit ir)) $ avg credit
 transfer "Sollzins" "Aufwendungen" counterparty $ (* (iDebit ir)) $ avg debit
 where avg m = sum m / fromIntegral n
   
                                             

-- ########################   Modell ###########################################

accounts = [ "Aufwendungen"
           , "Eigenkapital"
           , "Einnahmen"
           , "Giro"
           , "Bank"
           ]

zGiro = InterestRate 0.005 0.12

month :: Evolution ()
month = do
  m <- curMonth

  -- Initial balances
  when (m == 1) $ transfer "Eigenkapital" "Giro" "Eigenkapital" $ 100
  when (m == 4) $ transfer "Eigenkapital" "Giro" "Eigenkapital" $ negate 200  

  when (m `mod` 12 == 0) $
    transferInterest 12 zGiro "Giro" "Giro"
  
  
  --Zinsentstehung
  -- zinsen <- min 0 . (* (zGiroDispo/12)) <$> getBal "Giro"
  -- transfer "Aufwendungen" "Bank" dispoZinsen

  --Zinsbegleichung
  -- when (m `mod` 12 == 0) $
  --   transfer "Bank" "Giro" =<< getBal "Bank"

  -- Monat abschließen 
  incrMonth

main = do (_,s) <- runStateT ( replicateM_ 24 month ) $ init accounts
          putStrLn $ showHistory False s
          return ()
