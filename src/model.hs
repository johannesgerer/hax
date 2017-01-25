import qualified Data.Map.Strict as M
import Control.Applicative

type Account = String
type Balances  =  M.Map Account Double
type State = (Int, Balances)
type Evolution = [ State ]
type Month = Int
type Transaction = Account -> Account -> Double -> State -> State
type Step  = Evolution -> Evolution
             
accounts = [ "Aufwendungen"
           , "Einnahmen"
           , "Giro"
           , "Bank"
           , "Startkapital"
           ]

a = accounts !! 1
b = accounts !! 2

zGiroDispo = 0.12 :: Double

initial :: Balances
initial =  M.fromList $ (flip (,) 0) <$> accounts

transaction :: Transaction
transaction a1 a2 am = second $ M.update (Just . (subtract am)) a2
                       . M.update (Just . (+ am)) a1

timeStep :: State -> State
timeStep = first (+ 1)

step :: Step
step (bals:ev) = [ foldr1 (.) transactions bals , bals] ++ ev
  where transactions = ( if length ev == 0 then
                         [ transaction "Startkapital" "Giro" $ 100 ]
                         else
                           [ transaction "Bank" "Aufwendungen"
                             $ bals M.! "Giro" * zGiroDispo / 12] )
                       ++ if l `mod` 12 == 0 then
             [ transaction "Giro" "Bank" $ bals M.! "Bank" ]
             else []
        l = length ev

main = putStrLn $ unlines $ fmap show $ zip [0..] $ reverse
       $ M.toList <$>  ( iterate step [initial] !! 19 )
