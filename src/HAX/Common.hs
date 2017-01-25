{-# LANGUAGE NoMonomorphismRestriction
,  TypeSynonymInstances
,  FlexibleInstances
 #-}

-- | This module provides some common functions and reexports basic
-- modules.
module HAX.Common (
  -- * Basic Types
   Amount
  , Comment

  -- * Ranges and Arrays
  , range1
  , bounds1
  , range2
  , bounds2
  , updateArray
  , assocArray
    
   -- * Utility functions
  , when'
  , both
  , conv
  , positivePart
  , assert
  , PList(..)
    
  -- * Dates
  , ADate()
  , date
  , endOfYear
  , month
  , yearMonth
  , getMonth
  , getYear

  -- * Date spans

  , ASpan(..)
  , months
  , yearMonthSpan
  , yearSpan
  , divides
  , dateSpan
  , shift

   -- * Reexported modules
  ,module Control.Applicative
  ,module Control.Arrow
  ,module Control.Monad
  ,module Control.Monad.RWS.Strict
  ,module Data.Array
  ,module Data.Array.IO
  ,module Data.Decimal
  ,module Data.Function
  ,module Data.List
  ,module Data.Maybe
  ,module Data.Monoid
  ,module Data.Ord
  ,module Data.Ratio
  ,module Data.String
  ,module Data.Tuple
  ,module Text.Printf
  ,module Text.Show
  ) where 

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans (lift)
import Data.Array
import Data.Array.IO
import Data.Decimal
import Data.Function
import Data.List hiding (span)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.String
import Data.Tuple
import Prelude hiding (span)
import Text.Printf
import Text.Show

type Amount = Decimal
type Comment = String

-- | accounting dates
data ADate = UNSAFE_ADate { dUNSAFE_Months :: Int -- ^ 0 corresponds to January 0
                          }
          deriving (Eq,Ord,Ix)

-- | represents a time span between accounting dates
data ASpan = ASpan { sMonths :: Int -- ^ 0 corresponds to 0 time difference
                          }
          deriving (Eq,Ord,Ix)

getMonth :: ADate -> Int
getMonth = snd . yearMonth

getYear :: ADate -> Int
getYear = fst . yearMonth


-- | convert a month to a pair of 'Int's.
yearMonth :: ADate -> (Int , Int) -- ^ (year,month)
yearMonth (UNSAFE_ADate m) = second succ $ m `divMod` 12

-- | Create an 'ADate' value that corresponds to the last month of the
-- given year.
endOfYear :: Int -> ADate
endOfYear y = UNSAFE_ADate $ 12 * (y+1) -1

-- | Create an 'ADate' value that corresponds to the given month in
-- year 0. (Fails if argument is outside of [1..12])
month :: Int -> ADate
month m  = if m >12 || m<1 then error $ printf "Bad Month: %v" m
               else UNSAFE_ADate $ m - 1

-- | Create an 'ASpan' value corresponding to the given number of
-- monhts.
months :: Int -> ASpan
months = ASpan

-- | convert a span to a pair of 'Int's.
yearMonthSpan :: ASpan -> (Int , Int) -- ^ (year,month)
yearMonthSpan (ASpan m) =  m `divMod` 12
           
yearSpan :: ASpan -> Int
yearSpan = fst . yearMonthSpan 

-- | Calculate the span between two dates
dateSpan :: ADate -- ^ start date
     -> ADate -- ^ end date
     -> ASpan
dateSpan a b = ASpan $ on (-) dUNSAFE_Months  b a

-- | Shift a date by a given date span.
shift :: ASpan -> ADate -> ADate
shift s d = UNSAFE_ADate $ dUNSAFE_Months d + sMonths s
                    
-- | Construct an 'ADate' value from a month an a year
date :: Int -> Int -> ADate
date m y = if y < 1800 then error $ printf "Year %v is earlier than 1800???" y
           else shift (ASpan $ 12 * y) $ month m

instance Show ASpan where
  show s= show (sMonths s) ++ " months"

instance Show ADate where
  show = (\(y,m) -> printf "%2d/%02d" m $ y`mod` 100) . yearMonth

instance PrintfArg (PList Decimal) where
  formatArg ds _ = showListWith showsD $ pList ds

newtype PList a = PList { pList :: [a] }

instance PrintfArg ADate where
  formatArg = formatString . show

-- | Check if the second span is a multiple of the first.
divides :: ASpan -> ASpan -> Bool
divides a b = forSpan2 mod b a == 0
            
forSpan2 f = (ASpan .) . on f sMonths
forSpan1 f =  ASpan . f . sMonths

-- 'ASpan's support the usual arithmetics
instance Num ASpan where
  (+) = forSpan2 (+)
  (*) = forSpan2 (*)
  (-) = forSpan2 (-)
  fromInteger = ASpan . fromInteger
  abs = forSpan1 abs
  signum = forSpan1 signum

-- | helper function to update an MArray value
updateArray :: (MArray a e m, Ix i) => a i e -> i -> (e->e) -> m ()
updateArray a i f = readArray a i >>= writeArray a i . f

                    
instance (Show a, Integral a) => PrintfArg (DecimalRaw a) where
  formatArg d _ = showsD d
  -- parseFormat = error "printf parsing not implemented for DecimalRaw"

showsD = shows . roundTo 2 . (/1000)


-- | Get the range of the first dimension of an array
range1 = range . bounds1
-- | Get the bounds of the first dimension of an array
bounds1 = both fst
          
-- | Get the range of the second dimension of an array
range2 = range . bounds2
-- | Get the bounds of the second dimension of an array
bounds2 = both snd


-- | apply a function to both elements of a pair
both :: (b -> c) -> (b, b) -> (c, c)
both = join (***)

-- | apply a function to both elements of a pair
bothM :: Monad m => (b -> m c) -> (b, b) -> m (c, c)
bothM f (a,b) = do a' <- f a
                   b' <- f b
                   return (a',b')

-- -- more general
-- ,RankNTypes
-- ,ScopedTypeVariables
-- ,ConstraintKinds
-- both :: (as a1, as a2) => (forall a. as a => a->b) -> (a1,a2) -> (b,b)
-- both f = f *** f

-- | Return a value only if the condition holds and zero otherwise.
when' :: Num a => Bool -> a -> a
when' a b = if a then b else 0


-- | Conversion between different fractional types. E.g. between
-- 'Double' and 'Decimal'.
conv :: (Real a, Fractional c) => a -> c
conv = (fromRational . toRational)


-- | generate an array and take the bounds from the input data
assocArray :: Ix i =>
             [(i, e)] -- ^ a list of associations of the form (index, value)
          -> Array i e
assocArray assocs = array (minimum indx,maximum indx) assocs
  where indx = fst <$> assocs

-- | bound checking array acces with error msg
-- (?!) :: Ix i => Array i e -> (String,i) -> e
-- array ?! (String,index) = if bounds array `inRange` index
--                           then array ! index
--                           else error index 

-- | take the positive par of a 'Num'
positivePart a = max a 0

-- | assert that the result of an action satisfies a certain condition or print an error message
assert condition getAm msg = do
  am <- getAm
  when (not $ condition am) $ lift $ putStrLn $ "ASSERTION FAILED: " ++ msg
  return am
