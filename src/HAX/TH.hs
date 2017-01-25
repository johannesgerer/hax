{-# LANGUAGE TemplateHaskell
,NoMonomorphismRestriction
,OverloadedStrings
 #-}
module TH where

-- This module is not need, due to the automatic derivability of Enum and Ix

import Language.Haskell.TH
import Control.Applicative
import Control.Monad

-- ^ For ever element of the list define:  _element = (index of element)
mkAccounts :: [String] -> Q [Dec]
mkAccounts names = sequence $ zipWith decs [1..] names
  where decs num name = do
          let n1 = mkName $ "_" ++ name
          sigD n1 $ conT $ mkName "Int"
          funD n1 
            [clause [] (normalB $ litE $ IntegerL num ) []]

chartOfAccounts :: Name -> Q [Dec]
chartOfAccounts name = sequence $ return $ funD (mkName "accountNumber")
                       =<< zipWith dec [1..] <$> extractFields name
  where dec num f = clause [conP f []] (normalB $ litE $ IntegerL num ) []
  

extractFields :: Name -> Q [Name]
extractFields name = do
  i <- reify name
  let msg = unlines ["This is only supported for simple data"
                             ,"declarations with constructors without arguments"]
      cons = case i of
        TyConI (DataD _ _ _ cons _) -> cons
        _ -> error msg
      get (NormalC cname []) = cname
      get (NormalC cname _) = error msg
  return $ get <$> cons
