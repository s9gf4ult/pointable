module Pointable.Lib where

import Language.Haskell.TH

data FieldDesc = FieldDesc
  { fieldName    :: String
  , fieldType    :: Type
  , fieldSumType :: Type
  }

data ProdDesc = ProdDesc
  { prodName   :: String
  , prodFields :: [FieldDesc]
  }

noBang :: Bang
noBang = Bang NoSourceUnpackedness NoSourceStrictness

genProd :: ProdDesc -> Q [Dec]
genProd desc = do
  let
    prodType =
      let
        con = (error "FIXME: ")
        dec = DataD [] (mkName $ prodName desc) [] Nothing [con] []
      in dec
    sumType =
      let
        sumName = "Point" ++ prodName desc
        toSumConstr fd =
          NormalC (mkName $ sumName ++ fieldName fd) [(noBang, fieldSumType fd)]
        cons = map toSumConstr $ prodFields desc
        dec = DataD [] (mkName sumName) [] Nothing cons []
      in dec
  return [prodType, sumType]
