{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.HList where

import Data.Kind

data HList (r :: [Type]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

type family HContains (xs :: [Type]) (x :: Type) :: Constraint where
  HContains '[] x = x ~ ()
  HContains (x ': xs) x = ()
  HContains (_ ': xs) x = HContains xs x

getElem :: HContains l a => HList l -> a
getElem = \case
  HNil -> ()
  HCons (e :: a) _ -> e
  HCons _ x -> getElem x
