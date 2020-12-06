{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.HList where

data HList (r :: [*]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

class HContains (l :: [*]) a where
  getElem :: HList l -> a

instance HContains '[] () where
  getElem HNil = ()

instance HContains l (HList l) where
  getElem = id

instance HContains '[a] a where
  getElem (HCons a _) = a

instance HContains l a => HContains (a ': l) a where
  getElem (HCons a _) = a

instance HContains l a => HContains (b ': l) a where
  getElem (HCons _ hl) = getElem hl
