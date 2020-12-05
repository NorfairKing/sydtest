{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Syd.HList where

data HList (r :: [*]) where
  HNil :: HList '[]
  HCons :: e -> HList l -> HList (e ': l)

class HContains a b where
  getElem :: a -> b

instance HContains (HList '[]) () where
  getElem HNil = ()

instance HContains (HList '[a]) a where
  getElem (HCons a _) = a

instance HContains (HList l) a => HContains (HList (a ': l)) a where
  getElem (HCons a _) = getElem a

instance HContains (HList l) a => HContains (HList (b ': l)) a where
  getElem (HCons _ hl) = getElem hl

instance {-# OVERLAPPING #-} HContains a a where
  getElem = id
