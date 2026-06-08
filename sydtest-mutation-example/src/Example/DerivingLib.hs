module Example.DerivingLib
  ( Color (..),
    Wrapper (..),
  )
where

-- | A stock-derived enumeration.  Deriving 'Eq', 'Ord', 'Show', 'Read',
-- 'Enum' and 'Bounded' makes GHC generate instance-method code whose source
-- spans point back at this @deriving@ clause.  That code is compiler-generated,
-- not user-written, so the plugin must skip it.
--
-- Before the fix, the const-family operators fired on the derived methods (the
-- 'Bool' results of derived '==' / 'compare', the list results of derived
-- 'enumFrom', the 'String' results of derived 'show', ...), producing nonsense
-- mutants like @deriving (Show, (\\_ _ -> False), ...)@.  Those mutants can
-- never be killed by a test of the user's own code, so they are pure noise.
data Color = Red | Green | Blue
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | A newtype whose 'Enum' instance defines only 'toEnum' and 'fromEnum'; GHC
-- fills in the rest ('enumFrom', 'enumFromTo', ...) from the class defaults.
-- Those default-method copies are generated bindings attributed to the
-- instance head, so they must be skipped too — this mirrors the
-- @instance (Enum X) => Enum Y where toEnum = ...@ pattern that surfaced the
-- bug downstream (a const-empty-list mutant @(\\_ -> [])@ replacing the whole
-- instance head).
--
-- The two methods we do write are constructor- and variable-headed, so they
-- contribute no source-level mutation sites: the whole module's manifest is
-- therefore expected to be empty.
newtype Wrapper = Wrapper Int

instance Enum Wrapper where
  toEnum = Wrapper
  fromEnum (Wrapper n) = n
