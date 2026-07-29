module Example.ConstConstructorLib
  ( Direction (..),
    Tagged (..),
    MyMaybe (..),
    Shape (..),
    directionOf,
    directionsOf,
    isNorth,
    taggedOff,
    taggedOn,
    myHead,
    unitSquare,
    ignoreDirection,
  )
where

-- | A three-constructor enumeration: every constructor is nullary, so
-- 'ConstConstructor' can switch any 'Direction'-typed expression to any of
-- the three.
data Direction = North | East | South
  deriving (Eq, Show)

-- | An enumeration with a type parameter.  Its constructors are
-- @forall a. Tagged a@, so a mutant has to instantiate that parameter; an
-- un-instantiated constructor would make the @ifMutation@ wrapper around it
-- ill-typed Core, which Core Lint rejects and which miscompiles the whole
-- site without it.  The @coreLint@ flag on the example's mutation check pins
-- this.
data Tagged a = Off | On
  deriving (Eq, Show)

-- | A hand-rolled 'Maybe': only one of its constructors is nullary, so
-- 'MyNothing' is the only constant of the type and the only replacement
-- 'ConstConstructor' offers.  'Maybe' itself is left to 'ConstNothing' and
-- 'MaybeOp'; this is the same mutation for a type they do not know about.
data MyMaybe a = MyNothing | MyJust a
  deriving (Eq, Show)

-- | No constructor of this type is nullary, so it has no constant to switch
-- to and 'ConstConstructor' never fires on a 'Shape'-typed expression.
data Shape = Circle Int | Square Int
  deriving (Eq, Show)

-- | The direction for a number, by parity.
--
-- Three kinds of 'ConstConstructor' site, all at arity 0:
--
--   * the whole @if@ expression, which is not constructor-headed, so all
--     three constructors are offered;
--   * the two branches, which are constructors, so each is replaced by the
--     two /other/ constructors and never by itself.
directionOf :: Int -> Direction
directionOf n = if even n then North else East

-- | The directions for a list of numbers.
--
-- @directionOf@ is passed to 'map' rather than applied, so
-- 'ConstConstructor' fires on it at arity 1 and produces @(\\_ -> North)@,
-- @(\\_ -> East)@ and @(\\_ -> South)@.
directionsOf :: [Int] -> [Direction]
directionsOf = map directionOf

-- | Whether a direction is north.
--
-- The @d == North@ comparison is 'Bool'-typed: 'Bool' has two nullary
-- constructors too, but 'ConstBool' and 'BoolLit' already produce exactly
-- the mutants 'ConstConstructor' would, so it is excluded and this
-- expression carries no 'ConstConstructor' site.  The two 'Direction'-typed
-- operands do carry one each.
isNorth :: Direction -> Bool
isNorth d = d == North

-- | A 'Tagged' value at a concrete type argument.  The only other
-- constructor is 'On', so 'ConstConstructor' produces exactly one mutant.
taggedOff :: Tagged Int
taggedOff = Off

-- | The same constant under a type signature.
--
-- 'ConstConstructor' has to see through the signature to recognise which
-- constructor this expression already is: without that, it offers 'On' as a
-- replacement for 'On', which is an equivalent mutant no test can kill.
taggedOn :: Tagged Int
taggedOn = On :: Tagged Int

-- | The head of a list, as a 'MyMaybe'.
--
-- The @MyJust x@ branch is replaced by 'MyNothing': a constructor
-- application mutated into a different constructor, which no other
-- constant-inserting operator does.  The @MyNothing@ branch already /is/ the
-- only constant of the type, so it is not a mutation site at all.
myHead :: [a] -> MyMaybe a
myHead [] = MyNothing
myHead (x : _) = MyJust x

-- | A 'Shape'-typed constant.  Only the integer literal is a mutation site.
unitSquare :: Shape
unitSquare = Square 1

-- | Throw a direction away.
--
-- @()@ has a single constructor, so there is no other constant to switch to
-- and 'ConstConstructor' does not fire.
ignoreDirection :: Direction -> ()
ignoreDirection _ = ()
