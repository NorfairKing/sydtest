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
    (?:),
    pick,
    myHead,
    unitSquare,
    ignoreDirection,
    noDirections,
    northAt,
    constantDirection,
    homeDirection,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

-- | The same constant under an inline type signature.
--
-- The mutation site is the constructor itself, not the signature node, so
-- 'ConstConstructor' sees which constructor this expression already is and
-- offers only 'Off'.  Offering 'On' as a replacement for 'On' would be an
-- equivalent mutant no test can kill.
taggedOn :: Tagged Int
taggedOn = On :: Tagged Int

-- | Pick between two directions by the parity of a third argument.
--
-- The third argument makes an infix use of this operator a /partial/
-- application, which is the shape that lands a mutation on the operator token
-- itself: 'ConstConstructor' matches the bare @?:@ at arity 3.  Splicing a
-- lambda into the operator slot would not reparse, so the preview has to
-- rewrite the whole infix expression in prefix form instead.
(?:) :: Direction -> Direction -> Int -> Direction
(?:) a b n = if even n then a else b

-- | The direction for a number, chosen by an infix partial application.
pick :: Int -> Direction
pick = North ?: East

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

-- | An empty 'Map', written through the alias containers exports for it.
--
-- @Map@ has two constructors, one of them the nullary @Tip@, so the type
-- qualifies.  But @Map.empty@ /is/ @Tip@ -- it is an ordinary function whose
-- entire definition is that constructor -- so replacing this expression with
-- @Tip@ is an equivalent mutant no test can kill.  It is the same no-op the
-- operator already declines to offer for @taggedOn@, reached through a
-- function rather than by writing the constructor out, so this expression
-- carries no 'ConstConstructor' site.
noDirections :: Map Int Direction
noDirections = Map.empty

-- | A 'Map' that is not empty.
--
-- The equivalence above is a property of the expression, not of the type:
-- emptying a map that holds something is a real mutation, so
-- 'ConstConstructor' does fire here and offers @Tip@.
northAt :: Int -> Map Int Direction
northAt n = Map.singleton n North

-- | A function that answers with the same constructor whatever it is given.
--
-- Which functions those are is a semantic property, so unlike @Map.empty@
-- above there is nothing in the compiler to read it off: the in-repo mutation
-- config lists @constantDirection@ under the operator's @skip-calls-to@ key.
-- Without that entry the call in 'homeDirection' would carry a mutation to
-- each of the three constructors, all three equivalent and unkillable.
--
-- The key skips /calls/, so the body below is a mutation site like any
-- other: switching this @North@ to @East@ changes what the function answers,
-- and the test kills it.
--
-- The argument is @()@ to keep the example free of sites that have nothing
-- to do with this operator: a literal argument would be one, and an
-- unforced one at that, since the pattern here does not look at what it is
-- given.
constantDirection :: () -> Direction
constantDirection () = North

-- | A call to a function listed under @skip-calls-to@, which therefore
-- carries no 'ConstConstructor' site.
homeDirection :: Direction
homeDirection = constantDirection ()
