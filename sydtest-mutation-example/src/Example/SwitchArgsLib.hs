module Example.SwitchArgsLib
  ( divideThe,
    parenDivide,
    RGB (..),
    makeColour,
    greyscale,
  )
where

-- | Integer division.  Both arguments are 'Int', so
-- 'SwitchFunctionArguments' produces one mutant that swaps them: @div b a@
-- instead of @div a b@.
divideThe :: Int -> Int -> Int
divideThe a b = div a b

-- | A parenthesised application: the parentheses around @div a b@ are
-- required for precedence, so GHC keeps an 'HsPar' node around it.  The swap
-- mutation must be produced exactly once -- not once for the parenthesis node
-- and once for the inner application.
parenDivide :: Int -> Int -> Int
parenDivide a b = succ (div a b)

-- | A colour built from three 'Int' components.
data RGB = RGB Int Int Int
  deriving (Eq, Show)

-- | All three arguments are 'Int', so 'SwitchFunctionArguments' produces one
-- mutant per pair: swapping (r, g), (r, b), and (g, b).
makeColour :: Int -> Int -> Int -> RGB
makeColour r g b = RGB r g b

-- | The same constructor applied to three textually identical arguments.
-- Every swap would be a no-op, so 'SwitchFunctionArguments' produces nothing.
greyscale :: Int -> RGB
greyscale v = RGB v v v
