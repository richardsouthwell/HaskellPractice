import GHC.Read
import GHC.TypeLits
import GHC.Generics
import Control.DeepSeq
import Control.Monad
import Data.Ratio
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec

-- | Finite number type. @'Finite' n@ is inhabited by exactly @n@ values. Invariants:
--
-- prop> getFinite x < natVal x
-- prop> getFinite x >= 0
newtype Finite (n :: Nat) = Finite Integer
                          deriving (Eq, Ord, Generic)

-- | Convert an 'Integer' into a 'Finite', throwing an error if the input is out of bounds.
finite :: KnownNat n => Integer -> Finite n
finite x = result
    where
        result = if x < natVal result && x >= 0
            then Finite x
            else error $ "finite: Integer " ++ show x ++ " is not representable in Finite " ++ show (natVal result)

-- | Convert a 'Finite' into the corresponding 'Integer'.
getFinite :: Finite n -> Integer
getFinite (Finite x) = x