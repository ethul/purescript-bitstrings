-- | https://github.com/dwdyer/uncommons-maths/blob/master/core/src/java/main/org/uncommons/maths/binary/BitString.java
-- | https://github.com/unnoon/cell-bitset/blob/master/src/BitSet.js
module Data.BitString
  ( BitString
  , singleton
  , cons
  , snoc

  , length
  , index, (!!)
  , updateAt
  , append
  , bitCount

  , toArrayLittleEndian
  , toArrayBigEndian
  , toStringLittleEndian
  , toStringBigEndian

  , fromNumber
  , fromInt


  , test1
  , test2
  , test3
  ) where

import Prelude hiding (append)

import Data.Array as Array
import Data.Foldable (foldMap, foldl)
import Data.Generic as Generic
import Data.Generic.Rep as GenericRep
import Data.Int (ceil, floor, toNumber)
import Data.Int.Bits as Bits
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Additive (Additive)
import Data.Newtype (wrap, unwrap)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

import Math as Math

-- | BitString representation stored as an array of numbers in base `2^32`
-- |
-- | The BitString is stored in little-endian order where the least
-- | significant  bit is  stored in  the  number at  the first index
-- | of the array.

newtype BitString = BitString (Array Number)

singleton :: Boolean -> BitString
singleton a = BitString (pure (if a then one else zero))

length :: BitString -> Int
length (BitString as) = (Array.length as - one) * bits + length'
  where
  length' :: Int
  length' = maybe zero log2' (Array.head as)
    where
    log2' :: Number -> Int
    log2' a
      | a < 2.0 = one
      | otherwise = ceil (log2 a)

index :: BitString -> Int -> Maybe Boolean
index as @ (BitString as') i
  | i < 0 = Nothing
  | i >= length as = Nothing
  | otherwise = (\x -> bitsAnd (bitsZshr x (toNumber i)) one /= zero) <$> Array.index as' (slot i)

infixl 8 index as !!

cons :: Boolean -> BitString -> BitString
cons a as = singleton a <> as

snoc :: BitString -> Boolean -> BitString
snoc as a = as <> singleton a

updateAt :: Int -> Boolean-> BitString -> Maybe BitString
updateAt i a (BitString as) = BitString <$> Array.modifyAt (slot i) value as
  where
  value :: Number -> Number
  value =
    if not a
       then bitsAnd (bitsComplement (bitsShl one (toNumber i)))
       else bitsOr (bitsShl one (toNumber i))

bitCount :: BitString -> Int
bitCount (BitString as) = unwrap (foldMap bitCount' as)
  where
  bitCount' :: Number -> Additive Int
  bitCount' a = wrap (ceil (bitsZshr ((bitsAnd (x + bitsZshr x 4.0) (toNumber 0xF0F0F0F)) * (toNumber 0x1010101)) 24.0))
    where
    x :: Number
    x = bitsAnd y (toNumber 0x33333333) + bitsAnd (bitsZshr y 2.0) (toNumber 0x33333333)

    y :: Number
    y = a - bitsAnd (bitsZshr a one) (toNumber 0x55555555)

-- take :: Int -> BitString -> BitString
-- take n as = ?take

append :: BitString -> BitString -> BitString
append (as @ BitString as') (bs @ BitString bs') = append' add
  where
  append' :: Tuple (Array Number) Number -> BitString
  append' (Tuple xs c)
    | c == zero = BitString xs
    | otherwise = BitString (xs <> pure c)

  add :: Tuple (Array Number) Number
  add = foldl go (Tuple mempty zero) (Array.range zero (max' - one))
    where
    max' :: Int
    max' = max (Array.length as') (Array.length bs')

    go :: Tuple (Array Number) Number -> Int -> Tuple (Array Number) Number
    go (Tuple acc c) i =
      let a = fromMaybe zero (Array.index as' i)
          b = fromMaybe zero (Array.index bs' i)
          x = a + b + c
       in if x >= base
             then Tuple (acc <> pure (x - base)) one
             else Tuple (acc <> pure x) zero

fromNumber :: Number -> BitString
fromNumber x = fromNumber' sub
  where
  fromNumber' :: Tuple (Array Number) Number -> BitString
  fromNumber' (Tuple xs c)
    | c == zero = BitString xs
    | otherwise = BitString (pure c <> xs)

  sub :: Tuple (Array Number) Number
  sub = foldl go (Tuple mempty x) (Array.reverse (Array.range zero size))
    where
    size :: Int
    size = floor (log x)

    go :: Tuple (Array Number) Number -> Int -> Tuple (Array Number) Number
    go (Tuple acc c) i =
      let a = pow (toNumber i)
          b = Math.floor (c / a)
       in Tuple (pure b <> acc) (c - a * b)

fromInt :: Int -> BitString
fromInt = fromNumber <<< toNumber

toArrayLittleEndian :: BitString -> Array Boolean
toArrayLittleEndian as = fromMaybe mempty (sequence (index as <$> Array.range zero (length as - one)))

toArrayBigEndian :: BitString -> Array Boolean
toArrayBigEndian = Array.reverse <<< toArrayLittleEndian

toStringLittleEndian :: BitString -> String
toStringLittleEndian = foldMap toStringBoolean <<< toArrayLittleEndian

toStringBigEndian :: BitString -> String
toStringBigEndian = foldMap toStringBoolean <<< toArrayBigEndian

toStringBoolean :: Boolean -> String
toStringBoolean = if _ then "1" else "0"

bits :: Int
bits = 32

slot :: Int -> Int
slot i = Bits.zshr i (ceil (log2 (toNumber bits)))

base :: Number
base = Math.pow 2.0 (toNumber bits)

log2 :: Number -> Number
log2 a = Math.log a * Math.log2e

log :: Number -> Number
log a = Math.log a / Math.log base

pow :: Number -> Number
pow = Math.pow base

instance semigroupBitString :: Semigroup BitString where
  append = append

instance monoidBitString :: Monoid BitString where
  mempty = BitString mempty

instance ordBitString :: Ord BitString where
  compare (BitString as) (BitString bs) = Array.reverse as `compare` Array.reverse bs

derive newtype instance eqBitString :: Eq BitString

derive newtype instance showBitString :: Show BitString

derive instance genericRepBitString :: GenericRep.Generic BitString _

derive instance genericBitString :: Generic.Generic BitString

foreign import bitsAnd :: Number -> Number -> Number

foreign import bitsOr :: Number -> Number -> Number

foreign import bitsComplement :: Number -> Number

foreign import bitsShl :: Number -> Number -> Number

foreign import bitsZshr :: Number -> Number -> Number





test1 :: Array Number -> BitString
test1 as = BitString as
test2 :: String
test2 = toStringBigEndian test3
test3 :: BitString
test3 = BitString [ 4294967294.0 ] <> BitString [ 1.0 ] <> BitString [ 1.0 ] <> BitString [ 1.0 ]
