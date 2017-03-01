{-# LANGUAGE DataKinds #-}
module FloatAddSingle where

import CLaSH.Prelude

import Data.Bool
import Data.Tuple.Select

-- Single precision floating-point add.
fadds :: BitVector 32 -> BitVector 32 -> BitVector 32
fadds a b = result
    where
    opA = unpackFloatS a
    opB = unpackFloatS b
   
    (expDiff, swap) = exponentDiff (sel2 opA) (sel2 opB)

    (opA', opB') = bool (opA, opB) (opB, opA) swap

    (signA, expA, mantA) = opA'
    (signB, expB, mantB) = opB'

    (mantB', g, r, s) = preAlignShift mantB expDiff

    (mantRes, overflow) = mantA `mantAdd` mantB'

    leadingOneIdx = leadingOneDetector mantRes

    mantRes' :: Unsigned 24
    mantRes'
        | overflow  = unpack $ (1 :: Bit) ++# (slice d23 d1 mantRes)
        | otherwise = normalizationShift mantRes g leadingOneIdx 

    expRes :: Unsigned 8
    expRes
        | overflow  = expA + 1
        | otherwise = normalizeExponent expA leadingOneIdx

    result = (0 :: Bit) ++# pack expRes ++# (slice d22 d0 mantRes')

-- Unpack single precision float into sign, exponent and mantissa.
-- Implied leading 1 is appended to mantissa
unpackFloatS :: BitVector 32 -> (Bit, Unsigned 8, Unsigned 24)
unpackFloatS a = (sign, exponent, mantissa)
    where
    sign     = slice d31 d31 a
    exponent = unpack $ slice d30 d23 a
    mantissa = unpack $ (1 :: Bit) ++# slice d22 d0 a


exponentToSigned :: Unsigned 8 -> Signed 8
exponentToSigned a = fromIntegral a - 127



-- Returns absolute difference between exponents and whether or not a swap is needed
exponentDiff :: Unsigned 8 -> Unsigned 8 -> (Unsigned 8, Bool)
exponentDiff a b = (diff, swap)
    where
    swap = b > a
    diff
        | swap      = b - a
        | otherwise = a - b

mantissaSwap :: Unsigned 24 -> Unsigned 24 -> Bool -> (Unsigned 24, Unsigned 24)
mantissaSwap a b swap
    | swap      = (b, a)
    | otherwise = (a, b)

preAlignShift :: Unsigned 24 -> Unsigned 8 -> (Unsigned 24, Bit, Bit, Bit)
preAlignShift mant shiftAmount = (shiftedMant, guard, round, sticky)
    where
    padded  = pack mant ++# (0 :: BitVector 26) -- shifter becomes 50-bits
    shifted = padded `shiftR` fromIntegral shiftAmount
    shiftedMant = unpack $ slice d49 d26 shifted
    guard  = slice d25 d25 shifted
    round  = slice d24 d24 shifted
    sticky = reduceOr $ slice d23 d0 shifted

twosComplement :: Unsigned 24 -> Unsigned 24
twosComplement a  = complement a + 1

mantAdd :: Unsigned 24 -> Unsigned 24 -> (Unsigned 24, Bool)
mantAdd a b = (result, overflow)
    where
    temp = (resize a :: Unsigned 25) + (resize b :: Unsigned 25)
    result = unpack $ slice d23 d0 temp
    overflow = slice d24 d24 temp == high

-- Returns index of leading one
leadingOneDetector :: Unsigned 24 -> Index 24
leadingOneDetector a = fromIntegral $ countLeadingZeros a

-- If there are leading zeroes, shift mantissa left until no more leading zeroes.
-- Shift in the guard bit.
normalizationShift :: Unsigned 24 -> Bit -> Index 24 -> Unsigned 24
normalizationShift mant guard shiftAmount = result
    where
    temp = pack mant ++# guard
    shifted = temp `shiftL` fromIntegral shiftAmount
    result = unpack $ slice d24 d1 shifted

normalizeExponent :: Unsigned 8 -> Index 24 -> Unsigned 8
normalizeExponent exp shiftAmount = exp - (fromIntegral shiftAmount)


