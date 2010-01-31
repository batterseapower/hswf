module Primitives where

import TestUtilities


main = do
    run [0x00, 0x80, 0x07, 0x00] getFIXED `assertEquals` FIXED { fIXED_integer = 7, fIXED_decimal = 32768 } -- 7.5
    
    run [0x78] (aligned $ getUB 5) `assertEquals` 15
    run [0xE0] (aligned $ getSB 4) `assertEquals` (-2)
    run [0x46] (aligned $ getSB 7) `assertEquals` 35
    run [0x46] (aligned $ getSB 8) `assertEquals` 70
    run [0x11, 0x80] (aligned $ getSB 9) `assertEquals` 35
    -- These two test case were given in the specification with 0x30 instead of 0x60, but:
    -- 0x30 0x00 0x00 (19 bits) = 0011 0000 0000 0000 000x xxxx = 98304 (logical value), which doesn't match their result!
    -- 196608 (19 bits)         = 0110 0000 0000 0000 000x xxxx = 0x60 0x00 0x00
    run [0x60, 0x00, 0x00] (aligned $ getSB 19) `assertEquals` 196608
    run [0x60, 0x00, 0x00] (aligned $ getFB 19) `assertEquals` FIXED { fIXED_integer = 3, fIXED_decimal = 0 * 65536 } -- 7.5