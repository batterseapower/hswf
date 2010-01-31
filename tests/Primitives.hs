module Primitives where

import TestUtilities


main = do
    let run getter putter ws x = do
          runGet ws getter `assertEquals` x
          runPut (putter x) `assertEquals` ws
  
    run getFIXED putFIXED [0x00, 0x80, 0x07, 0x00] $ FIXED { fIXED_integer = 7, fIXED_decimal = 32768 } -- 7.5
    
    -- seeeeeeeemmmmmmmmmmmmmmmmmmmmmmm
    -- 01111111100000000000000000000000 = 0x7F800000 = +Infinity
    run getFLOAT putFLOAT [0x00, 0x00, 0x80, 0x7F] (1/0)
    
    -- seeeeemmmmmmmmmm
    -- 0100000000000000 = 0x4000 = +1.0 * 2^(16 - 16) = 1
    -- 0100001000000000 = 0x4200 = +1.5 * 2^(16 - 16) = 1.5
    run getFLOAT16 putFLOAT16 [0x00, 0x40] $ floatToFLOAT16 1
    run getFLOAT16 putFLOAT16 [0x00, 0x42] $ floatToFLOAT16 1.5
    -- 0111110000000000 = 0x7C00 = +Infinity
    -- 1111110000000000 = 0xFC00 = -Infinity
    run getFLOAT16 putFLOAT16 [0x00, 0x7C] $ floatToFLOAT16 (1/0)
    run getFLOAT16 putFLOAT16 [0x00, 0xFC] $ floatToFLOAT16 (-1/0)
    -- 0111110000010000 = 0x7C10 = NaN
    -- 1111110000010000 = 0xFC10 = NaN
    let fLOAT16NaN = floatToFLOAT16 (0/0)
        assertNaN x = if isNaN (fLOAT16ToFloat x) then return () else sayNotEqual ["assertNaN"] x fLOAT16NaN
    assertNaN $ runGet [0x10, 0x7C] getFLOAT16
    assertNaN $ runGet [0x10, 0xFC] getFLOAT16
    runPut (putFLOAT16 fLOAT16NaN) `assertEquals` [0xFF, 0x7F]
    
    run (aligned $ getUB 5) (flushed . putUB 5) [0x78] 15
    run (aligned $ getSB 4) (flushed . putSB 4) [0xE0] (-2)
    run (aligned $ getSB 7) (flushed . putSB 7) [0x46] 35
    run (aligned $ getSB 8) (flushed . putSB 8) [0x46] 70
    run (aligned $ getSB 9) (flushed . putSB 9) [0x11, 0x80] 35
    -- These two test case were given in the specification with 0x30 instead of 0x60, but:
    -- 0x30 0x00 0x00 (19 bits) = 0011 0000 0000 0000 000x xxxx = 98304 (logical value), which doesn't match their result!
    -- 196608 (19 bits)         = 0110 0000 0000 0000 000x xxxx = 0x60 0x00 0x00
    run (aligned $ getSB 19) (flushed . putSB 19) [0x60, 0x00, 0x00] 196608
    run (aligned $ getFB 19) (flushed . putFB 19) [0x60, 0x00, 0x00] $ FIXED { fIXED_integer = 3, fIXED_decimal = 0 * 65536 } -- 7.5
