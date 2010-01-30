module Primitives where

import TestUtilities


main = run [0x78] (getUB 5) `assertEquals` 15