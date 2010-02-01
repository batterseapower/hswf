module VanillaRect where

import TestUtilities


main = runGet vanilla_rect_bytes getRECT `assertEquals` vanilla_rect

-- From the specification: tests handling of bitfields and SB values
vanilla_rect_bytes = [
    0x78, 0x00, 0x05, 0x5F, 0x00, 0x00, 0x0F, 0xA0, 0x00
  ]

vanilla_rect = RECT { rECT_xmin = 0, rECT_xmax = 11000, rECT_ymin = 0, rECT_ymax = 8000 }
