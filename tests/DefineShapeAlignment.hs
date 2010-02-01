module DefineShapeAlignment where

import TestUtilities


main = runGet define_shape_bytes getDefineShape `assertEquals` define_shape

-- Observed in flash-gordon/blue.swf: if MATRIX records don't end by being
-- byte aligned then this will parse incorrectly (the gradient list will be
-- empty because you confuse padding with part of the GRADIENT entry).
define_shape_bytes = [
    0x16, 0x00, 0x86, 0x4A, 0xF2, 0xD9, 0xAE, 0x3E,
    0x71, 0xBB, 0xD8, 0x01, 0x10, 0x95, 0x2D, 0xC3,
    0x0A, 0x79, 0x81, 0x91, 0xE4, 0xE2, 0xAB, 0xE1,
    0x80, 0x02, 0x00, 0xFF, 0xFF, 0xFF, 0xFF, 0x10,
    0x59, 0x9C, 0x00, 0x10, 0x16, 0x0B, 0x66, 0xA6,
    0xB5, 0x3B, 0xAC, 0xAA, 0x40, 0xCC, 0xAE, 0xE0,
    0x3F, 0x65, 0x7E, 0x64, 0x57, 0xBF, 0x12, 0x3A,
    0xFF, 0x2D, 0xBB, 0x60, 0x00
  ]

define_shape = DefineShape {
    defineShape_shapeId = 22,
    defineShape_shapeBounds = RECT {
        rECT_nbits = 16,
        rECT_xmin = -13986,
        rECT_xmax = 23349,
        rECT_ymin = -14386,
        rECT_ymax = 14203},
    defineShape_shapes = SHAPEWITHSTYLE {
        sHAPEWITHSTYLE_fillStyles = [
            GradientFill {
                fILLSTYLE_linearRadial = Linear,
                fILLSTYLE_gradientMatrix = MATRIX {
                    mATRIX_scale = Just (FIXED {fIXED_decimal = 9, fIXED_integer = 0},FIXED {fIXED_decimal = 13, fIXED_integer = 0}),
                    mATRIX_rotate = Just (FIXED {fIXED_decimal = 49822, fIXED_integer = 0},FIXED {fIXED_decimal = 24676, fIXED_integer = 0}),
                    mATRIX_translateX = 5002,
                    mATRIX_translateY = -10301
                  },
                fILLSTYLE_gradient = GRADIENT {
                    gRADIENT_spreadMode = 0,
                    gRADIENT_interpolationMode = 0,
                    gRADIENT_gradientRecords = [
                        GRADRECORD {
                            gRADRECORD_ratio = 0,
                            gRADRECORD_color = Left (RGB {
                                rGB_red = 255,
                                rGB_green = 255,
                                rGB_blue = 255
                              })
                          },
                        GRADRECORD {
                          gRADRECORD_ratio = 255,
                          gRADRECORD_color = Left (RGB {
                              rGB_red = 16,
                              rGB_green = 89,
                              rGB_blue = 156
                            })
                          }
                      ]
                  }
              }
          ],
        sHAPEWITHSTYLE_lineStyles = Left [],
        sHAPEWITHSTYLE_numFillBits = 1,
        sHAPEWITHSTYLE_numLineBits = 0,
        sHAPEWITHSTYLE_shapeRecords = [
            STYLECHANGERECORD {
                sTYLECHANGERECORD_move = Just (16,23349,13737),
                sTYLECHANGERECORD_fillStyle0 = Nothing,
                sTYLECHANGERECORD_fillStyle1 = Just 1,
                sTYLECHANGERECORD_lineStyle = Nothing,
                sTYLECHANGERECORD_new = Nothing
              },
            CURVEDEDGERECORD {
                cURVEDEDGERECORD_numBits = 14,
                cURVEDEDGERECORD_controlDeltaX = -19799,
                cURVEDEDGERECORD_controlDeltaY = 818,
                cURVEDEDGERECORD_anchorDeltaX = -17536,
                cURVEDEDGERECORD_anchorDeltaY = -619
              },
            STRAIGHTEDGERECORD {
                sTRAIGHTEDGERECORD_numBits = 14,
                sTRAIGHTEDGERECORD_straightEdge = VerticalLine {
                    straightEdge_deltaY = -28322
                  }
              },
            STRAIGHTEDGERECORD {
                sTRAIGHTEDGERECORD_numBits = 15,
                sTRAIGHTEDGERECORD_straightEdge = HorizontalLine {
                    straightEdge_deltaX = 37335
                  }
              },
            STRAIGHTEDGERECORD {
                sTRAIGHTEDGERECORD_numBits = 14,
                sTRAIGHTEDGERECORD_straightEdge = VerticalLine {
                    straightEdge_deltaY = 28123
                  }
              }
          ]
      }
  }
