module SpecificationExample where

import TestUtilities

main :: IO ()
main = do
    swf_bytes <- io_swf_bytes
    runGetSwf swf_bytes `assertEquals` swf


io_swf_bytes = readFileWords $ "examples" </> "spec" </> "example.swf"

swf = Swf {
    compressed = False,
    version = 3,
    fileLength = 79,
    frameSize = RECT {
        rECT_xmin = 0,
        rECT_xmax = 11000,
        rECT_ymin = 0,
        rECT_ymax = 8000
    },
    frameRate = FIXED8 {
        fIXED8_decimal = 0,
        fIXED8_integer = 12
    },
    frameCount = 1,
    tags = [
        SetBackgroundColor {
            setBackgroundColor_backgroundColor = RGB {
                rGB_red = 255,
                rGB_green = 255,
                rGB_blue = 255
            }
        },
        DefineShape {
            defineShape_shapeId = 1,
            defineShape_shapeBounds = RECT {
                rECT_xmin = 2010,
                rECT_xmax = 4910,
                rECT_ymin = 1670,
                rECT_ymax = 4010
            },
            defineShape_shapes = SHAPEWITHSTYLE {
                sHAPEWITHSTYLE_fillStyles = [],
                sHAPEWITHSTYLE_lineStyles = Left [
                    LINESTYLE {
                        lINESTYLE_width = 20,
                        lINESTYLE_color = Left (RGB {
                            rGB_red = 0,
                            rGB_green = 0,
                            rGB_blue = 0
                        })
                    }
                ],
                sHAPEWITHSTYLE_shapeRecords = [
                    STYLECHANGERECORD {
                        sTYLECHANGERECORD_move = Just (14,
                        4900,
                        1680),
                        sTYLECHANGERECORD_fillStyle0 = Nothing,
                        sTYLECHANGERECORD_fillStyle1 = Nothing,
                        sTYLECHANGERECORD_lineStyle = Just 1,
                        sTYLECHANGERECORD_new = Nothing
                    },
                    STRAIGHTEDGERECORD {
                        sTRAIGHTEDGERECORD_numBits = 11,
                        sTRAIGHTEDGERECORD_straightEdge = VerticalLine {
                            straightEdge_deltaY = 2320
                        }
                    },
                    STRAIGHTEDGERECORD {
                        sTRAIGHTEDGERECORD_numBits = 11,
                        sTRAIGHTEDGERECORD_straightEdge = HorizontalLine {
                            straightEdge_deltaX = -2880
                        }
                    },
                    STRAIGHTEDGERECORD {
                        sTRAIGHTEDGERECORD_numBits = 11,
                        sTRAIGHTEDGERECORD_straightEdge = VerticalLine {
                            straightEdge_deltaY = -2320
                        }
                    },
                    STRAIGHTEDGERECORD {
                        sTRAIGHTEDGERECORD_numBits = 11,
                        sTRAIGHTEDGERECORD_straightEdge = HorizontalLine {
                            straightEdge_deltaX = 2880
                        }
                    }
                ]
            }
        },
        PlaceObject2 {
            placeObject2_placeFlagMove = False,
            placeObject2_depth = 1,
            placeObject2_characterId = Just 1,
            placeObject2_matrix = Just (MATRIX {
                mATRIX_scale = Nothing,
                mATRIX_rotate = Nothing,
                mATRIX_translateX = 0,
                mATRIX_translateY = 0
            }),
            placeObject2_colorTransform = Nothing,
            placeObject2_ratio = Nothing,
            placeObject2_name = Nothing,
            placeObject2_clipDepth = Nothing,
            placeObject2_clipActions = Nothing
        },
        ShowFrame,
        End
    ]
}