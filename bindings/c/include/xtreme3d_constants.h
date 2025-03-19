/* Xtreme3D constants header */

#ifndef XTREME3D_CONSTANTS_H
#define XTREME3D_CONSTANTS_H

#define osInherited 0
#define osNone 1
#define osRenderFarthestFirst 2
#define osRenderBlendedLast 3
#define osRenderNearestFirst 4

#define vcNone 0
#define vcInherited 1
#define vcObjectBased 2
#define vcHierarchical 3

#define vsmSync 0
#define vsmNoSync 1

#define aaDefault 0
#define aaNone 1
#define aa2x 2
#define aa2xHQ 3
#define aa4x 4
#define aa4xHQ 5
#define aa6x 6
#define aa8x 7
#define aa16x 8
#define csa8x 9
#define csa8xHQ 10
#define csa16x 11
#define csa16xHQ 12

#define cimNone 0
#define cimPosition 1
#define cimOrientation 2

#define csPerspective 0
#define csOrthogonal 1
#define csOrtho2D 2
#define csInfinitePerspective 3

#define lsSpot 0
#define lsOmni 1
#define lsParallel 2

#define ndOutside 0
#define ndInside 1

#define aamNone 0
#define aamPlayOnce 1
#define aamLoop 2
#define aamBounceForward 3
#define aamBounceBackward 4
#define aamLoopBackward 5

#define afpNone 0
#define afpLinear 1

#define hrsFullGeometry 0
#define hrsTesselated 1

#define totTesselateAlways 0
#define totTesselateIfVisible 1

#define scNoOverlap 0
#define scContainsFully 1
#define scContainsPartially 2

#define pmFill 0
#define pmLines 1
#define pmPoints 2

#define tmmUser 0
#define tmmObjectLinear 1
#define tmmEyeLinear 2
#define tmmSphere 3
#define tmmCubeMapReflection 4
#define tmmCubeMapNormal 5
#define tmmCubeMapLight0 6
#define tmmCubeMapCamera 7

#define tiaDefault 0
#define tiaAlphaFromIntensity 1
#define tiaSuperBlackTransparent 2
#define tiaLuminance 3
#define tiaLuminanceSqrt 4
#define tiaOpaque 5
#define tiaTopLeftPointColorTransparent 6
#define tiaInverseLuminance 7
#define iaInverseLuminanceSqrt 8
#define tiaBottomRightPointColorTransparent 9

#define tmDecal 0
#define tmModulate 1
#define tmBlend 2
#define tmReplace 3

#define bmOpaque 0
#define bmTransparency 1
#define bmAdditive 2
#define bmAlphaTest50 3
#define bmAlphaTest100 4
#define bmModulate 5

#define miNearest 0
#define miLinear 1
#define miNearestMipmapNearest 2
#define miLinearMipmapNearest 3
#define miNearestMipmapLinear 4
#define miLinearMipmapLinear 5
#define maNearest 0
#define maLinear 1

#define fcBufferDefault 0
#define fcCull 1
#define fcNoCull 2

#define tfDefault 0
#define tfRGB 1
#define tfRGBA 2
#define tfRGB16 3
#define tfRGBA16 4
#define tfAlpha 5
#define tfLuminance 6
#define tfLuminanceAlpha 7
#define tfIntensity 8
#define tfNormalMap 9
#define tfRGBAFloat16 10
#define tfRGBAFloat32 11
#define tfExtended 12

#define tfALPHA4 0
#define tfALPHA8 1
#define tfALPHA12 2
#define tfALPHA16 3
#define tfDEPTH_COMPONENT16 4
#define tfDEPTH_COMPONENT24 5
#define tfDEPTH_COMPONENT32 6
#define tfLUMINANCE4 7
#define tfLUMINANCE8 8
#define tfLUMINANCE12 9
#define tfLUMINANCE16 10
#define tfLUMINANCE4_ALPHA4 11
#define tfLUMINANCE6_ALPHA2 12
#define tfLUMINANCE8_ALPHA8 13
#define tfLUMINANCE12_ALPHA4 14
#define tfLUMINANCE12_ALPHA12 15
#define tfLUMINANCE16_ALPHA16 16
#define tfINTENSITY4 17
#define tfINTENSITY8 18
#define tfINTENSITY12 19
#define tfINTENSITY16I 20
#define tfR3_G3_B2 21
#define tfRGB4 22
#define tfRGB5 23
#define tfRGB8 24
#define tfRGB10 25
#define tfRGB12 26
#define tfR16G16B16 27
#define tfRGBA2 28
#define tfRGBA4 29
#define tfRGB5_A1 30
#define tfRGBA8 31
#define tfRGB10_A2 32
#define tfRGBA12 33
#define tfR16G16B16A16 4
#define tfCOMPRESSED_RGB_S3TC_DXT1 35
#define tfCOMPRESSED_RGBA_S3TC_DXT1 36
#define tfCOMPRESSED_RGBA_S3TC_DXT3 37
#define tfCOMPRESSED_RGBA_S3TC_DXT5 38
#define tfSIGNED_LUMINANCE8 39
#define tfSIGNED_LUMINANCE8_ALPHA8 40
#define tfSIGNED_RGB8 41
#define tfSIGNED_RGBA8 42
#define tfSIGNED_RGB8_UNSIGNED_ALPHA8 43
#define tfSIGNED_ALPHA8 44
#define tfSIGNED_INTENSITY8 45
#define tfHILO16 46
#define tfSIGNED_HILO16 47
#define tfDSDT8 48
#define tfDSDT8_MAG8 49
#define tfDSDT8_MAG8_INTENSITY8 50
#define tfHILO8 51
#define tfSIGNED_HILO8 52
#define tfFLOAT_R16 53
#define tfFLOAT_R32 54
#define tfFLOAT_RG16 55
#define tfFLOAT_RGB16 56
#define tfFLOAT_RGBA16 57
#define tfFLOAT_RG32 58
#define tfFLOAT_RGB32 59
#define tfFLOAT_RGBA32 60
#define tfRGBA_FLOAT32 61
#define tfRGB_FLOAT32 62
#define tfALPHA_FLOAT32 63
#define tfINTENSITY_FLOAT32 64
#define tfLUMINANCE_FLOAT32 65
#define tfLUMINANCE_ALPHA_FLOAT32 66
#define tfRGBA_FLOAT16 67
#define tfRGB_FLOAT16 68
#define tfALPHA_FLOAT16 69
#define tfINTENSITY_FLOAT16 70
#define tfLUMINANCE_FLOAT16 71
#define tfLUMINANCE_ALPHA_FLOAT16 72
#define tfDEPTH24_STENCIL8 73
#define tfDEPTH_COMPONENT32F 74
#define tfDEPTH32F_STENCIL8 75
#define tfSRGB8 76
#define tfSRGB8_ALPHA8 77
#define tfSLUMINANCE8 78
#define tfSLUMINANCE8_ALPHA8 79
#define tfCOMPRESSED_SRGB_S3TC_DXT1 80
#define tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1 81
#define tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3 82
#define tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5 83
#define tfRGB9_E5 84
#define tfR11F_G11F_B10F 85
#define tfCOMPRESSED_LUMINANCE_LATC1 86
#define tfCOMPRESSED_SIGNED_LUMINANCE_LATC1 87
#define tfCOMPRESSED_LUMINANCE_ALPHA_LATC2 88
#define tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2 89
#define tfCOMPRESSED_LUMINANCE_ALPHA_3DC 90
#define tfRGBA32UI 91
#define tfRGB32UI 92
#define tfALPHA32UI 93
#define tfINTENSITY32UI 94
#define tfLUMINANCE32UI 95
#define tfLUMINANCE_ALPHA32UI 96
#define tfRGBA16UI 97
#define tfRGB16UI 98
#define tfALPHA16UI 99
#define tfINTENSITY16UI 100
#define tfLUMINANCE16UI 101
#define tfLUMINANCE_ALPHA16UI 102
#define tfRGBA8UI 103
#define tfRGB8UI 104
#define tfALPHA8UI 105
#define tfINTENSITY8UI 106
#define tfLUMINANCE8UI 107
#define tfLUMINANCE_ALPHA8UI 108
#define tfRGBA32I 109
#define tfRGB32I 110
#define tfALPHA32I 111
#define tfINTENSITY32I 112
#define tfLUMINANCE32I 113
#define tfLUMINANCE_ALPHA32I 114
#define tfRGBA16I 115
#define tfRGB16I 116
#define tfALPHA16I 117

#define tfLUMINANCE16I 119
#define tfLUMINANCE_ALPHA16I 120
#define tfRGBA8I 121
#define tfRGB8I 122
#define tfALPHA8I 123
#define tfINTENSITY8I 124
#define tfLUMINANCE8I 125
#define tfLUMINANCE_ALPHA8I 126
#define tfRG32UI 127
#define tfR32UI 128
#define tfRG16UI 129
#define tfR16UI 130
#define tfRG8UI 131
#define tfR8UI 132
#define tfRG32I 133
#define tfR32I 134
#define tfRG16I 135
#define tfR16I 136
#define tfRG8I 137
#define tfR8I 138
#define tfRG8 139
#define tfR8 140
#define tfRG16 141
#define tfR16 142
#define tfRG16F 143
#define tfR16F 144
#define tfRG32F 145
#define tfR32F 146
#define tfCOMPRESSED_RED_RGTC1 147
#define tfCOMPRESSED_SIGNED_RED_RGTC1 148
#define tfCOMPRESSED_RG_RGTC2 149
#define tfCOMPRESSED_SIGNED_RG_RGTC2 150
#define tfR8_SNORM 151
#define tfRG8_SNORM 152
#define tfRGB8_SNORM 153
#define tfRGBA8_SNORM 154
#define tfR16_SNORM 155
#define tfRG16_SNORM 156
#define tfRGB16_SNORM 157
#define tfRGBA16_SNOR 158

#define twNone 0
#define twBoth 1
#define twVertical 2
#define twHorizontal 3
#define twSeparate 4

#define twRepeat 0
#define twClampToEdge 1
#define twClampToBorder 2
#define twMirrorRepeat 3
#define twMirrorClampToEdge 4
#define twMirrorClampToBorder 5

#define tcmNone 0
#define tcmCompareRtoTexture 1

#define cfNever 0
#define cfAlways 1
#define cfLess 2
#define cfLEqual 3
#define cfEqual 4
#define cfGreater 5
#define cfNotEqual 6
#define cfGEqual 7

#define tcDefault 0
#define tcNone 1
#define tcStandard 2
#define tcHighQuality 3
#define tcHighSpeed 4

#define tfIsotropic 0
#define tfAnisotropic 1

#define pNone 0
#define pGlossy 1
#define pBeastView 2
#define pOceanDepth 3
#define pDream 4
#define pOverBlur 5

#define sbmTop 0
#define sbmBottom 1
#define sbmLeft 2
#define sbmRight 3
#define sbmFront 4
#define sbmBack 5
#define sbmClouds 6

#define sbsFull 0
#define sbsTopHalf 1
#define sbsBottomHalf 2
#define sbsTopTwoThirds 3
#define sbsTopHalfClamped 4

#define lsmLines 0
#define lsmCubicSpline 1
#define lsmBezierSpline 2
#define lsmNURBSCurve 3
#define lsmSegments 4

#define lnaInvisible 0
#define lnaAxes 1
#define lnaCube 2
#define lnaDodecahedron 3

#define msUp 0
#define msDirection 1
#define msFaceCamera 2

#define svmAccurate 0
#define svmDarkening 1
#define svmOff 2

#define msRect 0
#define msDisk 1

#define ccsDCEStandard 0
#define ccsCollisionStandard 1
#define ccsHybrid 2

#define csbSlide 0
#define csbBounce 1

#define csEllipsoid 0
#define csBox 1
#define csFreeform 2
#define csTerrain 3

#define osmStep 0
#define osmStepFast 1
#define osmQuickStep 2

#define cmFineCulling 0
#define cmGrossCulling 1

#define glsSegments 0
#define glsLine 1

#define gpXY 0
#define gpYZ 1
#define gpXZ 2
#define gpXYZ 3

#define krbtUnknown 0
#define krbtStatic 1
#define krbtDynamic 2
#define krbtKinematic 3

#define uspEveryIteration 0
#define uspEveryFrame 1
#define uspNever 2

#define psDefault 0
#define psName 1
#define psMinDepth 2
#define psMaxDepth 3

#define aarMorph 0
#define aarSkeleton 1
#define aarNone 2

#define spDefault 0
#define sp1bit 1
#define sp4bits 2
#define sp8bits 3
#define sp16bits 4

#define tvDefault 0
#define tvFBOOnly 1

#define llMin 0
#define llMedium 1
#define llMax 3

#define lkDebug 0
#define lkInfo 1
#define lkNotice 2
#define lkWarning 3
#define lkError 4
#define lkFatalError 5

#endif /* XTREME3D_CONSTANTS_H */
