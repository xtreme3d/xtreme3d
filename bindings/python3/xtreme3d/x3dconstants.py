# Colors
c_black = 0.0
c_dkgray = 4210752.0
c_gray = 8421504.0
c_ltgray = 12632256.0
c_white = 16777215.0
c_aqua = 16776960.0
c_blue = 16711680.0
c_fuchsia = 16711935.0
c_green = 32768.0
c_lime = 65280.0
c_maroon = 128.0
c_navy = 8388608.0
c_olive = 32896.0
c_purple = 8388736.0
c_red = 255.0
c_silver = 12632256.0
c_teal = 8421376.0
c_yellow = 65535.0
c_orange = 33023.0

# Object sorting
osInherited = 0
osNone = 1
osRenderFarthestFirst = 2
osRenderBlendedLast = 3
osRenderNearestFirst = 4

# Visibility culling
vcNone = 0
vcInherited = 1
vcObjectBased = 2
vcHierarchical = 3

# VSync
vsmSync = 0
vsmNoSync = 1

# Antialiasing
aaDefault = 0
aaNone = 1
aa2x = 2
aa2xHQ = 3
aa4x = 4
aa4xHQ = 5
aa6x = 6
aa8x = 7
aa16x = 8
csa8x = 9
csa8xHQ = 10
csa16x = 11
csa16xHQ = 12

# Dummycube invariance
cimNone = 0
cimPosition = 1
cimOrientation = 2

# Camera projection style
csPerspective = 0
csOrthogonal = 1
csOrtho2D = 2
csInfinitePerspective = 3

# Light source type
lsSpot = 0
lsOmni = 1
lsParallel = 2

# Normal direction
ndOutside = 0
ndInside = 1

# Animation mode
aamNone = 0
aamPlayOnce = 1
aamLoop = 2
aamBounceForward = 3
aamBounceBackward = 4
aamLoopBackward = 5

# Frame interpolation
afpNone = 0
afpLinear = 1

# Terrain quality style
hrsFullGeometry = 0
hrsTesselated = 1

# Terrain occlusion tesselate
totTesselateAlways = 0
totTesselateIfVisible = 1

#
scNoOverlap = 0
scContainsFully = 1
scContainsPartially = 2

# Polygon mode
pmFill = 0
pmLines = 1
pmPoints = 2

# Texture mapping mode
tmmUser = 0
tmmObjectLinear = 1
tmmEyeLinear = 2
tmmSphere = 3
tmmCubeMapReflection = 4
tmmCubeMapNormal = 5
tmmCubeMapLight0 = 6
tmmCubeMapCamera = 7

# Texture image alpha
tiaDefault = 0
tiaAlphaFromIntensity = 1
tiaSuperBlackTransparent = 2
tiaLuminance = 3
tiaLuminanceSqrt = 4
tiaOpaque = 5
tiaTopLeftPointColorTransparent = 6
tiaInverseLuminance = 7
iaInverseLuminanceSqrt = 8
tiaBottomRightPointColorTransparent = 9

# Texture mode
tmDecal = 0
tmModulate = 1
tmBlend = 2
tmReplace = 3

# Blending mode
bmOpaque = 0
bmTransparency = 1
bmAdditive = 2
bmAlphaTest50 = 3
bmAlphaTest100 = 4
bmModulate = 5

# Texture filter
miNearest = 0
miLinear = 1
miNearestMipmapNearest = 2
miLinearMipmapNearest = 3
miNearestMipmapLinear = 4
miLinearMipmapLinear = 5
maNearest = 0
maLinear = 1

# Face culling
fcBufferDefault = 0
fcCull = 1
fcNoCull = 2

# Texture format
tfDefault = 0
tfRGB = 1
tfRGBA = 2
tfRGB16 = 3
tfRGBA16 = 4
tfAlpha = 5
tfLuminance = 6
tfLuminanceAlpha = 7
tfIntensity = 8
tfNormalMap = 9
tfRGBAFloat16 = 10
tfRGBAFloat32 = 11
tfExtended = 12

# Texture format extended
tfALPHA4 = 0
tfALPHA8 = 1
tfALPHA12 = 2
tfALPHA16 = 3
tfDEPTH_COMPONENT16 = 4
tfDEPTH_COMPONENT24 = 5
tfDEPTH_COMPONENT32 = 6
tfLUMINANCE4 = 7
tfLUMINANCE8 = 8
tfLUMINANCE12 = 9
tfLUMINANCE16 = 10
tfLUMINANCE4_ALPHA4 = 11
tfLUMINANCE6_ALPHA2 = 12
tfLUMINANCE8_ALPHA8 = 13
tfLUMINANCE12_ALPHA4 = 14
tfLUMINANCE12_ALPHA12 = 15
tfLUMINANCE16_ALPHA16 = 16
tfINTENSITY4 = 17
tfINTENSITY8 = 18
tfINTENSITY12 = 19
tfINTENSITY16I = 20
tfR3_G3_B2 = 21
tfRGB4 = 22
tfRGB5 = 23
tfRGB8 = 24
tfRGB10 = 25
tfRGB12 = 26
tfR16G16B16 = 27
tfRGBA2 = 28
tfRGBA4 = 29
tfRGB5_A1 = 30
tfRGBA8 = 31
tfRGB10_A2 = 32
tfRGBA12 = 33
tfR16G16B16A16 = 4
tfCOMPRESSED_RGB_S3TC_DXT1 = 35
tfCOMPRESSED_RGBA_S3TC_DXT1 = 36
tfCOMPRESSED_RGBA_S3TC_DXT3 = 37
tfCOMPRESSED_RGBA_S3TC_DXT5 = 38
tfSIGNED_LUMINANCE8 = 39
tfSIGNED_LUMINANCE8_ALPHA8 = 40
tfSIGNED_RGB8 = 41
tfSIGNED_RGBA8 = 42
tfSIGNED_RGB8_UNSIGNED_ALPHA8 = 43
tfSIGNED_ALPHA8 = 44
tfSIGNED_INTENSITY8 = 45
tfHILO16 = 46
tfSIGNED_HILO16 = 47
tfDSDT8 = 48
tfDSDT8_MAG8 = 49
tfDSDT8_MAG8_INTENSITY8 = 50
tfHILO8 = 51
tfSIGNED_HILO8 = 52
tfFLOAT_R16 = 53
tfFLOAT_R32 = 54
tfFLOAT_RG16 = 55
tfFLOAT_RGB16 = 56
tfFLOAT_RGBA16 = 57
tfFLOAT_RG32 = 58
tfFLOAT_RGB32 = 59
tfFLOAT_RGBA32 = 60
tfRGBA_FLOAT32 = 61
tfRGB_FLOAT32 = 62
tfALPHA_FLOAT32 = 63
tfINTENSITY_FLOAT32 = 64
tfLUMINANCE_FLOAT32 = 65
tfLUMINANCE_ALPHA_FLOAT32 = 66
tfRGBA_FLOAT16 = 67
tfRGB_FLOAT16 = 68
tfALPHA_FLOAT16 = 69
tfINTENSITY_FLOAT16 = 70
tfLUMINANCE_FLOAT16 = 71
tfLUMINANCE_ALPHA_FLOAT16 = 72
tfDEPTH24_STENCIL8 = 73
tfDEPTH_COMPONENT32F = 74
tfDEPTH32F_STENCIL8 = 75
tfSRGB8 = 76
tfSRGB8_ALPHA8 = 77
tfSLUMINANCE8 = 78
tfSLUMINANCE8_ALPHA8 = 79
tfCOMPRESSED_SRGB_S3TC_DXT1 = 80
tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1 = 81
tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3 = 82
tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5 = 83
tfRGB9_E5 = 84
tfR11F_G11F_B10F = 85
tfCOMPRESSED_LUMINANCE_LATC1 = 86
tfCOMPRESSED_SIGNED_LUMINANCE_LATC1 = 87
tfCOMPRESSED_LUMINANCE_ALPHA_LATC2 = 88
tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2 = 89
tfCOMPRESSED_LUMINANCE_ALPHA_3DC = 90
tfRGBA32UI = 91
tfRGB32UI = 92
tfALPHA32UI = 93
tfINTENSITY32UI = 94
tfLUMINANCE32UI = 95
tfLUMINANCE_ALPHA32UI = 96
tfRGBA16UI = 97
tfRGB16UI = 98
tfALPHA16UI = 99
tfINTENSITY16UI = 100
tfLUMINANCE16UI = 101
tfLUMINANCE_ALPHA16UI = 102
tfRGBA8UI = 103
tfRGB8UI = 104
tfALPHA8UI = 105
tfINTENSITY8UI = 106
tfLUMINANCE8UI = 107
tfLUMINANCE_ALPHA8UI = 108
tfRGBA32I = 109
tfRGB32I = 110
tfALPHA32I = 111
tfINTENSITY32I = 112
tfLUMINANCE32I = 113
tfLUMINANCE_ALPHA32I = 114
tfRGBA16I = 115
tfRGB16I = 116
tfALPHA16I = 117
# tfINTENSITY16I = 118
tfLUMINANCE16I = 119
tfLUMINANCE_ALPHA16I = 120
tfRGBA8I = 121
tfRGB8I = 122
tfALPHA8I = 123
tfINTENSITY8I = 124
tfLUMINANCE8I = 125
tfLUMINANCE_ALPHA8I = 126
tfRG32UI = 127
tfR32UI = 128
tfRG16UI = 129
tfR16UI = 130
tfRG8UI = 131
tfR8UI = 132
tfRG32I = 133
tfR32I = 134
tfRG16I = 135
tfR16I = 136
tfRG8I = 137
tfR8I = 138
tfRG8 = 139
tfR8 = 140
tfRG16 = 141
tfR16 = 142
tfRG16F = 143
tfR16F = 144
tfRG32F = 145
tfR32F = 146
tfCOMPRESSED_RED_RGTC1 = 147
tfCOMPRESSED_SIGNED_RED_RGTC1 = 148
tfCOMPRESSED_RG_RGTC2 = 149
tfCOMPRESSED_SIGNED_RG_RGTC2 = 150
tfR8_SNORM = 151
tfRG8_SNORM = 152
tfRGB8_SNORM = 153
tfRGBA8_SNORM = 154
tfR16_SNORM = 155
tfRG16_SNORM = 156
tfRGB16_SNORM = 157
tfRGBA16_SNOR = 158

# Texture wrap
twNone = 0
twBoth = 1
twVertical = 2
twHorizontal = 3
twSeparate = 4

# Texture wrap separate
twRepeat = 0
twClampToEdge = 1
twClampToBorder = 2
twMirrorRepeat = 3
twMirrorClampToEdge = 4
twMirrorClampToBorder = 5

# Texture compare mode
tcmNone = 0
tcmCompareRtoTexture = 1

# Texture depth compare func
cfNever = 0
cfAlways = 1
cfLess = 2
cfLEqual = 3
cfEqual = 4
cfGreater = 5
cfNotEqual = 6
cfGEqual = 7

# Texture compression
tcDefault = 0
tcNone = 1
tcStandard = 2
tcHighQuality = 3
tcHighSpeed = 4

# Texture filtering quality
tfIsotropic = 0
tfAnisotropic = 1

# Blur preset
pNone = 0
pGlossy = 1
pBeastView = 2
pOceanDepth = 3
pDream = 4
pOverBlur = 5

# Skybox material
sbmTop = 0
sbmBottom = 1
sbmLeft = 2
sbmRight = 3
sbmFront = 4
sbmBack = 5
sbmClouds = 6

# Skybox style
sbsFull = 0
sbsTopHalf = 1
sbsBottomHalf = 2
sbsTopTwoThirds = 3
sbsTopHalfClamped = 4

# Lines spline mode
lsmLines = 0
lsmCubicSpline = 1
lsmBezierSpline = 2
lsmNURBSCurve = 3
lsmSegments = 4

# Lines nodes aspect
lnaInvisible = 0
lnaAxes = 1
lnaCube = 2
lnaDodecahedron = 3

# Trail mark style
msUp = 0
msDirection = 1
msFaceCamera = 2

# Shadowvolume mode
svmAccurate = 0
svmDarkening = 1
svmOff = 2

# Mirror shape
msRect = 0
msDisk = 1

# 
ccsDCEStandard = 0
ccsCollisionStandard = 1
ccsHybrid = 2

# DCE slide or bounce
csbSlide = 0
csbBounce = 1

# DCE collider shape
csEllipsoid = 0
csBox = 1
csFreeform = 2
csTerrain = 3

# ODE step
osmStep = 0
osmStepFast = 1
osmQuickStep = 2

# Partition culling
cmFineCulling = 0
cmGrossCulling = 1

# Grid line style
glsSegments = 0
glsLine = 1

# Grid parts
gpXY = 0
gpYZ = 1
gpXZ = 2
gpXYZ = 3

# Kraft body type
krbtUnknown = 0
krbtStatic = 1
krbtDynamic = 2
krbtKinematic = 3

#
uspEveryIteration = 0
uspEveryFrame = 1
uspNever = 2

# Pick sort type
psDefault = 0
psName = 1
psMinDepth = 2
psMaxDepth = 3

#
aarMorph = 0
aarSkeleton = 1
aarNone = 2

# Stencil precision
spDefault = 0
sp1bit = 1
sp4bits = 2
sp8bits = 3
sp16bits = 4

# FBO target visibility
tvDefault = 0
tvFBOOnly = 1

#
llMin = 0
llMedium = 1
llMax = 3

# Log level
lkDebug = 0
lkInfo = 1
lkNotice = 2
lkWarning = 3
lkError = 4
lkFatalError = 5
