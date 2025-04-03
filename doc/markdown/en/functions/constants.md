# Список констант

Режим сортировки (`EngineSetObjectsSorting`):

`osInherited` = 0<br>
`osNone` = 1<br>
`osRenderFarthestFirst` = 2<br>
`osRenderBlendedLast` = 3<br>
`osRenderNearestFirst` = 4<br>

Режим отбора видимости (`EngineSetCulling`, `ObjectSetCulling`):

`vcNone` = 0<br>
`vcInherited` = 1<br>
`vcObjectBased` = 2<br>
`vcHierarchical` = 3<br>

Вертикальная синхронизация (`ViewerEnableVSync`):

`vsmSync` = 0<br>
`vsmNoSync` = 1<br>

Антиалиасинг (`ViewerSetAntiAliasing`):

`aaDefault` = 0<br>
`aaNone` = 1<br>
`aa2x` = 2<br>
`aa2xHQ` = 3<br>
`aa4x` = 4<br>
`aa4xHQ` = 5<br>
`aa6x` = 6<br>
`aa8x` = 7<br>
`aa16x` = 8<br>
`csa8x` = 9<br>
`csa8xHQ` = 10<br>
`csa16x` = 11<br>
`csa16xHQ` = 12<br>

Тип (стиль) камеры (`CameraSetStyle`):

`csPerspective` = 0<br>
`csOrthogonal` = 1<br>
`csOrtho2D` = 2<br>
`csInfinitePerspective` = 3<br>

Режим инвариантности относительно камеры (`DummycubeSetCameraMode`):

`cimNone` = 0<br>
`cimPosition` = 1<br>
`cimOrientation` = 2<br>

Тип (стиль) источника освещения (`LightCreate`, `LightSetStyle`):

`lsSpot` = 0<br>
`lsOmni` = 1<br>
`lsParallel` = 2<br>

Направление нормалей (`CubeSetNormalDirection`):

`ndOutside` = 0<br>
`ndInside` = 1<br>

Режим воспроизведения анимации (`ActorSetAnimationMode`):

`aamNone` = 0<br>
`aamPlayOnce` = 1<br>
`aamLoop` = 2<br>
`aamBounceForward` = 3<br>
`aamBounceBackward` = 4<br>
`aamLoopBackward` = 5<br>

Режим тесселяции (`TerrainSetQualityStyle`):

`hrsFullGeometry` = 0<br>
`hrsTesselated` = 1<br>

Режим тесселяции для невидимых тайлов (`TerrainSetOcclusionTesselate`):

`totTesselateAlways` = 0<br>
`totTesselateIfVisible` = 1<br>

`scNoOverlap` = 0<br>
`scContainsFully` = 1<br>
`scContainsPartially` = 2<br>

`pmFill` = 0<br>
`pmLines` = 1<br>
`pmPoints` = 2<br>

`tmmUser` = 0<br>
`tmmObjectLinear` = 1<br>
`tmmEyeLinear` = 2<br>
`tmmSphere` = 3<br>
`tmmCubeMapReflection` = 4<br>
`tmmCubeMapNormal` = 5<br>
`tmmCubeMapLight0` = 6<br>
`tmmCubeMapCamera` = 7<br>

`tiaDefault` = 0<br>
`tiaAlphaFromIntensity` = 1<br>
`tiaSuperBlackTransparent` = 2<br>
`tiaLuminance` = 3<br>
`tiaLuminanceSqrt` = 4<br>
`tiaOpaque` = 5<br>
`tiaTopLeftPointColorTransparent` = 6<br>
`tiaInverseLuminance` = 7<br>
`tiaInverseLuminanceSqrt` = 8<br>

`tmDecal` = 0<br>
`tmModulate` = 1<br>
`tmBlend` = 2<br>
`tmReplace` = 3<br>

`bmOpaque` = 0<br>
`bmTransparency` = 1<br>
`bmAdditive` = 2<br>
`bmAlphaTest50` = 3<br>
`bmAlphaTest100` = 4<br>
`bmModulate` = 5<br>

`miNearest` = 0<br>
`miLinear` = 1<br>
`miNearestMipmapNearest` = 2<br>
`miLinearMipmapNearest` = 3<br>
`miNearestMipmapLinear` = 4<br>
`miLinearMipmapLinear` = 5<br>
`maNearest` = 0<br>
`maLinear` = 1<br>

`fcBufferDefault` = 0<br>
`fcCull` = 1<br>
`fcNoCull` = 2<br>

`tfDefault` = 0<br>
`tfRGB` = 1<br>
`tfRGBA` = 2<br>
`tfRGB16` = 3<br>
`tfRGBA16` = 4<br>
`tfAlpha` = 5<br>
`tfLuminance` = 6<br>
`tfLuminanceAlpha` = 7<br>
`tfIntensity` = 8<br>
`tfNormalMap` = 9<br>
`tfRGBAFloat16` = 10<br>
`tfRGBAFloat32` = 11<br>
`tfExtended` = 12<br>

`tfALPHA4` = 0<br>
`tfALPHA8` = 1<br>
`tfALPHA12` = 2<br>
`tfALPHA16` = 3<br>
`tfDEPTH_COMPONENT16` = 4<br>
`tfDEPTH_COMPONENT24` = 5<br>
`tfDEPTH_COMPONENT32` = 6<br>
`tfLUMINANCE4` = 7<br>
`tfLUMINANCE8` = 8<br>
`tfLUMINANCE12` = 9<br>
`tfLUMINANCE16` = 10<br>
`tfLUMINANCE4_ALPHA4` = 11<br>
`tfLUMINANCE6_ALPHA2` = 12<br>
`tfLUMINANCE8_ALPHA8` = 13<br>
`tfLUMINANCE12_ALPHA4` = 14<br>
`tfLUMINANCE12_ALPHA12` = 15<br>
`tfLUMINANCE16_ALPHA16` = 16<br>
`tfINTENSITY4` = 17<br>
`tfINTENSITY8` = 18<br>
`tfINTENSITY12` = 19<br>
`tfINTENSITY16I` = 20<br>
`tfR3_G3_B2` = 21<br>
`tfRGB4` = 22<br>
`tfRGB5` = 23<br>
`tfRGB8` = 24<br>
`tfRGB10` = 25<br>
`tfRGB12` = 26<br>
`tfR16G16B16` = 27<br>
`tfRGBA2` = 28<br>
`tfRGBA4` = 29<br>
`tfRGB5_A1` = 30<br>
`tfRGBA8` = 31<br>
`tfRGB10_A2` = 32<br>
`tfRGBA12` = 33<br>
`tfR16G16B16A16` = 34<br>
`tfCOMPRESSED_RGB_S3TC_DXT1` = 35<br>
`tfCOMPRESSED_RGBA_S3TC_DXT1` = 36<br>
`tfCOMPRESSED_RGBA_S3TC_DXT3` = 37<br>
`tfCOMPRESSED_RGBA_S3TC_DXT5` = 38<br>
`tfSIGNED_LUMINANCE8` = 39<br>
`tfSIGNED_LUMINANCE8_ALPHA8` = 40<br>
`tfSIGNED_RGB8` = 41<br>
`tfSIGNED_RGBA8` = 42<br>
`tfSIGNED_RGB8_UNSIGNED_ALPHA8` = 43<br>
`tfSIGNED_ALPHA8` = 44<br>
`tfSIGNED_INTENSITY8` = 45<br>
`tfHILO16` = 46<br>
`tfSIGNED_HILO16` = 47<br>
`tfDSDT8` = 48<br>
`tfDSDT8_MAG8` = 49<br>
`tfDSDT8_MAG8_INTENSITY8` = 50<br>
`tfHILO8` = 51<br>
`tfSIGNED_HILO8` = 52<br>
`tfFLOAT_R16` = 53<br>
`tfFLOAT_R32` = 54<br>
`tfFLOAT_RG16` = 55<br>
`tfFLOAT_RGB16` = 56<br>
`tfFLOAT_RGBA16` = 57<br>
`tfFLOAT_RG32` = 58<br>
`tfFLOAT_RGB32` = 59<br>
`tfFLOAT_RGBA32` = 60<br>
`tfRGBA_FLOAT32` = 61<br>
`tfRGB_FLOAT32` = 62<br>
`tfALPHA_FLOAT32` = 63<br>
`tfINTENSITY_FLOAT32` = 64<br>
`tfLUMINANCE_FLOAT32` = 65<br>
`tfLUMINANCE_ALPHA_FLOAT32` = 66<br>
`tfRGBA_FLOAT16` = 67<br>
`tfRGB_FLOAT16` = 68<br>
`tfALPHA_FLOAT16` = 69<br>
`tfINTENSITY_FLOAT16` = 70<br>
`tfLUMINANCE_FLOAT16` = 71<br>
`tfLUMINANCE_ALPHA_FLOAT16` = 72<br>
`tfDEPTH24_STENCIL8` = 73<br>
`tfDEPTH_COMPONENT32F` = 74<br>
`tfDEPTH32F_STENCIL8` = 75<br>
`tfSRGB8` = 76<br>
`tfSRGB8_ALPHA8` = 77<br>
`tfSLUMINANCE8` = 78<br>
`tfSLUMINANCE8_ALPHA8` = 79<br>
`tfCOMPRESSED_SRGB_S3TC_DXT1` = 80<br>
`tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT1` = 81<br>
`tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT3` = 82<br>
`tfCOMPRESSED_SRGB_ALPHA_S3TC_DXT5` = 83<br>
`tfRGB9_E5` = 84<br>
`tfR11F_G11F_B10F` = 85<br>
`tfCOMPRESSED_LUMINANCE_LATC1` = 86<br>
`tfCOMPRESSED_SIGNED_LUMINANCE_LATC1` = 87<br>
`tfCOMPRESSED_LUMINANCE_ALPHA_LATC2` = 88<br>
`tfCOMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2` = 89<br>
`tfCOMPRESSED_LUMINANCE_ALPHA_3DC` = 90<br>
`tfRGBA32UI` = 91<br>
`tfRGB32UI` = 92<br>
`tfALPHA32UI` = 93<br>
`tfINTENSITY32UI` = 94<br>
`tfLUMINANCE32UI` = 95<br>
`tfLUMINANCE_ALPHA32UI` = 96<br>
`tfRGBA16UI` = 97<br>
`tfRGB16UI` = 98<br>
`tfALPHA16UI` = 99<br>
`tfINTENSITY16UI` = 100<br>
`tfLUMINANCE16UI` = 101<br>
`tfLUMINANCE_ALPHA16UI` = 102<br>
`tfRGBA8UI` = 103<br>
`tfRGB8UI` = 104<br>
`tfALPHA8UI` = 105<br>
`tfINTENSITY8UI` = 106<br>
`tfLUMINANCE8UI` = 107<br>
`tfLUMINANCE_ALPHA8UI` = 108<br>
`tfRGBA32I` = 109<br>
`tfRGB32I` = 110<br>
`tfALPHA32I` = 111<br>
`tfINTENSITY32I` = 112<br>
`tfLUMINANCE32I` = 113<br>
`tfLUMINANCE_ALPHA32I` = 114<br>
`tfRGBA16I` = 115<br>
`tfRGB16I` = 116<br>
`tfALPHA16I` = 117<br>
`tfLUMINANCE16I` = 119<br>
`tfLUMINANCE_ALPHA16I` = 120<br>
`tfRGBA8I` = 121<br>
`tfRGB8I` = 122<br>
`tfALPHA8I` = 123<br>
`tfINTENSITY8I` = 124<br>
`tfLUMINANCE8I` = 125<br>
`tfLUMINANCE_ALPHA8I` = 126<br>
`tfRG32UI` = 127<br>
`tfR32UI` = 128<br>
`tfRG16UI` = 129<br>
`tfR16UI` = 130<br>
`tfRG8UI` = 131<br>
`tfR8UI` = 132<br>
`tfRG32I` = 133<br>
`tfR32I` = 134<br>
`tfRG16I` = 135<br>
`tfR16I` = 136<br>
`tfRG8I` = 137<br>
`tfR8I` = 138<br>
`tfRG8` = 139<br>
`tfR8` = 140<br>
`tfRG16` = 141<br>
`tfR16` = 142<br>
`tfRG16F` = 143<br>
`tfR16F` = 144<br>
`tfRG32F` = 145<br>
`tfR32F` = 146<br>
`tfCOMPRESSED_RED_RGTC1` = 147<br>
`tfCOMPRESSED_SIGNED_RED_RGTC1` = 148<br>
`tfCOMPRESSED_RG_RGTC2` = 149<br>
`tfCOMPRESSED_SIGNED_RG_RGTC2` = 150<br>
`tfR8_SNORM` = 151<br>
`tfRG8_SNORM` = 152<br>
`tfRGB8_SNORM` = 153<br>
`tfRGBA8_SNORM` = 154<br>
`tfR16_SNORM` = 155<br>
`tfRG16_SNORM` = 156<br>
`tfRGB16_SNORM` = 157<br>
`tfRGBA16_SNOR` = 158<br>

`tcDefault` = 0<br>
`tcNone` = 1<br>
`tcStandard` = 2<br>
`tcHighQuality` = 3<br>
`tcHighSpeed` = 4<br>

`tfIsotropic` = 0<br>
`tfAnisotropic` = 1<br>

`pNone` = 0<br>
`pGlossy` = 1<br>
`pBeastView` = 2<br>
`pOceanDepth` = 3<br>
`pDream` = 4<br>
`pOverBlur` = 5<br>

`sbmTop` = 0<br>
`sbmBottom` = 1<br>
`sbmLeft` = 2<br>
`sbmRight` = 3<br>
`sbmFront` = 4<br>
`sbmBack` = 5<br>
`sbmClouds` = 6<br>

`sbsFull` = 0<br>
`sbsTopHalf` = 1<br>
`sbsBottomHalf` = 2<br>
`sbsTopTwoThirds` = 3<br>
`sbsTopHalfClamped` = 4<br>

`lsmLines` = 0<br>
`lsmCubicSpline` = 1<br>
`lsmBezierSpline` = 2<br>
`lsmNURBSCurve` = 3<br>
`lsmSegments` = 4<br>

`lnaInvisible` = 0<br>
`lnaAxes` = 1<br>
`lnaCube` = 2<br>
`lnaDodecahedron` = 3<br>

`msUp` = 0<br>
`msDirection` = 1<br>
`msFaceCamera` = 2<br>

`svmAccurate` = 0<br>
`svmDarkening` = 1<br>
`svmOff` = 2<br>

`ccsDCEStandard` = 0<br>
`ccsCollisionStandard` = 1<br>
`ccsHybrid` = 2<br>

`csbSlide` = 0<br>
`csbBounce` = 1<br>

`osmStep` = 0<br>
`osmStepFast` = 1<br>
`osmQuickStep` = 2<br>

`cmFineCulling` = 0<br>
`cmGrossCulling` = 1<br>

`afpNone` = 0<br>
`afpLinear` = 1<br>

`ccsDCEStandard` = 0<br>
`ccsCollisionStandard` = 1<br>
`ccsHybrid` = 2<br>

`csEllipsoid` = 0<br>
`csBox` = 1<br>
`csFreeform` = 2<br>
`csTerrain` = 3<br>

`glsSegments` = 0<br>
`glsLine` = 1<br>

`gpXY` = 0<br>
`gpYZ` = 1<br>
`gpXZ` = 2<br>
`gpXYZ` = 3<br>

`msRect` = 0<br>
`msDisk` = 1<br>

`krbtUnknown` = 0<br>
`krbtStatic` = 1<br>
`krbtDynamic` = 2<br>
`krbtKinematic` = 3<br>

`uspEveryIteration` = 0<br>
`uspEveryFrame` = 1<br>
`uspNever` = 2<br>

`psDefault` = 0<br>
`psName` = 1<br>
`psMinDepth` = 2<br>
`psMaxDepth` = 3<br>

`aarMorph` = 0<br>
`aarSkeleton` = 1<br>
`aarNone` = 2<br>

`spDefault` = 0<br>
`sp1bit` = 1<br>
`sp4bits` = 2<br>
`sp8bits` = 3<br>
`sp16bits` = 4<br>

`tvDefault` = 0<br>
`tvFBOOnly` = 1<br>

`llMin` = 0<br>
`llMedium` = 1<br>
`llMax` = 3<br>

`lkDebug` = 0<br>
`lkInfo` = 1<br>
`lkNotice` = 2<br>
`lkWarning` = 3<br>
`lkError` = 4<br>
`lkFatalError` = 5<br>
