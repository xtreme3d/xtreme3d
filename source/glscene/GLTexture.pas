{: GLTexture<p>

	Handles all the color and texture stuff.<p>

	<b>History : </b><font size=-1><ul>
      <li>06/03/05 - EG - FTextureEx now autocreated (like FTexture)
      <li>30/11/04 - EG - No longer stores TextureEx if empty
      <li>06/10/04 - NC - Corrected filtering param. setting for float texture, 
                          Now keep using GL_TEXTURE_RECTANGLE_NV for TGLFloatDataImage
      <li>05/10/04 - SG - Added Material.TextureEx (texture extension)
      <li>04/10/04 - NC - Added TGLFloatDataImage
      <li>03/07/04 - LR - Move InitWinColors to GLCrossPlatform
                          Replace TGraphics, TBitmap by TGLGraphics, TGLBitmap
      <li>29/06/04 - SG - Added bmModulate blending mode
      <li>08/04/04 - EG - Added AddMaterialsFromXxxx logic
      <li>04/09/03 - EG - Added TGLShader.Enabled
      <li>02/09/03 - EG - Added TGLColor.HSVA
      <li>28/07/03 - aidave - Added TGLColor.RandomColor
      <li>24/07/03 - EG - Introduced TGLTextureImageEditor mechanism
      <li>04/07/03 - EG - Material.Texture now autocreating,
                          added per-texture brightness and gamma correction
      <li>13/06/03 - EG - cubemap images can now be saved/restored as a whole
      <li>05/06/03 - EG - Assign fixes (Andrzej Kaluza)
      <li>23/05/03 - EG - More generic libmaterial registration
      <li>08/12/02 - EG - Added tiaInverseLuminance
      <li>13/11/02 - EG - Added tmmCubeMapLight0
      <li>18/10/02 - EG - CubeMap texture matrix now setup for 2nd texture unit too
      <li>24/07/02 - EG - Added TGLLibMaterials.DeleteUnusedMaterials
      <li>13/07/02 - EG - Improved materials when lighting is off
      <li>10/07/02 - EG - Added basic protection against cyclic material refs
      <li>08/07/02 - EG - Multipass support
      <li>18/06/02 - EG - Added TGLShader
      <li>26/01/02 - EG - Makes use of new xglBegin/EndUpdate mechanism
      <li>24/01/02 - EG - Added vUseDefaultSets mechanism,
                          TGLPictureImage no longer systematically creates a TPicture 
      <li>21/01/02 - EG - Fixed OnTextureNeeded calls (Leonel)
      <li>20/01/02 - EG - Fixed texture memory use report error
      <li>10/01/02 - EG - Added Material.FaceCulling, default texture filters
                          are now Linear/MipMap 
      <li>07/01/02 - EG - Added renderDPI to rci
      <li>16/12/01 - EG - Added support for cube maps (texture and mappings)
      <li>30/11/01 - EG - Texture-compression related errors now ignored (unsupported formats)
      <li>14/09/01 - EG - Use of vFileStreamClass
      <li>06/09/01 - EG - No longers depends on 'Windows'
      <li>04/09/01 - EG - Texture binding cache
      <li>31/08/01 - EG - tiaDefault wasn't honoured (Rene Lindsay)
      <li>25/08/01 - EG - Added TGLBlankImage
      <li>16/08/01 - EG - drawState now part of TRenderContextInfo
      <li>15/08/01 - EG - TexGen support (object_linear, eye_linear and sphere_map)
      <li>13/08/01 - EG - Fixed OnTextureNeeded handling (paths for mat lib)
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>27/07/01 - EG - TGLLibMaterials now a TOwnedCollection
      <li>19/07/01 - EG - Added "Enabled" to TGLTexture
      <li>28/06/01 - EG - Added AddTextureMaterial TGraphic variant
      <li>14/03/01 - EG - Streaming fixes by Uwe Raabe
      <li>08/03/01 - EG - TGLPicFileImage.GetBitmap32 now resets filename if not found
      <li>01/03/01 - EG - Fixed TGLMaterial.DestroyHandle,
                          Added Texture2 notifications and material cacheing
      <li>26/02/01 - EG - Added support for GL_EXT_texture_filter_anisotropic
      <li>23/02/01 - EG - Fixed texture matrix messup (second was using first)
      <li>21/02/01 - EG - Minor fix for TextureImageRequiredMemory,
                          TexGen calls now based on XOpenGL
      <li>14/02/01 - EG - Added support for texture format & texture compression
      <li>31/01/01 - EG - Added Multitexture support
      <li>28/01/01 - EG - Added MaterialOptions
      <li>15/01/01 - EG - Enhanced TGLPicFileImage.LoadFromFile
      <li>13/01/01 - EG - New helper functions for TGLMaterialLibrary
      <li>08/01/01 - EG - Not-so-clean fix for TGLTexture.Destroy... better fix
                          will require awareness of rendering contexts...
      <li>06/12/00 - EG - Added PrepareBuildList mechanism
      <li>16/10/00 - EG - Fix in TGLPictureImage.Assign
      <li>25/09/00 - EG - New texture management implemented
      <li>13/08/00 - EG - Added AddTextureMaterial
      <li>06/08/00 - EG - File not found error now happens only once per texture,
                          also added some more doc and texture transforms support
                          to TGLLibMaterial
      <li>27/07/00 - EG - TGLPictureImage.Assign now accepts TGraphic & TPicture,
                          Added max texture size clamping
      <li>15/07/00 - EG - Upgrade for new list/handle destruction scheme
      <li>05/07/00 - EG - Added tiaTopLeftPointColorTransparent
      <li>28/06/00 - EG - Added asserts for missing texture files
      <li>01/06/00 - EG - Added ReloadTexture (support for texture library),
                          Fixed persistence of material names in a texture library
      <li>28/05/00 - EG - TGLColor now has NotifyChange support for TGLBaseSceneObject
      <li>23/04/00 - EG - Fixed bugs with TGLPicFileImage & TGLPersistentImage,
                          Added tiaOpaque
		<li>17/04/00 - EG - Added Assign to DummyCube and Sprite
      <li>16/04/00 - EG - Added TGLPicFileImage.Assign
      <li>26/03/00 - EG - Finally fixed nasty bug in TGLMaterial.Free
		<li>22/03/00 - EG - Added BeginUpdate/EndUpdate to TGLPictureImage,
								  Made use of [Un]SetGLState in TGLMaterial
								  (gain = 7-10% on T&L intensive rendering),
                          TGLTexBaseClass is no more (RIP)
		<li>21/03/00 - EG - TGLMaterial props are now longer stored when it is
								  linked to a material library entry,
								  Added TGLPictureImage (split from TGLPersistentImage),
								  TGLPicFileImage has been updated and reactivated,
								  ColorManager is now autocreated and non longer force-linked.
      <li>19/03/00 - EG - Added SaveToXxxx & LoadFromXxxx to TGLMaterialLibrary
		<li>18/03/00 - EG - Added GetGLTextureImageClassesAsStrings,
								  Added FindGLTextureImageClassByFriendlyName,
								  FChanges states now ignored in TGLTexture.GetHandle,
								  Added SaveToFile/LoadFromFile to TextureImage
		<li>17/03/00 - EG - Added tiaLuminance
		<li>14/03/00 - EG - Added RegisterGLTextureImageClass stuff,
								  Added ImageAlpha
		<li>13/03/00 - EG - Changed TGLTextureImage image persistence again,
								  Added "Edit" method for texture image classes,
								  TMagFilter/TMinFilter -> TGLMagFilter/TGLMinFilter
		<li>03/03/00 - EG - Removed TImagePath,
								  Started major rework of the whole TGLTextureImage stuff,
								  Fixed and optimized TGLTexture.PrepareImage
		<li>12/02/00 - EG - Added Material Library
      <li>10/02/00 - EG - Fixed crash when texture is empty
		<li>08/02/00 - EG - Added AsWinColor & DeclareCurrentAsDefault to TGLColor,
								  fixed notification on material property setXxx methods,
								  Objects now begin with 'TGL'
		<li>07/02/00 - EG - "Update"s renamed to "NotifyChange"s
		<li>06/02/00 - EG - RoundUpToPowerOf2, RoundDownToPowerOf2 and
                          IsPowerOf2 moved to GLMisc, added TGLPersistentImage.Assign,
                          fixed TGLMaterial.Assign,
                          disable inheritance stuff in TGLFaceProperties.Apply (needs fixing),
                          Diffuse & ambient color now default to openGL values
      <li>05/02/00 - EG - Javadocisation, fixes and enhancements :<br>
                          TGLColor.Update, ConvertWinColor, TPicImage,
								  TGLMaterial.Apply
   </ul></font>
}
unit GLTexture;

interface

{$i GLScene.inc}

uses
  Classes, OpenGL1x, VectorGeometry, SysUtils, GLMisc, GLGraphics, GLContext,
  GLCrossPlatform, PersistentClasses, GLUtils, GLState;

type
	PColorVector = ^TColorVector;
   TColorVector = TVector;

const

   // color definitions

   // Window's colors (must be filled at program
   // startup, since they depend on the desktop scheme)

   {$J+ - allow change of the following typed constants}

   clrScrollBar           : TColorVector = (0,0,0,1);
   clrBackground          : TColorVector = (0,0,0,1);
   clrActiveCaption       : TColorVector = (0,0,0,1);
   clrInactiveCaption     : TColorVector = (0,0,0,1);
   clrMenu                : TColorVector = (0,0,0,1);
   clrWindow              : TColorVector = (0,0,0,1);
   clrWindowFrame         : TColorVector = (0,0,0,1);
   clrMenuText            : TColorVector = (0,0,0,1);
   clrWindowText          : TColorVector = (0,0,0,1);
   clrCaptionText         : TColorVector = (0,0,0,1);
   clrActiveBorder        : TColorVector = (0,0,0,1);
   clrInactiveBorder      : TColorVector = (0,0,0,1);
   clrAppWorkSpace        : TColorVector = (0,0,0,1);
   clrHighlight           : TColorVector = (0,0,0,1);
   clrHighlightText       : TColorVector = (0,0,0,1);
   clrBtnFace             : TColorVector = (0,0,0,1);
   clrBtnShadow           : TColorVector = (0,0,0,1);
   clrGrayText            : TColorVector = (0,0,0,1);
   clrBtnText             : TColorVector = (0,0,0,1);
   clrInactiveCaptionText : TColorVector = (0,0,0,1);
   clrBtnHighlight        : TColorVector = (0,0,0,1);
   clr3DDkShadow          : TColorVector = (0,0,0,1);
   clr3DLight             : TColorVector = (0,0,0,1);
   clrInfoText            : TColorVector = (0,0,0,1);
   clrInfoBk              : TColorVector = (0,0,0,1);
      
   {$J- - disable change of other typed constants}

   // 'static' color definitions
   // sort of grays
   clrTransparent         : TColorVector = (0,    0,    0,    0);
   clrBlack               : TColorVector = (0,    0,    0,    1);
   clrGray05              : TColorVector = (0.05, 0.05, 0.05, 1);
   clrGray10              : TColorVector = (0.10, 0.10, 0.10, 1);
   clrGray15              : TColorVector = (0.15, 0.15, 0.15, 1);
   clrGray20              : TColorVector = (0.20, 0.20, 0.20, 1);
   clrGray25              : TColorVector = (0.25, 0.25, 0.25, 1);
   clrGray30              : TColorVector = (0.30, 0.30, 0.30, 1);
   clrGray35              : TColorVector = (0.35, 0.35, 0.35, 1);
   clrGray40              : TColorVector = (0.40, 0.40, 0.40, 1);
   clrGray45              : TColorVector = (0.45, 0.45, 0.45, 1);
   clrGray50              : TColorVector = (0.50, 0.50, 0.50, 1);
   clrGray55              : TColorVector = (0.55, 0.55, 0.55, 1);
   clrGray60              : TColorVector = (0.60, 0.60, 0.60, 1);
   clrGray65              : TColorVector = (0.65, 0.65, 0.65, 1);
   clrGray70              : TColorVector = (0.70, 0.70, 0.70, 1);
   clrGray75              : TColorVector = (0.75, 0.75, 0.75, 1);
   clrGray80              : TColorVector = (0.80, 0.80, 0.80, 1);
   clrGray85              : TColorVector = (0.85, 0.85, 0.85, 1);
   clrGray90              : TColorVector = (0.90, 0.90, 0.90, 1);
   clrGray95              : TColorVector = (0.95, 0.95, 0.95, 1);
   clrWhite               : TColorVector = (1,    1,    1,    1);

   // other grays
   clrDimGray             : TColorVector = (0.329412, 0.329412, 0.329412, 1);
   clrGray                : TColorVector = (0.752941, 0.752941, 0.752941, 1);
   clrLightGray           : TColorVector = (0.658824, 0.658824, 0.658824, 1);

   // colors en masse
   clrAquamarine          : TColorVector = (0.439216, 0.858824, 0.576471, 1);
   clrBlueViolet          : TColorVector = (0.62352,  0.372549, 0.623529, 1);
   clrBrown               : TColorVector = (0.647059, 0.164706, 0.164706, 1);
   clrCadetBlue           : TColorVector = (0.372549, 0.623529, 0.623529, 1);
   clrCoral               : TColorVector = (1,        0.498039, 0.0,      1);
   clrCornflowerBlue      : TColorVector = (0.258824, 0.258824, 0.435294, 1);
   clrDarkGreen           : TColorVector = (0.184314, 0.309804, 0.184314, 1);
   clrDarkOliveGreen      : TColorVector = (0.309804, 0.309804, 0.184314, 1);
   clrDarkOrchid          : TColorVector = (0.6,      0.196078, 0.8,      1);
   clrDarkSlateBlue       : TColorVector = (0.419608, 0.137255, 0.556863, 1);
   clrDarkSlateGray       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
   clrDarkSlateGrey       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
   clrDarkTurquoise       : TColorVector = (0.439216, 0.576471, 0.858824, 1);
   clrFirebrick           : TColorVector = (0.556863, 0.137255, 0.137255, 1);
   clrForestGreen         : TColorVector = (0.137255, 0.556863, 0.137255, 1);
   clrGold                : TColorVector = (0.8,      0.498039, 0.196078, 1);
   clrGoldenrod           : TColorVector = (0.858824, 0.858824, 0.439216, 1);
   clrGreenYellow         : TColorVector = (0.576471, 0.858824, 0.439216, 1);
   clrIndian              : TColorVector = (0.309804, 0.184314, 0.184314, 1);
   clrKhaki               : TColorVector = (0.623529, 0.623529, 0.372549, 1);
   clrLightBlue           : TColorVector = (0.74902,  0.847059, 0.847059, 1);
   clrLightSteelBlue      : TColorVector = (0.560784, 0.560784, 0.737255, 1);
   clrLimeGreen           : TColorVector = (0.196078, 0.8,      0.196078, 1);
   clrMaroon              : TColorVector = (0.556863, 0.137255, 0.419608, 1);
   clrMediumAquamarine    : TColorVector = (0.196078, 0.8,      0.6,      1);
   clrMediumBlue          : TColorVector = (0.196078, 0.196078, 0.8,      1);
   clrMediumForestGreen   : TColorVector = (0.419608, 0.556863, 0.137255, 1);
   clrMediumGoldenrod     : TColorVector = (0.917647, 0.917647, 0.678431, 1);
   clrMediumOrchid        : TColorVector = (0.576471, 0.439216, 0.858824, 1);
   clrMediumSeaGreen      : TColorVector = (0.258824, 0.435294, 0.258824, 1);
   clrMediumSlateBlue     : TColorVector = (0.498039, 0,        1,        1);
   clrMediumSpringGreen   : TColorVector = (0.498039, 1,        0,        1);
   clrMediumTurquoise     : TColorVector = (0.439216, 0.858824, 0.858824, 1);
   clrMediumViolet        : TColorVector = (0.858824, 0.439216, 0.576471, 1);
   clrMidnightBlue        : TColorVector = (0.184314, 0.184314, 0.309804, 1);
   clrNavy                : TColorVector = (0.137255, 0.137255, 0.556863, 1);
   clrNavyBlue            : TColorVector = (0.137255, 0.137255, 0.556863, 1);
   clrOrange              : TColorVector = (1,        0.5,      0.0,      1);
   clrOrangeRed           : TColorVector = (1,        0.25,     0,        1);
   clrOrchid              : TColorVector = (0.858824, 0.439216, 0.858824, 1);
   clrPaleGreen           : TColorVector = (0.560784, 0.737255, 0.560784, 1);
   clrPink                : TColorVector = (0.737255, 0.560784, 0.560784, 1);
   clrPlum                : TColorVector = (0.917647, 0.678431, 0.917647, 1);
   clrSalmon              : TColorVector = (0.435294, 0.258824, 0.258824, 1);
   clrSeaGreen            : TColorVector = (0.137255, 0.556863, 0.419608, 1);
   clrSienna              : TColorVector = (0.556863, 0.419608, 0.137255, 1);
   clrSkyBlue             : TColorVector = (0.196078, 0.6,      0.8,      1);
   clrSlateBlue           : TColorVector = (0,        0.498039, 1,        1);
   clrSpringGreen         : TColorVector = (0,        1,        0.498039, 1);
   clrSteelBlue           : TColorVector = (0.137255, 0.419608, 0.556863, 1);
   clrTan                 : TColorVector = (0.858824, 0.576471, 0.439216, 1);
   clrThistle             : TColorVector = (0.847059, 0.74902,  0.847059, 1);
   clrTurquoise           : TColorVector = (0.678431, 0.917647, 0.917647, 1);
   clrViolet              : TColorVector = (0.309804, 0.184314, 0.309804, 1);
   clrVioletRed           : TColorVector = (0.8,      0.196078, 0.6,      1);
   clrWheat               : TColorVector = (0.847059, 0.847059, 0.74902,  1);
   clrYellowGreen         : TColorVector = (0.6,      0.8,      0.196078, 1);
   clrSummerSky           : TColorVector = (0.22,     0.69,     0.87,     1);
   clrRichBlue            : TColorVector = (0.35,     0.35,     0.67,     1);
   clrBrass               : TColorVector = (0.71,     0.65,     0.26,     1);
   clrCopper              : TColorVector = (0.72,     0.45,     0.20,     1);
   clrBronze              : TColorVector = (0.55,     0.47,     0.14,     1);
   clrBronze2             : TColorVector = (0.65,     0.49,     0.24,     1);
   clrSilver              : TColorVector = (0.90,     0.91,     0.98,     1);
   clrBrightGold          : TColorVector = (0.85,     0.85,     0.10,     1);
   clrOldGold             : TColorVector = (0.81,     0.71,     0.23,     1);
   clrFeldspar            : TColorVector = (0.82,     0.57,     0.46,     1);
   clrQuartz              : TColorVector = (0.85,     0.85,     0.95,     1);
   clrNeonPink            : TColorVector = (1.00,     0.43,     0.78,     1);
   clrDarkPurple          : TColorVector = (0.53,     0.12,     0.47,     1);
   clrNeonBlue            : TColorVector = (0.30,     0.30,     1.00,     1);
   clrCoolCopper          : TColorVector = (0.85,     0.53,     0.10,     1);
   clrMandarinOrange      : TColorVector = (0.89,     0.47,     0.20,     1);
   clrLightWood           : TColorVector = (0.91,     0.76,     0.65,     1);
   clrMediumWood          : TColorVector = (0.65,     0.50,     0.39,     1);
   clrDarkWood            : TColorVector = (0.52,     0.37,     0.26,     1);
   clrSpicyPink           : TColorVector = (1.00,     0.11,     0.68,     1);
   clrSemiSweetChoc       : TColorVector = (0.42,     0.26,     0.15,     1);
   clrBakersChoc          : TColorVector = (0.36,     0.20,     0.09,     1);
   clrFlesh               : TColorVector = (0.96,     0.80,     0.69,     1);
   clrNewTan              : TColorVector = (0.92,     0.78,     0.62,     1);
   clrNewMidnightBlue     : TColorVector = (0.00,     0.00,     0.61,     1);
   clrVeryDarkBrown       : TColorVector = (0.35,     0.16,     0.14,     1);
   clrDarkBrown           : TColorVector = (0.36,     0.25,     0.20,     1);
   clrDarkTan             : TColorVector = (0.59,     0.41,     0.31,     1);
   clrGreenCopper         : TColorVector = (0.32,     0.49,     0.46,     1);
   clrDkGreenCopper       : TColorVector = (0.29,     0.46,     0.43,     1);
   clrDustyRose           : TColorVector = (0.52,     0.39,     0.39,     1);
   clrHuntersGreen        : TColorVector = (0.13,     0.37,     0.31,     1);
   clrScarlet             : TColorVector = (0.55,     0.09,     0.09,     1);
   clrMediumPurple        : TColorVector = (0.73,     0.16,     0.96,     1);
   clrLightPurple         : TColorVector = (0.87,     0.58,     0.98,     1);
   clrVeryLightPurple     : TColorVector = (0.94,     0.81,     0.99,     1);
   clrGreen               : TColorVector = (0,        0.5,      0,        1);
   clrOlive               : TColorVector = (0.5,      0.5,      1,        1);
   clrPurple              : TColorVector = (1,        0,        1,        1);
   clrTeal                : TColorVector = (0,        0.5,      0.5,      1);
   clrRed                 : TColorVector = (1,        0,        0,        1);
   clrLime                : TColorVector = (0,        1,        0,        1);
   clrYellow              : TColorVector = (1,        1,        0,        1);
   clrBlue                : TColorVector = (0,        0,        1,        1);
   clrFuchsia             : TColorVector = (1,        0,        1,        1);
   clrAqua                : TColorVector = (0,        1,        1,        1);

   cDefaultNormalMapScale = 0.125;

type

	PRGBColor = ^TRGBColor;
	TRGBColor = TAffineByteVector;

	TGLTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace);
	TGLTextureWrap = (twBoth, twNone, twVertical, twHorizontal);

	TGLFaceProperties  = class;
	TGLTexture         = class;
	TGLMaterial        = class;
   TGLMaterialLibrary = class;

   TDrawState = (dsRendering, dsPicking, dsPrinting);

   TGLSize = record
      cx : Longint;
      cy : Longint;
   end;

   // TRenderContextInfo
   //
   {: Stores contextual info useful during rendering methods. }
   TRenderContextInfo = record
      scene : TObject;
      buffer : TObject;
      cameraPosition : TVector;
      cameraDirection, cameraUp : TVector;
      modelViewMatrix : PMatrix;
      viewPortSize : TGLSize;
      renderDPI : Integer;
      materialLibrary : TGLMaterialLibrary;
      lightmapLibrary : TGLMaterialLibrary;
      fogDisabledCounter : Integer;
      lightingDisabledCounter : Integer;
      drawState : TDrawState;
      objectsSorting : TGLObjectsSorting;
      visibilityCulling : TGLVisibilityCulling;
      GLStates : TGLStateCache;
      rcci : TRenderContextClippingInfo;
      sceneAmbientColor : TColorVector;
      bufferFaceCull : Boolean;
      proxySubObject : Boolean;
      ignoreMaterials : Boolean;
      ignoreBlendingRequests : Boolean;
      amalgamating : Boolean;
   end;
   PRenderContextInfo = ^TRenderContextInfo;

   // TGLColor
	//
   {: Wraps an OpenGL color. }
   TGLColor = class(TGLUpdateAbleObject)
      private
         { Private Properties }
			FColor : TColorVector;
         FPDefaultColor : PColorVector;
			procedure SetColorVector(const aColor : TColorVector); overload;
			procedure SetColorComponent(index : Integer; value : TGLFloat);
			procedure SetAsWinColor(const val : TColor);
			function GetAsWinColor : TColor;

		protected
         { Protected Properties }
			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

         function GetHSVA : TVector;
         procedure SetHSVA(const hsva : TVector);

		public
         { Public Properties }
			constructor Create(AOwner : TPersistent); override;
			constructor CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);
         destructor Destroy; override;
         procedure NotifyChange(Sender : TObject); override;
			procedure Assign(Source : TPersistent); override;
			procedure Initialize(const color : TColorVector);
			function AsAddress : PGLFloat;

         procedure RandomColor;
         procedure SetColor(red, green, blue : Single; alpha : Single = 1); overload;

			property Color : TColorVector read FColor write SetColorVector;
			property AsWinColor : TColor read GetAsWinColor write SetAsWinColor;
         property HSVA : TVector read GetHSVA write SetHSVA;

         property DefaultColor : TColorVector read FColor;

{$ifndef FPC}
		published
         { Published Properties }
			property Red :   TGLFloat index 0 read FColor[0] write SetColorComponent stored False;
			property Green : TGLFloat index 1 read FColor[1] write SetColorComponent stored False;
			property Blue :  TGLFloat index 2 read FColor[2] write SetColorComponent stored False;
			property Alpha : TGLFloat index 3 read FColor[3] write SetColorComponent stored False;
{$else}
			property Red :   TGLFloat index 0 read FColor[0] write SetColorComponent;
			property Green : TGLFloat index 1 read FColor[1] write SetColorComponent;
			property Blue :  TGLFloat index 2 read FColor[2] write SetColorComponent;
			property Alpha : TGLFloat index 3 read FColor[3] write SetColorComponent;
{$endif}
	end;

   // TTextureNeededEvent
   //
   TTextureNeededEvent = procedure (Sender : TObject; var textureFileName : String) of object;

	TGLTextureChange  = (tcImage, tcParams);
	TGLTextureChanges = set of TGLTextureChange;

	{: Defines how and if Alpha channel is defined for a texture image.<ul>
		<li>tiaDefault : uses the alpha channel in the image if any
		<li>tiaAlphaFromIntensity : the alpha channel value is deduced from other
			RGB components intensity (the brighter, the more opaque)
		<li>tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
			completely transparent, others are completely opaque
		<li>tiaLuminance : the luminance value is calculated for each pixel
			and used for RGB and Alpha values
		<li>tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)
      <li>tiaOpaque : alpha channel is uniformously set to 1.0
      <li>tiaTopLeftPointColorTransparent : points of the same color as the
         top left point of the bitmap are transparent, others are opaque.
      </ul>
   }
	TGLTextureImageAlpha = (tiaDefault, tiaAlphaFromIntensity,
									tiaSuperBlackTransparent, tiaLuminance,
									tiaLuminanceSqrt, tiaOpaque,
                           tiaTopLeftPointColorTransparent,
                           tiaInverseLuminance, tiaInverseLuminanceSqrt);

	// TGLTextureImage
	//
	{: Base class for texture image data.<p>
		Basicly, subclasses are to be considered as different ways of getting
		a HBitmap (interfacing the actual source).<br>
		SubClasses should be registered using RegisterGLTextureImageClass to allow
		proper persistence and editability in the IDE experts. }
	TGLTextureImage = class(TGLUpdateAbleObject)
		private
			FOwnerTexture : TGLTexture;
         FOnTextureNeeded : TTextureNeededEvent;

		protected
			function GetHeight: Integer; virtual; abstract;
			function GetWidth: Integer; virtual; abstract;

         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;

		public
         { Public Properties }
			constructor Create(AOwner : TPersistent); override;
			destructor Destroy; override;

			property OwnerTexture : TGLTexture read FOwnerTexture write FOwnerTexture;
			procedure NotifyChange(Sender : TObject); override;

			{: Request to edit the textureImage.<p>
				Returns True if changes have been made.<br>
				This method may be invoked from the IDE or at run-time. }
			function Edit : Boolean;
			{: Save textureImage to file.<p>
				This may not save a picture, but for instance, parameters, if the
				textureImage is a procedural texture. }
			procedure SaveToFile(const fileName : String); dynamic; abstract;
			{: Load textureImage from a file.<p>
				This may not load a picture, but for instance, parameters, if the
				textureImage is a procedural texture.<br>
            Subclasses should invoke inherited which will take care of the
            "OnTextureNeeded" stuff. }
			procedure LoadFromFile(const fileName : String); dynamic;
			{: Returns a user-friendly denomination for the class.<p>
				This denomination is used for picking a texture image class
				in the IDE expert. }
			class function FriendlyName : String; virtual; abstract;
			{: Returns a user-friendly description for the class.<p>
				This denomination is used for helping the user when picking a
				texture image class in the IDE expert. If it's not overriden,
				takes its value from FriendlyName. }
			class function FriendlyDescription : String; virtual;
         {: Native opengl texture target.<p>
            Usually GL_TEXTURE_2D (default) or GL_TEXTURE_CUBE_MAP_ARB. }
         class function NativeTextureTarget : TGLUInt; virtual;

			{: Request reload/refresh of data upon next use. }
			procedure Invalidate; dynamic;

			{: Returns image's bitmap handle.<p>
            The specified target can be TEXTURE_2D or one of the cube maps targets.<br>
				If the actual image is not a windows bitmap (BMP), descendants should
				take care of properly converting to bitmap. }
			function GetBitmap32(target : TGLUInt) : TGLBitmap32; virtual; abstract;
			{: Request for unloading bitmapData, to free some memory.<p>
				This one is invoked when GLScene no longer needs the Bitmap data
				it got through a call to GetHBitmap.<br>
				Subclasses may ignore this call if the HBitmap was obtained at
				no particular memory cost. }
			procedure ReleaseBitmap32; virtual;

			property Width : Integer read GetWidth;
			property Height : Integer read GetHeight;
	end;

	TGLTextureImageClass = class of TGLTextureImage;

   // TGLTextureImageEditor
   //
   TGLTextureImageEditor = class(TObject)
		public
         { Public Properties }
			{: Request to edit a textureImage.<p>
				Returns True if changes have been made.<br>
				This method may be invoked from the IDE or at run-time. }
			class function Edit(aTexImage : TGLTextureImage) : Boolean; virtual; abstract;
   end;

   TGLTextureImageEditorClass = class of TGLTextureImageEditor;

	// TGLBlankImage
	//
	{: A texture image with no specified content, only a size.<p>
      This texture image type is of use if the context of your texture is
      calculated at run-time (with a TGLMemoryViewer for instance). }
	TGLBlankImage = class(TGLTextureImage)
		private
         { Private Declarations }
			FBitmap : TGLBitmap32;
         FWidth, FHeight : Integer;

		protected
         { Protected Declarations }
         procedure SetWidth(val : Integer);
		   function GetWidth: Integer; override;
         procedure SetHeight(val : Integer);
			function GetHeight: Integer; override;

		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

			function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;
			procedure ReleaseBitmap32; override;

			procedure SaveToFile(const fileName : String); override;
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

		published
         { Published Declarations }
         {: Width of the blank image (for memory allocation). }
			property Width : Integer read GetWidth write SetWidth default 256;
         {: Width of the blank image (for memory allocation). }
			property Height : Integer read GetHeight write SetHeight default 256;
	end;

	// TGLPictureImage
	//
	{: Base class for image data classes internally based on a TPicture. }
	TGLPictureImage = class(TGLTextureImage)
		private
         { Private Declarations }
			FBitmap : TGLBitmap32;
			FGLPicture : TGLPicture;
			FUpdateCounter : Integer;

		protected
         { Protected Declarations }
			function GetHeight: Integer; override;
			function GetWidth: Integer; override;

         function  GetPicture : TGLPicture; 
			procedure SetPicture(const aPicture : TGLPicture);
			procedure PictureChanged(Sender: TObject);

		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

			{: Use this function if you are going to modify the Picture directly.<p>
				Each invokation MUST be balanced by a call to EndUpdate. }
			procedure BeginUpdate;
         {: Ends a direct picture modification session.<p>
            Follows a BeginUpdate. }
			procedure EndUpdate;
			function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;
			procedure ReleaseBitmap32; override;

         {: Holds the image content. }
			property Picture : TGLPicture read GetPicture write SetPicture;
	end;

	// TGLPersistentImage
	//
	{: Stores any image compatible with Delphi's TPicture mechanism.<p>
		The picture's data is actually stored into the DFM, the original
		picture name or path is not remembered. It is similar in behaviour
		to Delphi's TImage.<p>
		Note that if original image is for instance JPEG format, only the JPEG
		data will be stored in the DFM (compact) }
	TGLPersistentImage = class(TGLPictureImage)
		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure SaveToFile(const fileName : String); override;
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

		published
         { Published Declarations }
			property Picture;
	end;

	// TGLPicFileImage
	//
	{: Uses a picture whose data is found in a file (only filename is stored).<p>
      The image is unloaded after upload to OpenGL. }
	TGLPicFileImage = class(TGLPictureImage)
		private
			FPictureFileName : String;
         FAlreadyWarnedAboutMissingFile : Boolean;

		protected
			procedure SetPictureFileName(const val : String);

		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

  			procedure Assign(Source: TPersistent); override;

			//: Only picture file name is saved
			procedure SaveToFile(const fileName : String); override;
         {: Load picture file name or use fileName as picture filename.<p>
            The autodetection is based on the filelength and presence of zeros. }
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

			function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;
			procedure Invalidate; override;

		published
         {: Filename of the picture to use. } 
			property PictureFileName : String read FPictureFileName write SetPictureFileName;
	end;

   // TGLCubeMapTarget
   //
   TGLCubeMapTarget = (cmtPX, cmtNX, cmtPY, cmtNY, cmtPZ, cmtNZ);

	// TGLCubeMapImage
	//
	{: A texture image used for specifying and stroing a cube map.<p>
      Not unlike TGLPictureImage, but storing 6 of them instead of just one.<br>
      Saving & loading as a whole currently not supported. }
	TGLCubeMapImage = class(TGLTextureImage)
		private
         { Private Declarations }
			FBitmap : TGLBitmap32;
			FUpdateCounter : Integer;
         FPicture : array [cmtPX..cmtNZ] of TGLPicture;
		protected
         { Protected Declarations }
		   function GetWidth: Integer; override;
			function GetHeight: Integer; override;
         procedure SetPicture(index : TGLCubeMapTarget; const val : TGLPicture);
         function GetPicture(index : TGLCubeMapTarget) : TGLPicture;

			procedure PictureChanged(Sender: TObject);

		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

			function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;
			procedure ReleaseBitmap32; override;

			{: Use this function if you are going to modify the Picture directly.<p>
				Each invokation MUST be balanced by a call to EndUpdate. }
			procedure BeginUpdate;
			procedure EndUpdate;

			procedure SaveToFile(const fileName : String); override;
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function NativeTextureTarget : TGLUInt; override;

         {: Indexed access to the cube map's sub pictures. }
         property Picture[index : TGLCubeMapTarget] : TGLPicture read GetPicture write SetPicture;

		published
         { Public Declarations }
         {$ifndef FPC}
         property PicturePX : TGLPicture index cmtPX read GetPicture write SetPicture;
         property PictureNX : TGLPicture index cmtNX read GetPicture write SetPicture;
         property PicturePY : TGLPicture index cmtPY read GetPicture write SetPicture;
         property PictureNY : TGLPicture index cmtNY read GetPicture write SetPicture;
         property PicturePZ : TGLPicture index cmtPZ read GetPicture write SetPicture;
         property PictureNZ : TGLPicture index cmtNZ read GetPicture write SetPicture;
         {$endif}
	end;

	// TGLFloatDataImage
	//
	{: A texture image of float data type.<p>
      Currently only support dynamic nvidia 32bit/16bit RGBA. }
	TGLFloatDataImage = class(TGLTextureImage)
		private
         { Private Declarations }
			FBitmap : TGLBitmap32;
         FWidth, FHeight : Integer;

		protected
         { Protected Declarations }
         procedure SetWidth(val : Integer);
		   function GetWidth: Integer; override;
         procedure SetHeight(val : Integer);
			function GetHeight: Integer; override;

// 			property Picture;

		public
         { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

			function GetBitmap32(target : TGLUInt) : TGLBitmap32; override;
			procedure ReleaseBitmap32; override;

			procedure SaveToFile(const fileName : String); override;
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

			class function NativeTextureTarget : TGLUInt; override;      
		published
         { Published Declarations }
         {: Width of the blank image (for memory allocation). }
			property Width : Integer read GetWidth write SetWidth default 256;
         {: Width of the blank image (for memory allocation). }
			property Height : Integer read GetHeight write SetHeight default 256;
	end;


   TGLLibMaterial = Class;

   // TGLShaderStyle
   //
   {: Define GLShader style application relatively to a material.<ul>
      <li>ssHighLevel: shader is applied before material application, and unapplied
            after material unapplication
      <li>ssLowLevel: shader is applied after material application, and unapplied
            before material unapplication
      <li>ssReplace: shader is applied in place of the material (and material
            is completely ignored)
      </ul> }
   TGLShaderStyle = (ssHighLevel, ssLowLevel, ssReplace);

   // TGLShader
   //
   {: Generic, abstract shader class.<p>
      Shaders are modeled here as an abstract material-altering entity with
      transaction-like behaviour. The base class provides basic context and user
      tracking, as well as setup/application facilities.<br>
      Subclasses are expected to provide implementation for DoInitialize,
      DoApply, DoUnApply and DoFinalize. }
   TGLShader = class (TGLUpdateAbleComponent)
	   private
	      { Private Declarations }
         FEnabled : Boolean;
         FLibMatUsers : TList;
         FVirtualHandle : TGLVirtualHandle;
         FShaderStyle : TGLShaderStyle;
         FUpdateCount : Integer;
         FShaderActive, FShaderInitialized : Boolean;

	   protected
			{ Protected Declarations }
         {: Invoked once, before the first call to DoApply.<p>
            The call happens with the OpenGL context being active. }
         procedure DoInitialize; dynamic;
         {: Request to apply the shader.<p>
            Always followed by a DoUnApply when the shader is no longer needed. }
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); virtual; abstract;
         {: Request to un-apply the shader.<p>
            Subclasses can assume the shader has been applied previously.<br>
            Return True to request a multipass. }
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; virtual; abstract;
         {: Invoked once, before the destruction of context or release of shader.<p>
            The call happens with the OpenGL context being active. }
         procedure DoFinalize; dynamic;

         function GetShaderInitialized : Boolean;
         procedure InitializeShader;
         procedure FinalizeShader;
         procedure OnVirtualHandleAllocate(sender : TGLVirtualHandle; var handle : Integer);
         procedure OnVirtualHandleDestroy(sender : TGLVirtualHandle; var handle : Integer);
         procedure SetEnabled(val : Boolean);

         property ShaderInitialized : Boolean read GetShaderInitialized;
         property ShaderActive : Boolean read FShaderActive;

         procedure RegisterUser(libMat : TGLLibMaterial);
         procedure UnRegisterUser(libMat : TGLLibMaterial);

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         {: Subclasses should invoke this function when shader properties are altered. }
			procedure NotifyChange(Sender : TObject); override;
         procedure BeginUpdate;
         procedure EndUpdate;

         {: Apply shader to OpenGL state machine.}
         procedure Apply(var rci : TRenderContextInfo; Sender : TObject);
         {: UnApply shader.<p>
            When returning True, the caller is expected to perform a multipass
            rendering by re-rendering then invoking UnApply again, until a
            "False" is returned. }
         function UnApply(var rci : TRenderContextInfo) : Boolean;

         {: Shader application style (default is ssLowLevel). }
         property ShaderStyle : TGLShaderStyle read FShaderStyle write FShaderStyle;

      published
	      { Published Declarations }
         {: Turns on/off shader application.<p>
            Note that this only turns on/off the shader application, if the
            ShaderStyle is ssReplace, the material won't be applied even if
            the shader is disabled. }
         property Enabled : Boolean read FEnabled write SetEnabled default True;
   end;

   // TGLTextureFormat
   //
   {: Texture format for OpenGL (rendering) use.<p>
      Internally, GLScene handles all "base" images as 32 Bits RGBA, but you can
      specify a generic format to reduce OpenGL texture memory use:<ul>
      <li>tfDefault : uses global default format
      <li>tfRGB : 24 bits RGB, 8 bits per component
      <li>tfRGBA : 32 bits RGBA, 8 bits per component
      <li>tfRGB16 : 16 bits RGB (5, 5, 5)
      <li>tfRGBA16 : 16 bits RGBA (4, 4, 4, 4)
      <li>tfAlpha : 8 Bits Alpha-channel only
      <li>tfLuminance : 8 bits Luminance only
      <li>tfLuminanceAlpha : 16 bits Luminance and Alpha channel (8, 8)
      <li>tfIntensity : 8 bits Intensity only
      <li>tfNormalMap : 24 bits RGB normal map, which is computed from the
         original texture (assumed to be an hightmap)
      </ul><br>The actual representation may differ, f.i. old 16bits boards
      may convert everything to 16bit formats, GeForce boards don't have
      a 24 bits format internally and will convert to 32 bits, etc. }
   TGLTextureFormat = (tfDefault, tfRGB, tfRGBA, tfRGB16, tfRGBA16, tfAlpha,
                       tfLuminance, tfLuminanceAlpha, tfIntensity, tfNormalMap,
                       tfRGBAFloat16, tfRGBAFloat32); // float_type

   // TGLTextureCompression
   //
   {: Texture compression option.<p>
      If OpenGL supports it, this will activate a compressed texture format:<ul>
      <li>tcDefault : uses global default compression option
      <li>tcNone : do not use compression
      <li>tcStandard : use standard compression, average quality, average rate
      <li>tcHighQuality : choose a high-quality, low-speed compression
      <li>tcHighSpeed : choose a high-speed, low-quality compression
      </ul>. }
   TGLTextureCompression = (tcDefault, tcNone, tcStandard, tcHighQuality, tcHighSpeed);

   // TGLTextureFilteringQuality
   //
   TGLTextureFilteringQuality = (tfIsotropic, tfAnisotropic);

   // TGLTextureMappingMode
   //
   TGLTextureMappingMode = (tmmUser, tmmObjectLinear, tmmEyeLinear, tmmSphere,
                            tmmCubeMapReflection, tmmCubeMapNormal,
                            tmmCubeMapLight0, tmmCubeMapCamera);

	// TGLTexture
	//
   {: Defines basic texturing properties.<p>
      You can control texture wrapping, smoothing/filtering and of course define
      the texture map (note that texturing is disabled by default).<p>
      A built-in mechanism (through ImageAlpha) allows auto-generation of an
      Alpha channel for all bitmaps (see TGLTextureImageAlpha). }
	TGLTexture = class (TGLUpdateAbleObject)
		private
	      { Private Declarations }
			FTextureHandle       : TGLTextureHandle;
			FTextureMode         : TGLTextureMode;
			FTextureWrap         : TGLTextureWrap;
         FTextureFormat       : TGLTextureFormat;
			FMinFilter           : TGLMinFilter;
			FMagFilter           : TGLMagFilter;
			FChanges             : TGLTextureChanges;
			FDisabled            : Boolean;
			FImage               : TGLTextureImage;
			FImageAlpha          : TGLTextureImageAlpha;
         FImageBrightness     : Single;
         FImageGamma          : Single;
         FMappingMode         : TGLTextureMappingMode;
         FMapSCoordinates     : TGLCoordinates4;
         FMapTCoordinates     : TGLCoordinates4;
         FOnTextureNeeded     : TTextureNeededEvent;
         FCompression         : TGLTextureCompression;
         FRequiredMemorySize  : Integer;
         FFilteringQuality    : TGLTextureFilteringQuality;
         FTexWidth, FTexHeight : Integer;
         FEnvColor            : TGLColor;
         FNormalMapScale      : Single;

		protected
			{ Protected Declarations }
         procedure NotifyImageChange;
         procedure NotifyParamsChange;

			procedure SetImage(AValue: TGLTextureImage);
			procedure SetImageAlpha(const val : TGLTextureImageAlpha);
         procedure SetImageBrightness(const val : Single);
         function  StoreBrightness : Boolean;
         procedure SetImageGamma(const val : Single);
         function  StoreGamma : Boolean;
			procedure SetMagFilter(AValue: TGLMagFilter);
			procedure SetMinFilter(AValue: TGLMinFilter);
			procedure SetTextureMode(AValue: TGLTextureMode);
			procedure SetTextureWrap(AValue: TGLTextureWrap);
         procedure SetTextureFormat(const val : TGLTextureFormat);
         procedure SetCompression(const val : TGLTextureCompression);
         procedure SetFilteringQuality(const val : TGLTextureFilteringQuality);
         procedure SetMappingMode(const val : TGLTextureMappingMode);
         function  GetMappingSCoordinates : TGLCoordinates4;
         procedure SetMappingSCoordinates(const val : TGLCoordinates4);
         function  GetMappingTCoordinates : TGLCoordinates4;
         procedure SetMappingTCoordinates(const val : TGLCoordinates4);
			procedure SetDisabled(AValue: Boolean);
         procedure SetEnabled(const val : Boolean);
         function GetEnabled : Boolean;
         procedure SetEnvColor(const val : TGLColor);
         procedure SetNormalMapScale(const val : Single);
         function StoreNormalMapScale : Boolean;

         function StoreImageClassName : Boolean;

			function GetHandle: TGLuint; virtual;
			//: Load texture to OpenGL subsystem
			procedure PrepareImage(target : TGLUInt); virtual;
			//: Setup OpenGL texture parameters
			procedure PrepareParams(target : TGLUInt); virtual;

         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;
         procedure DoOnTextureNeeded(Sender : TObject; var textureFileName : String);

		public
	      { Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor  Destroy; override;

         procedure PrepareBuildList;
         procedure ApplyMappingMode;
         procedure UnApplyMappingMode;
			procedure Apply(var rci : TRenderContextInfo);
         procedure UnApply(var rci : TRenderContextInfo);
         {: Applies to TEXTURE1_ARB }
         procedure ApplyAsTexture2(var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
         procedure UnApplyAsTexture2(var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
         {: N=1 for TEXTURE0_ARB, N=2 for TEXTURE1_ARB, etc. }
         procedure ApplyAsTextureN(n : Integer; var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
         procedure UnApplyAsTextureN(n : Integer; var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);

			procedure Assign(Source: TPersistent); override;
			procedure NotifyChange(Sender : TObject); override;

			procedure DestroyHandles;

			procedure SetImageClassName(const val : String);
			function GetImageClassName : String;

         {: Returns the OpenGL memory used by the texture.<p>
            The compressed size is returned if, and only if texture compression
            if active and possible, and the texture has been allocated (Handle
            is defined), otherwise the estimated size (from TextureFormat
            specification) is returned. }
         function TextureImageRequiredMemory : Integer;
         {: Allocates the texture handle if not already allocated.<p>
            The texture is binded and parameters are setup, but no image data
            is initialized by this call - for expert use only. }
         function AllocateHandle : TGLuint;
         function IsHandleAllocated : Boolean;
         {: Returns OpenGL texture format corresponding to current options. }
         function OpenGLTextureFormat : Integer;
         {: Returns if of float data type}
         function IsFloatType: boolean;

         {: Is the texture enabled?.<p>
            Always equals to 'not Disabled'. }
         property Enabled : Boolean read GetEnabled write SetEnabled;
         {: Handle to the OpenGL texture object.<p>
            If the handle hasn't already been allocated, it will be allocated
            by this call (ie. do not use if no OpenGL context is active!) }
			property Handle : TGLuint read GetHandle;

         {: Actual width used for last texture specification binding. }
         property TexWidth : Integer read FTexWidth;
         {: Actual width used for last texture specification binding. }
         property TexHeight : Integer read FTexHeight;

		published
	      { Published Declarations }

			{: Image ClassName for enabling True polymorphism.<p>
				This is ugly, but since the default streaming mechanism does a
				really bad job at storing	polymorphic owned-object properties,
				and neither TFiler nor TPicture allow proper use of the built-in
				streaming, that's the only way I found to allow a user-extensible
				mechanism. }
			property ImageClassName : String read GetImageClassName write SetImageClassName stored StoreImageClassName;
			{: Image data for the texture.<p> }
			property Image: TGLTextureImage read FImage write SetImage;

         {: Automatic Image Alpha setting.<p>
            Allows to control how and if the image's Alpha channel (transparency)
            is computed. }
			property ImageAlpha : TGLTextureImageAlpha read FImageAlpha write SetImageAlpha default tiaDefault;
         {: Texture brightness correction.<p>
            This correction is applied upon loading a TGLTextureImage, it's a
            simple saturating scaling applied to the RGB components of
            the 32 bits image, before it is passed to OpenGL, and before
            gamma correction (if any). }
         property ImageBrightness : Single read FImageBrightness write SetImageBrightness stored StoreBrightness;
         {: Texture gamma correction.<p>
            The gamma correction is applied upon loading a TGLTextureImage,
            applied to the RGB components of the 32 bits image, before it is
            passed to OpenGL, after brightness correction (if any). }
         property ImageGamma : Single read FImageGamma write SetImageGamma stored StoreGamma;

         {: Texture magnification filter. }
			property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maLinear;
         {: Texture minification filter. }
			property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miLinearMipMapLinear;
         {: Texture application mode. }
			property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode default tmDecal;
         {: Wrapping mode for the texture. }
			property TextureWrap: TGLTextureWrap read FTextureWrap write SetTextureWrap default twBoth;

         {: Texture format for use by the renderer.<p>
            See TGLTextureFormat for details. }
         property TextureFormat : TGLTextureFormat read FTextureFormat write SetTextureFormat default tfDefault;
         {: Texture compression control.<p>
            If True the compressed TextureFormat variant (the OpenGL ICD must
            support GL_ARB_texture_compression, or this option is ignored). }
         property Compression : TGLTextureCompression read FCompression write SetCompression default tcDefault;
         {: Specifies texture filtering quality.<p>
            You can choose between bilinear and trilinear filetring (anisotropic).<p>
            The OpenGL ICD must support GL_EXT_texture_filter_anisotropic or
            this property is ignored. }
         property FilteringQuality : TGLTextureFilteringQuality read FFilteringQuality write SetFilteringQuality default tfIsotropic;

         {: Texture coordinates mapping mode.<p>
            This property controls automatic texture coordinates generation. }
         property MappingMode : TGLTextureMappingMode read FMappingMode write SetMappingMode default tmmUser;
         {: Texture mapping coordinates mode for S axis.<p>
            This property stores the coordinates for automatic texture
            coordinates generation. }
         property MappingSCoordinates : TGLCoordinates4 read GetMappingSCoordinates write SetMappingSCoordinates;
         {: Texture mapping coordinates mode for T axis.<p>
            This property stores the coordinates for automatic texture
            coordinates generation. }
         property MappingTCoordinates : TGLCoordinates4 read GetMappingTCoordinates write SetMappingTCoordinates;

         {: Texture Environment color. }
         property EnvColor : TGLColor read FEnvColor write SetEnvColor;

         {: If true, the texture is disabled (not used). }
			property Disabled: Boolean read FDisabled write SetDisabled default True;

         {: Normal Map scaling.<p>
            Only applies when TextureFormat is tfNormalMap, this property defines
            the scaling that is applied during normal map generation (ie. controls
            the intensity of the bumps). }
         property NormalMapScale : Single read FNormalMapScale write SetNormalMapScale stored StoreNormalMapScale;
	end;

   // TGLTextureExItem
   //
   TGLTextureExItem = class (TCollectionItem)
      private
         { Private Decalarations }
         FTexture : TGLTexture;
         FTextureIndex : Integer;
         FTextureOffset, FTextureScale : TGLCoordinates;
         FTextureMatrixIsIdentity : Boolean;
         FTextureMatrix : TMatrix;
         FApplied : Boolean;

      protected
         { Protected Decalarations }
         function GetDisplayName : String; override;
         function GetOwner : TPersistent; override;
         procedure SetTexture(const Value : TGLTexture);
         procedure SetTextureIndex(const Value : Integer);
         procedure SetTextureOffset(const Value : TGLCoordinates);
         procedure SetTextureScale(const Value : TGLCoordinates);
         procedure NotifyTexMapChange(Sender : TObject);

         procedure CalculateTextureMatrix;

         procedure OnNotifyChange(Sender : TObject);

      public
         { Public Decalarations }
         constructor Create(ACollection : TCollection); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         procedure NotifyChange(Sender : TObject);

         procedure Apply(var rci : TRenderContextInfo);
         procedure UnApply(var rci : TRenderContextInfo);

      published
         { Published Decalarations }
         property Texture : TGLTexture read FTexture write SetTexture;
         property TextureIndex : Integer read FTextureIndex write SetTextureIndex;
         property TextureOffset : TGLCoordinates read FTextureOffset write SetTextureOffset;
         property TextureScale : TGLCoordinates read FTextureScale write SetTextureScale;

   end;

   // TGLTextureEx
   //
   TGLTextureEx = class (TCollection)
      private
         FMaterial : TGLMaterial;

      protected
         { Protected Decalarations }
         procedure SetItems(index : Integer; const Value : TGLTextureExItem);
         function GetItems(index : Integer) : TGLTextureExItem;
         function GetOwner : TPersistent; override;
         procedure Loaded;

      public
         { Public Decalarations }
         constructor Create(AOwner : TGLMaterial);

         procedure NotifyChange(Sender : TObject);
         procedure Apply(var rci : TRenderContextInfo);
         procedure UnApply(var rci : TRenderContextInfo);
         function IsTextureEnabled(Index : Integer) : Boolean;

         function Add : TGLTextureExItem;

         property Items[index : Integer] : TGLTextureExItem read GetItems write SetItems; default;
   end;

	TShininess = 0..128;
   TPolygonMode = (pmFill, pmLines, pmPoints);

   // TGLFaceProperties
   //
   {: Stores basic face lighting properties.<p>
      The lighting is described with the standard ambient/diffuse/emission/specular
      properties that behave like those of most rendering tools.<br>
      You also have control over shininess (governs specular lighting) and
      polygon mode (lines / fill). }
	TGLFaceProperties = class (TGLUpdateAbleObject)
	   private
	      { Private Declarations }
         FAmbient, FDiffuse, FSpecular, FEmission  : TGLColor;
         FPolygonMode : TPolygonMode;
         FShininess : TShininess;

      protected
	      { Protected Declarations }
         procedure SetAmbient(AValue: TGLColor);
         procedure SetDiffuse(AValue: TGLColor);
         procedure SetEmission(AValue: TGLColor);
         procedure SetSpecular(AValue: TGLColor);
         procedure SetPolygonMode(AValue: TPolygonMode);
         procedure SetShininess(AValue: TShininess);

	   public
			{ Public Declarations }
         constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure Apply(var rci : TRenderContextInfo; aFace : TGLEnum);
         procedure ApplyNoLighting(var rci : TRenderContextInfo; aFace : TGLEnum);
         procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }
         property Ambient : TGLColor read FAmbient write SetAmbient;
         property Diffuse : TGLColor read FDiffuse write SetDiffuse;
         property Emission : TGLColor read FEmission write SetEmission;
         property Shininess : TShininess read FShininess write SetShininess default 0;
         property PolygonMode : TPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
         property Specular : TGLColor read FSpecular write SetSpecular;
   end;

   TGLLibMaterialName = String;

   // TBlendingMode
   //
   {: Simplified blending options.<p>
      bmOpaque : disable blending<br>
      bmTransparency : uses standard alpha blending<br>
      bmAdditive : activates additive blending (with saturation)<br>
      bmAlphaTest50 : uses opaque blending, with alpha-testing at 50% (full
         transparency if alpha is below 0.5, full opacity otherwise)<br>
      bmAlphaTest100 : uses opaque blending, with alpha-testing at 100%<br>
      bmModulate : uses modulation blending }
   TBlendingMode = (bmOpaque, bmTransparency, bmAdditive,
                    bmAlphaTest50, bmAlphaTest100, bmModulate);

   // TFaceCulling
   //
   TFaceCulling = (fcBufferDefault, fcCull, fcNoCull);

   // TMaterialOptions
   //
   {: Control special rendering options for a material.<p>
      moIgnoreFog : fog is deactivated when the material is rendered }
   TMaterialOption = (moIgnoreFog, moNoLighting);
   TMaterialOptions = set of TMaterialOption;

	// TGLMaterial
   //
   {: Describes a rendering material.<p>
      A material is basicly a set of face properties (front and back) that take
      care of standard material rendering parameters (diffuse, ambient, emission
      and specular) and texture mapping.<br>
      An instance of this class is available for almost all objects in GLScene
      to allow quick definition of material properties. It can link to a
      TGLLibMaterial (taken for a material library).<p>
      The TGLLibMaterial has more adavanced properties (like texture transforms)
      and provides a standard way of sharing definitions and texture maps. }
	TGLMaterial = class (TGLUpdateAbleObject)
      private
	      { Private Declarations }
         FFrontProperties, FGLBackProperties : TGLFaceProperties;
			FBlendingMode : TBlendingMode;
         FTexture : TGLTexture;
         FTextureEx : TGLTextureEx;
         FMaterialLibrary : TGLMaterialLibrary;
         FLibMaterialName : TGLLibMaterialName;
         FMaterialOptions : TMaterialOptions;
         FFaceCulling : TFaceCulling;
         currentLibMaterial : TGLLibMaterial;

	   protected
	      { Protected Declarations }
         function GetBackProperties : TGLFaceProperties;
         procedure SetBackProperties(Values: TGLFaceProperties);
         procedure SetFrontProperties(Values: TGLFaceProperties);
         procedure SetBlendingMode(const val : TBlendingMode);
         procedure SetMaterialOptions(const val : TMaterialOptions);
         function  GetTexture : TGLTexture;
         procedure SetTexture(ATexture: TGLTexture);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetLibMaterialName(const val : TGLLibMaterialName);
         procedure SetFaceCulling(const val : TFaceCulling);
         function  GetTextureEx : TGLTextureEx;
         procedure SetTextureEx(const value : TGLTextureEx);
         function  StoreTextureEx : Boolean;

			procedure NotifyLibMaterialDestruction;
			//: Back, Front, Texture and blending not stored if linked to a LibMaterial
			function StoreMaterialProps : Boolean;

		public
			{ Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

         procedure PrepareBuildList;
			procedure Apply(var rci : TRenderContextInfo);
         {: Restore non-standard material states that were altered;<p>
            A return value of True is a multipass request. }
         function  UnApply(var rci : TRenderContextInfo) : Boolean;
			procedure Assign(Source: TPersistent); override;
			procedure NotifyChange(Sender : TObject); override;
			procedure NotifyTexMapChange(Sender : TObject);
         procedure DestroyHandles;

         procedure Loaded;

         {: Returns True if the material is blended.<p>
            Will return the libmaterial's blending if it is linked to a material
            library. }
         function Blended : Boolean;

         //: True if the material has a secondary texture
         function HasSecondaryTexture : Boolean;

         //: True if the material comes from the library instead of the texture property
         function MaterialIsLinkedToLib : Boolean;

         //: Gets the primary texture either from material library or the texture property
         function GetActualPrimaryTexture: TGLTexture;

		published
			{ Published Declarations }
			property BackProperties: TGLFaceProperties read GetBackProperties write SetBackProperties stored StoreMaterialProps;
			property FrontProperties: TGLFaceProperties read FFrontProperties write SetFrontProperties stored StoreMaterialProps;
			property BlendingMode : TBlendingMode read FBlendingMode write SetBlendingMode stored StoreMaterialProps default bmOpaque;
         property MaterialOptions : TMaterialOptions read FMaterialOptions write SetMaterialOptions default [];
			property Texture : TGLTexture read GetTexture write SetTexture stored StoreMaterialProps;
         property FaceCulling : TFaceCulling read FFaceCulling write SetFaceCulling default fcBufferDefault;

			property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
			property LibMaterialName : TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
         property TextureEx : TGLTextureEx read GetTextureEx write SetTextureEx stored StoreTextureEx;
	  end;

	// TGLLibMaterial
	//
   {: Material in a material library.<p>
      Introduces Texture transformations (offset and scale). Those transformations
      are available only for lib materials to minimize the memory cost of basic
      materials (which are used in almost all objects). }
	TGLLibMaterial = class (TCollectionItem)
	   private
	      { Private Declarations }
         userList : TList;
         FName : TGLLibMaterialName;
         FNameHashKey : Integer;
         FMaterial : TGLMaterial;
         FTextureOffset, FTextureScale : TGLCoordinates;
         FTextureMatrixIsIdentity : Boolean;
         FTextureMatrix : TMatrix;
         FTexture2Name : TGLLibMaterialName;
         FShader : TGLShader;
         notifying : Boolean; // used for recursivity protection
         libMatTexture2 : TGLLibMaterial; // internal cache
         FTag : Integer;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure Loaded;

         class function ComputeNameHashKey(const name : String) : Integer;

         procedure SetName(const val : TGLLibMaterialName);
         procedure SetMaterial(const val : TGLMaterial);
         procedure SetTextureOffset(const val : TGLCoordinates);
         procedure SetTextureScale(const val : TGLCoordinates);
         procedure SetTexture2Name(const val : TGLLibMaterialName);
         procedure SetShader(const val : TGLShader);

         procedure CalculateTextureMatrix;
         procedure DestroyHandles;
         procedure OnNotifyChange(Sender : TObject);
         procedure DoOnTextureNeeded(Sender : TObject; var textureFileName : String);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;

         procedure PrepareBuildList;
			procedure Apply(var rci : TRenderContextInfo);
         //: Restore non-standard material states that were altered
         function  UnApply(var rci : TRenderContextInfo) : Boolean;

         procedure RegisterUser(obj : TGLUpdateAbleObject); overload;
			procedure UnregisterUser(obj : TGLUpdateAbleObject); overload;
         procedure RegisterUser(comp : TGLUpdateAbleComponent); overload;
			procedure UnregisterUser(comp : TGLUpdateAbleComponent); overload;
         procedure RegisterUser(libMaterial : TGLLibMaterial); overload;
			procedure UnregisterUser(libMaterial : TGLLibMaterial); overload;
         procedure NotifyUsers;
         procedure NotifyUsersOfTexMapChange;

         property NameHashKey : Integer read FNameHashKey;

	   published
	      { Published Declarations }
         property Name : TGLLibMaterialName read FName write SetName;
         property Material : TGLMaterial read FMaterial write SetMaterial;
         property Tag : Integer read FTag write FTag;

         {: Texture offset in texture coordinates.<p>
            The offset is applied <i>after</i> scaling. }
         property TextureOffset : TGLCoordinates read FTextureOffset write SetTextureOffset;
         {: Texture coordinates scaling.<p>
            Scaling is applied <i>before</i> applying the offset, and is applied
            to the texture coordinates, meaning that a scale factor of (2, 2, 2)
            will make your texture look twice <i>smaller</i>. }
         property TextureScale : TGLCoordinates read FTextureScale write SetTextureScale;

         {: Reference to the second texture.<p>
            The referred LibMaterial *must* be in the same material library.<p>
            Second textures are supported only through ARB multitexturing (ignored
            if not supported). }
         property Texture2Name : TGLLibMaterialName read FTexture2Name write SetTexture2Name;

         {: Optionnal shader for the material. }
         property Shader : TGLShader read FShader write SetShader;
	end;

	// TGLLibMaterials
	//
   {: A collection of materials, mainly used in material libraries. }
	TGLLibMaterials = class (TOwnedCollection)
	   private
	      { Protected Declarations }

	   protected
	      { Protected Declarations }
         procedure Loaded;

         procedure SetItems(index : Integer; const val : TGLLibMaterial);
	      function GetItems(index : Integer) : TGLLibMaterial;
         procedure DestroyHandles;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

         function Owner : TPersistent;

         function Add: TGLLibMaterial;
	      function FindItemID(ID: Integer): TGLLibMaterial;
	      property Items[index : Integer] : TGLLibMaterial read GetItems write SetItems; default;
         function MakeUniqueName(const nameRoot : TGLLibMaterialName) : TGLLibMaterialName;
         function GetLibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;
         procedure PrepareBuildList;
         procedure SetNamesToTStrings(aStrings : TStrings);
         {: Deletes all the unused materials in the collection.<p>
            A material is considered unused if no other material references it. }
         procedure DeleteUnusedMaterials;
   end;

   // TGLMaterialLibrary
   //
   {: Stores a set of materials, to be used and shared by scene objects.<p>
      Use a material libraries for storing commonly used materials, it provides
      an efficient way to share texture and material data among many objects,
      thus reducing memory needs and rendering time.<p>
      Materials in a material library also feature advanced control properties
      like texture coordinates transforms. }  
   TGLMaterialLibrary = class (TGLCadenceAbleComponent)
	   private
	      { Private Declarations }
         FDoNotClearMaterialsOnLoad : Boolean;
         FMaterials : TGLLibMaterials;
         FTexturePaths : String;
         FOnTextureNeeded : TTextureNeededEvent;
         FTexturePathList : TStringList;
         FLastAppliedMaterial : TGLLibMaterial;

	   protected
			{ Protected Declarations }
         procedure Loaded; override;
         procedure SetMaterials(const val : TGLLibMaterials);
         function StoreMaterials : Boolean;
         procedure SetTexturePaths(const val : String);

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure DestroyHandles;

         procedure WriteToFiler(writer : TVirtualWriter);
   	   procedure ReadFromFiler(reader : TVirtualReader);
	      procedure SaveToStream(aStream : TStream); dynamic;
	      procedure LoadFromStream(aStream : TStream); dynamic;
         procedure AddMaterialsFromStream(aStream : TStream);

         {: Save library content to a file.<p>
            Recommended extension : .GLML<br>
            Currently saves only texture, ambient, diffuse, emission
            and specular colors. }
	      procedure SaveToFile(const fileName : String);
	      procedure LoadFromFile(const fileName : String);
         procedure AddMaterialsFromFile(const fileName : String);

         {: Add a "standard" texture material.<p>
            "standard" means linear texturing mode with mipmaps and texture
            modulation mode with default-strength color components.<br>
            If persistent is True, the image will be loaded oersistently in memory
            (via a TGLPersistentImage), if false, it will be unloaded after upload
            to OpenGL (via TGLPicFileImage). }
         function AddTextureMaterial(const materialName, fileName : String;
                                     persistent : Boolean = True) : TGLLibMaterial; overload;
         {: Add a "standard" texture material.<p>
            TGLGraphic based variant. }
         function AddTextureMaterial(const materialName : String; graphic : TGLGraphic) : TGLLibMaterial; overload;

         {: Returns libMaterial of given name if any exists. }
         function LibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;

         {: Applies the material of given name.<p>
            Returns False if the material could not be found. ake sure this
            call is balanced with a corresponding UnApplyMaterial (or an
            assertion will be triggered in the destructor).<br>
            If a material is already applied, and has not yet been unapplied,
            an assertion will be triggered. }
         function ApplyMaterial(const materialName : String; var rci : TRenderContextInfo) : Boolean;
         {: Un-applies the last applied material.<p>
            Use this function in conjunction with ApplyMaterial.<br>
            If no material was applied, an assertion will be triggered. }
         function UnApplyMaterial(var rci : TRenderContextInfo) : Boolean;

      published
	      { Published Declarations }
         {: The materials collection. }
         property Materials : TGLLibMaterials read FMaterials write SetMaterials stored StoreMaterials;
         {: Paths to lookup when attempting to load a texture.<p>
            You can specify multiple paths when loading a texture, the separator
            being the semi-colon ';' character. Directories are looked up from
            first to last, the first file name match is used.<br>
            The current directory is always implicit and checked last.<p>
            Note that you can also use the OnTextureNeeded event to provide a
            filename. }
         property TexturePaths : String read FTexturePaths write SetTexturePaths;
         {: This event is fired whenever a texture needs to be loaded from disk.<p>
            The event is triggered before even attempting to load the texture,
            and before TexturePaths is used. }
         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;

   end;

   PColorEntry = ^TColorEntry;
   TColorEntry = record
                   Name  : String[31];
                   Color : TColorVector;
                 end;

   // TGLColorManager
   //
   TGLColorManager = class (TList)
      public
         destructor Destroy; override;
         
         procedure AddColor(const aName: String; const aColor: TColorVector);
         procedure EnumColors(Proc: TGetStrProc);
         function  FindColor(const aName: String): TColorVector;
         {: Convert a clrXxxx or a '<red green blue alpha> to a color vector }
         function  GetColor(const aName: String): TColorVector;
         function  GetColorName(const aColor: TColorVector): String;
         procedure RegisterDefaultColors;
         procedure RemoveColor(const aName: String);
   end;

   ETexture = class (Exception);

   
function ColorManager: TGLColorManager;

//: Converts a color vector (containing float values)
function ConvertColorVector(const AColor : TColorVector) : TColor; overload;
{: Converts a color vector (containing float values) and alter intensity.<p>
   intensity is in [0..1] }
function ConvertColorVector(const AColor: TColorVector; intensity : Single) : TColor; overload;
//: Converts RGB components into a color vector with correct range
function ConvertRGBColor(const aColor: array of Byte) : TColorVector;
//: Converts a delphi color into its RGB fragments and correct range
function ConvertWinColor(aColor: TColor; alpha : Single = 1) : TColorVector;

procedure RegisterColor(const aName : String; const aColor : TColorVector);
procedure UnRegisterColor(const aName : String);

//: Register a TGLTextureImageClass (used for persistence and IDE purposes)
procedure RegisterGLTextureImageClass(textureImageClass : TGLTextureImageClass);
//: Finds a registerer TGLTextureImageClass using its classname
function FindGLTextureImageClass(const className : String) : TGLTextureImageClass;
//: Finds a registerer TGLTextureImageClass using its FriendlyName
function FindGLTextureImageClassByFriendlyName(const friendlyName : String) : TGLTextureImageClass;
//: Defines a TStrings with the list of registered TGLTextureImageClass.
procedure SetGLTextureImageClassesToStrings(aStrings : TStrings);
{: Creates a TStrings with the list of registered TGLTextureImageClass.<p>
	To be freed by caller. }
function GetGLTextureImageClassesAsStrings : TStrings;

// Global texturing defaults
//
var
   vDefaultTextureFormat : TGLTextureFormat = tfRGBA;
   vDefaultTextureCompression : TGLTextureCompression = tcNone;

//: Invokes the editor for the given TGLTextureImage
function EditGLTextureImage(aTexImage : TGLTextureImage) : Boolean;
procedure RegisterGLTextureImageEditor(aTexImageClass : TGLTextureImageClass;
                                       texImageEditor : TGLTextureImageEditorClass);
procedure UnRegisterGLTextureImageEditor(texImageEditor : TGLTextureImageEditorClass);

procedure RegisterTGraphicClassFileExtension(const extension : String;
                                             const aClass : TGraphicClass);
function CreateGraphicFromFile(const fileName : String) : TGLGraphic;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses GLScene, GLStrings, XOpenGL, ApplicationFileIO, PictureRegisteredFormats;

{$Q-} // no range checking

var
	vGLTextureImageClasses : TList;
	vColorManager : TGLColorManager;
   vTIEClass, vTIEEditor : TList;

const
	cTextureMode : array [tmDecal..tmReplace] of TGLEnum =
							( GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE );

var
   vTGraphicFileExtension : array of String;
   vTGraphicClass : array of TGraphicClass;

// RegisterTGraphicClassFileExtension
//
procedure RegisterTGraphicClassFileExtension(const extension : String;
                                             const aClass : TGraphicClass);
var
   n : Integer;
begin
   n:=Length(vTGraphicFileExtension);
   SetLength(vTGraphicFileExtension, n+1);
   SetLength(vTGraphicClass, n+1);
   vTGraphicFileExtension[n]:=LowerCase(extension);
   vTGraphicClass[n]:=aClass;
end;

// CreateGraphicFromFile
//
function CreateGraphicFromFile(const fileName : String) : TGLGraphic;
var
   i : Integer;
   ext : String;
   fs : TStream;
   graphicClass : TGraphicClass;
begin
   Result:=nil;
   if FileStreamExists(fileName) then begin
      graphicClass:=nil;
      ext:=LowerCase(ExtractFileExt(fileName));
      for i:=0 to High(vTGraphicFileExtension) do begin
         if vTGraphicFileExtension[i]=ext then begin
            graphicClass:=TGraphicClass(vTGraphicClass[i]);
            Break;
         end;
      end;
      if graphicClass=nil then
         graphicClass:=GraphicClassForExtension(ext);
      if graphicClass<>nil then begin
         Result:=graphicClass.Create;
         try
            fs:=CreateFileStream(fileName, fmOpenRead);
            try
               Result.LoadFromStream(fs);
            finally
               fs.Free;
            end;
         except
            FreeAndNil(Result);
            raise;
         end;
      end;
   end;
end;

// EditGLTextureImage
//
function EditGLTextureImage(aTexImage : TGLTextureImage) : Boolean;
var
   i : Integer;
   editor : TGLTextureImageEditorClass;
begin
   if Assigned(vTIEClass) then begin
      i:=vTIEClass.IndexOf(Pointer(aTexImage.ClassType));
      if i>=0 then begin
         editor:=TGLTextureImageEditorClass(vTIEEditor[i]);
         Result:=editor.Edit(aTexImage);
         Exit;
      end;
   end;
   InformationDlg(aTexImage.ClassName+': editing not supported.');
   Result:=False;
end;

// RegisterGLTextureImageEditor
//
procedure RegisterGLTextureImageEditor(aTexImageClass : TGLTextureImageClass;
                                       texImageEditor : TGLTextureImageEditorClass);
begin
   if not Assigned(vTIEClass) then begin
      vTIEClass:=TList.Create;
      vTIEEditor:=TList.Create;
   end;
   vTIEClass.Add(Pointer(aTexImageClass));
   vTIEEditor.Add(texImageEditor);
end;

// UnRegisterGLTextureImageEditor
//
procedure UnRegisterGLTextureImageEditor(texImageEditor : TGLTextureImageEditorClass);
var
   i : Integer;
begin
   if Assigned(vTIEClass) then begin
      i:=vTIEEditor.IndexOf(texImageEditor);
      if i>=0 then begin
         vTIEClass.Delete(i);
         vTIEEditor.Delete(i);
      end;
   end;
end;

// ColorManager
//
function ColorManager : TGLColorManager;
begin
	if not Assigned(vColorManager) then begin
		vColorManager:=TGLColorManager.Create;
		vColorManager.RegisterDefaultColors;
	end;
	Result:=vColorManager;
end;

// RegisterGLTextureImageClass
//
procedure RegisterGLTextureImageClass(textureImageClass : TGLTextureImageClass);
begin
	if not Assigned(vGLTextureImageClasses) then
		vGLTextureImageClasses:=TList.Create;
	vGLTextureImageClasses.Add(textureImageClass);
end;

// FindGLTextureImageClass
//
function FindGLTextureImageClass(const className : String) : TGLTextureImageClass;
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	Result:=nil;
	if Assigned(vGLTextureImageClasses) then
		for i:=0 to vGLTextureImageClasses.Count-1 do begin
			tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
			if tic.ClassName=className then begin
				Result:=tic;
				Break;
			end;
		end;

end;

// FindGLTextureImageClassByFriendlyName
//
function FindGLTextureImageClassByFriendlyName(const friendlyName : String) : TGLTextureImageClass;
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	Result:=nil;
	if Assigned(vGLTextureImageClasses) then
		for i:=0 to vGLTextureImageClasses.Count-1 do begin
			tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
			if tic.FriendlyName=friendlyName then begin
				Result:=tic;
				Break;
			end;
		end;
end;

// SetGLTextureImageClassesToStrings
//
procedure SetGLTextureImageClassesToStrings(aStrings : TStrings);
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	with aStrings do begin
		BeginUpdate;
		Clear;
		if Assigned(vGLTextureImageClasses) then
			for i:=0 to vGLTextureImageClasses.Count-1 do begin
				tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
				AddObject(tic.FriendlyName, Pointer(tic));
			end;
		EndUpdate;
	end;
end;

// GetGLTextureImageClassesAsStrings
//
function GetGLTextureImageClassesAsStrings : TStrings;
begin
	Result:=TStringList.Create;
	SetGLTextureImageClassesToStrings(Result);
end;

// IncludeTrailingBackslash
//
function IncludeTrailingBackslash(const s : string): string;
begin
   if IsDelimiter('\', s, Length(s)-1) then
      Result:=s+'\'
   else Result:=s;
end;

// ------------------
// ------------------ TGLColor ------------------
// ------------------

// Create
//
constructor TGLColor.Create(AOwner: TPersistent);
begin
   inherited;
   Initialize(clrBlack);
end;

// CreateInitialized
//
constructor TGLColor.CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);
begin
   Create(AOwner);
   Initialize(color);
   OnNotifyChange:=changeEvent;
end;

// Destroy
//
destructor TGLColor.Destroy;
begin
   Dispose(FPDefaultColor);
   inherited;
end;

// Initialize
//
procedure TGLColor.Initialize(const color : TColorVector);
begin
	SetVector(FColor, color);
   if vUseDefaultSets then begin
      New(FPDefaultColor);
   	SetVector(FPDefaultColor^, color);
   end;
end;

// SetColorVector
//
procedure TGLColor.SetColorVector(const aColor : TColorVector);
begin
   SetVector(FColor, AColor);
	NotifyChange(Self);
end;

// SetColorComponent
//
procedure TGLColor.SetColorComponent(index : Integer; value : TGLFloat);
begin
	if FColor[index]<>value then begin
		FColor[index]:=value;
		NotifyChange(Self);
	end;
end;

// SetAsWinColor
//
procedure TGLColor.SetAsWinColor(const val : TColor);
begin
	FColor:=ConvertWinColor(val);
	NotifyChange(Self);
end;

// GetAsWinColor
//
function TGLColor.GetAsWinColor : TColor;
begin
	Result:=ConvertColorVector(FColor);
end;

// Assign
//
procedure TGLColor.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLColor) then begin
		FColor:=TGLColor(Source).FColor;
      NotifyChange(Self);
   end else inherited;
end;

// DefineProperties
//
procedure TGLColor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Color', ReadData, WriteData,
                             not (Assigned(FPDefaultColor) and VectorEquals(FColor, FPDefaultColor^)));
end;

// ReadData
//
procedure TGLColor.ReadData(Stream: TStream);
begin
   Stream.Read(FColor, SizeOf(FColor));
end;

// WriteData
//
procedure TGLColor.WriteData(Stream: TStream);
begin
   Stream.Write(FColor, SizeOf(FColor));
end;

// NotifyChange
//
procedure TGLColor.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then begin
      if Owner is TGLBaseSceneObject then
         TGLBaseSceneObject(Owner).StructureChanged;
      inherited;
   end;
end;

// AsAddress
//
function TGLColor.AsAddress: PGLFloat;
begin
	Result:=@FColor;
end;

// RandomColor
//
procedure TGLColor.RandomColor;
begin
   Red:=Random;
   Green:=Random;
   Blue:=Random;
end;

// SetColor
//
procedure TGLColor.SetColor(red, green, blue : Single; alpha : Single = 1);
begin
   FColor[0]:=red;
   FColor[1]:=Green;
   FColor[2]:=blue;
   FColor[3]:=alpha;
   NotifyChange(Self);
end;

// GetHSVA
//
function TGLColor.GetHSVA : TVector;
var
   delta, min : Single;
const
   H = 0;
   S = 1;
   V = 2;
begin
   min:=MinFloat(PFloatVector(@FColor), 3);
   Result[V]:=MaxFloat(PFloatVector(@FColor), 3);
   delta:=Result[V]-min;

  // saturation is zero if R, G & B are zero
  // hue undefined (zero) if saturation is zero or color is gray (delta=zero)
   if (Result[V]=0) or (delta=0) then begin
      Result[S]:=0;
      Result[H]:=0;
   end else begin
      Result[S]:=delta/Result[V];
      if Red=Result[V] then
         // between yellow and magenta
         Result[H]:=60*(Green-Blue)/delta
      else if Green=Result[V] then
         // between cyan and yellow
         Result[H]:=120+60*(Blue-Red)/delta
      else // between magenta and cyan
         Result[H]:=240+60*(Red-Green)/delta;
      if Result[H]<0 then  // normalize H
         Result[H]:=Result[H]+360;
   end;
   Result[3]:=Alpha;
end;

// SetHSVA
//
procedure TGLColor.SetHSVA(const hsva : TVector);
var
   f, hTemp, p, q, t : Single;
const
   H = 0;
   S = 1;
   V = 2;
begin
   if hsva[S]=0 then begin
      // gray (ignore hue)
      FColor[0]:=hsva[V];
      FColor[1]:=hsva[V];
      FColor[2]:=hsva[V];
   end else begin
      hTemp:=hsva[H]*(1/60);
      f:=Frac(hTemp);

      p:=hsva[V]*(1-hsva[S]);
      q:=hsva[V]*(1-(hsva[S]*f));
      t:=hsva[V]*(1-(hsva[S]*(1-f)));

      case Trunc(hTemp) mod 6 of
         0 : begin
            FColor[0]:=hsva[V];
            FColor[1]:=t;
            FColor[2]:=p;
         end;
         1 : begin
            FColor[0]:=q;
            FColor[1]:=hsva[V];
            FColor[2]:=p;
         end;
         2 : begin
            FColor[0]:=p;
            FColor[1]:=hsva[V];
            FColor[2]:=t;
         end;
         3 : begin
            FColor[0]:=p;
            FColor[1]:=q;
            FColor[2]:=hsva[V];
         end;
         4 : begin
            FColor[0]:=t;
            FColor[1]:=p;
            FColor[2]:=hsva[V];
         end;
         5 : begin
            FColor[0]:=hsva[V];
            FColor[1]:=p;
            FColor[2]:=q;
         end;
      end
   end;
   FColor[3]:=hsva[3];
   NotifyChange(Self);
end;

// ------------------
// ------------------ TGLFaceProperties ------------------
// ------------------

// Create
//
constructor TGLFaceProperties.Create(aOwner : TPersistent);
begin
   inherited;
   // OpenGL default colors
   FAmbient:=TGLColor.CreateInitialized(Self, clrGray20);
   FDiffuse:=TGLColor.CreateInitialized(Self, clrGray80);
   FEmission:=TGLColor.Create(Self);
   FSpecular:=TGLColor.Create(Self);
   FShininess:=0;
end;

// Destroy
//
destructor TGLFaceProperties.Destroy;
begin
   FAmbient.Free;
   FDiffuse.Free;
   FEmission.Free;
   FSpecular.Free;
   inherited Destroy;
end;

// Apply
//
procedure TGLFaceProperties.Apply(var rci : TRenderContextInfo;
                                  aFace : TGLEnum);
const
   cPolygonMode : array [pmFill..pmPoints] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);
begin
   rci.GLStates.SetGLMaterialColors(aFace,
      Emission.FColor, Ambient.FColor, Diffuse.FColor, Specular.FColor, FShininess);
   rci.GLStates.SetGLPolygonMode(aFace, cPolygonMode[FPolygonMode]);
end;

// ApplyNoLighting
//
procedure TGLFaceProperties.ApplyNoLighting(var rci : TRenderContextInfo;
                                            aFace : TGLEnum);
const
   cPolygonMode : array [pmFill..pmPoints] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);
begin
   glColor4fv(@Diffuse.FColor);
   rci.GLStates.SetGLPolygonMode(aFace, cPolygonMode[FPolygonMode]);
end;

// Assign
//
procedure TGLFaceProperties.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLFaceProperties) then begin
      FAmbient.FColor:=TGLFaceProperties(Source).FAmbient.FColor;
      FDiffuse.FColor:=TGLFaceProperties(Source).FDiffuse.FColor;
      FSpecular.FColor:=TGLFaceProperties(Source).FSpecular.FColor;
      FShininess:=TGLFaceProperties(Source).FShininess;
      FPolygonMode:=TGLFaceProperties(Source).FPolygonMode;
		FEmission.FColor:=TGLFaceProperties(Source).FEmission.FColor;
		NotifyChange(Self);
   end;
end;

// SetAmbient
//
procedure TGLFaceProperties.SetAmbient(AValue: TGLColor);
begin
   FAmbient.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetDiffuse
//
procedure TGLFaceProperties.SetDiffuse(AValue: TGLColor);
begin
   FDiffuse.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetEmission
//
procedure TGLFaceProperties.SetEmission(AValue: TGLColor);
begin
   FEmission.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetSpecular
//
procedure TGLFaceProperties.SetSpecular(AValue: TGLColor);
begin
   FSpecular.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetPolygonMode
//
procedure TGLFaceProperties.SetPolygonMode(AValue: TPolygonMode);
begin
   if AValue<>FPolygonMode then begin
      FPolygonMode := AValue;
      NotifyChange(Self);
   end;
end;

// SetShininess
//
procedure TGLFaceProperties.SetShininess(AValue: TShininess);
begin
	if FShininess<>AValue then begin
		FShininess:=AValue;
		NotifyChange(Self);
	end;
end;

// ------------------
// ------------------ TGLTextureImage ------------------
// ------------------

// Create
//
constructor TGLTextureImage.Create(AOwner: TPersistent);
begin
	inherited;
	FOwnerTexture:=(AOwner as TGLTexture);
end;

// Destroy
//
destructor TGLTextureImage.Destroy;
begin
	inherited Destroy;
end;

// FriendlyDescription
//
class function TGLTextureImage.FriendlyDescription : String;
begin
	Result:=FriendlyName;
end;

// NativeTextureTarget
//
class function TGLTextureImage.NativeTextureTarget : TGLUInt;
begin
   Result:=GL_TEXTURE_2D;
end;

// Invalidate
//
procedure TGLTextureImage.Invalidate;
begin
	ReleaseBitmap32;
	Include(FOwnerTexture.FChanges, tcImage);
   NotifyChange(Self);
end;

// ReleaseBitmap32
//
procedure TGLTextureImage.ReleaseBitmap32;
begin
	// nothing here.
end;

// NotifyChange
//
procedure TGLTextureImage.NotifyChange;
begin
	Include(FOwnerTexture.FChanges, tcImage);
	FOwnerTexture.NotifyChange(Self);
end;

// Edit
//
function TGLTextureImage.Edit : Boolean;
begin
   Result:=EditGLTextureImage(Self);
end;

// LoadFromFile
//
procedure TGLTextureImage.LoadFromFile(const fileName : String);
var
   buf : String;
begin
   if Assigned(FOnTextureNeeded) then begin
      buf:=fileName;
      FOnTextureNeeded(Self, buf);
   end;
end;

// ------------------
// ------------------ TGLBlankImage ------------------
// ------------------

// Create
//
constructor TGLBlankImage.Create(AOwner: TPersistent);
begin
	inherited;
	FWidth:=256;
	FHeight:=256;
end;

// Destroy
//
destructor TGLBlankImage.Destroy;
begin
	ReleaseBitmap32;
	inherited Destroy;
end;

// Assign
//
procedure TGLBlankImage.Assign(Source: TPersistent);
begin
	if Assigned(Source) then begin
      if (Source is TGLBlankImage) then begin
         FWidth:=TGLBlankImage(Source).FWidth;
         FHeight:=TGLBlankImage(Source).FHeight;
         Invalidate;
      end else inherited;
	end else inherited;
end;

// SetWidth
//
procedure TGLBlankImage.SetWidth(val : Integer);
begin
   if val<>FWidth then begin
      FWidth:=val;
      if FWidth<1 then FWidth:=1;
      Invalidate;
   end;
end;

// GetWidth
//
function TGLBlankImage.GetWidth: Integer;
begin
	Result:=FWidth;
end;

// SetHeight
//
procedure TGLBlankImage.SetHeight(val : Integer);
begin
   if val<>FHeight then begin
      FHeight:=val;
      if FHeight<1 then FHeight:=1;
      Invalidate;
   end;
end;

// GetHeight
//
function TGLBlankImage.GetHeight: Integer;
begin
	Result:=FHeight;
end;

// GetBitmap32
//
function TGLBlankImage.GetBitmap32(target : TGLUInt) : TGLBitmap32;
begin
	if not Assigned(FBitmap) then begin
      FBitmap:=TGLBitmap32.Create;
      FBitmap.Width:=FWidth;
      FBitmap.Height:=FHeight;
	end;
	Result:=FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLBlankImage.ReleaseBitmap32;
begin
	if Assigned(FBitmap) then begin
   	FBitmap.Free;
		FBitmap:=nil;
	end;
end;

// SaveToFile
//
procedure TGLBlankImage.SaveToFile(const fileName : String);
begin
   SaveStringToFile(fileName, '[BlankImage]'#13#10'Width='+IntToStr(Width)
                              +#13#10'Height='+IntToStr(Height));
end;

// LoadFromFile
//
procedure TGLBlankImage.LoadFromFile(const fileName : String);
var
   sl : TStringList;
   buf : String;
begin
   buf:=fileName;
   if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(Self, buf);
   if FileExists(buf) then begin
      sl:=TStringList.Create;
      try
         sl.LoadFromFile(buf);
         FWidth:=StrToInt(sl.Values['Width']);
         FHeight:=StrToInt(sl.Values['Height']);
      finally
         sl.Free;
      end;
   end else begin
      Assert(False, Format(glsFailedOpenFile, [fileName]));
   end;
end;

// FriendlyName
//
class function TGLBlankImage.FriendlyName : String;
begin
   Result:='Blank Image';
end;

// FriendlyDescription
//
class function TGLBlankImage.FriendlyDescription : String;
begin
   Result:='Blank Image (Width x Height)';
end;

// ------------------
// ------------------ TGLPictureImage ------------------
// ------------------

// Create
//
constructor TGLPictureImage.Create(AOwner: TPersistent);
begin
	inherited;
end;

// Destroy
//
destructor TGLPictureImage.Destroy;
begin
	ReleaseBitmap32;
	FGLPicture.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLPictureImage.Assign(Source: TPersistent);
var
   bmp : TGLBitmap;
begin
	if Assigned(Source) then begin
      if (Source is TGLPersistentImage) then
   		Picture.Assign(TGLPersistentImage(Source).Picture)
      else if (Source is TGLGraphic) then
         Picture.Assign(Source)
      else if (Source is TGLPicture) then
         Picture.Assign(Source)
      else if (Source is TGLBitmap32) then begin
         bmp:=TGLBitmap32(Source).Create32BitsBitmap;
         Picture.Graphic:=bmp;
         bmp.Free;
      end else inherited;
	end else inherited;
end;

// BeginUpdate
//
procedure TGLPictureImage.BeginUpdate;
begin
	Inc(FUpdateCounter);
	Picture.OnChange:=nil;
end;

// EndUpdate
//
procedure TGLPictureImage.EndUpdate;
begin
	Assert(FUpdateCounter>0, ClassName+': Unbalanced Begin/EndUpdate');
	Dec(FUpdateCounter);
	Picture.OnChange:=PictureChanged;
	if FUpdateCounter=0 then
		PictureChanged(Picture);
end;

// GetHeight
//
function TGLPictureImage.GetHeight: Integer;
begin
	Result:=Picture.Height;
end;

// GetWidth
//
function TGLPictureImage.GetWidth: Integer;
begin
	Result:=Picture.Width;
end;

// GetBitmap32
//
function TGLPictureImage.GetBitmap32(target : TGLUInt) : TGLBitmap32;
begin
	if not Assigned(FBitmap) then begin
      FBitmap:=TGLBitmap32.Create;
      // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
      // for instance, TJPegImage triggers an OnChange when it is drawn...
      if Assigned(Picture.OnChange) then begin
         Picture.OnChange:=nil;
         try
            FBitmap.Assign(Picture.Graphic);
         finally
            Picture.OnChange:=PictureChanged;
         end;
      end else FBitmap.Assign(Picture.Graphic);
	end;
	Result:=FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLPictureImage.ReleaseBitmap32;
begin
	if Assigned(FBitmap) then begin
   	FBitmap.Free;
		FBitmap:=nil;
	end;
end;

// PictureChanged
//
procedure TGLPictureImage.PictureChanged(Sender: TObject);
begin
 	Invalidate;
end;

// GetPicture
//
function TGLPictureImage.GetPicture : TGLPicture;
begin
   if not Assigned(FGLPicture) then begin
      FGLPicture:=TGLPicture.Create;
      FGLPicture.OnChange:=PictureChanged;
   end;
   Result:=FGLPicture;
end;

// SetPicture
//
procedure TGLPictureImage.SetPicture(const aPicture : TGLPicture);
begin
	Picture.Assign(aPicture);
end;

// ------------------
// ------------------ TGLPersistentImage ------------------
// ------------------

// Create
//
constructor TGLPersistentImage.Create(AOwner: TPersistent);
begin
	inherited;
end;

// Destroy
//
destructor TGLPersistentImage.Destroy;
begin
	inherited Destroy;
end;

// SaveToFile
//
procedure TGLPersistentImage.SaveToFile(const fileName : String);
begin
	Picture.SaveToFile(fileName);
end;

// LoadFromFile
//
procedure TGLPersistentImage.LoadFromFile(const fileName : String);
var
   buf : String;
   gr : TGLGraphic;
begin
   buf:=fileName;
   if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(Self, buf);
   if ApplicationFileIODefined then begin
      gr:=CreateGraphicFromFile(buf);
      if Assigned(gr) then begin
         Picture.Graphic:=gr;
         gr.Free;
         Exit;
      end;
   end else if FileExists(buf) then begin
      Picture.LoadFromFile(buf);
      Exit;
   end;
   Picture.Graphic:=nil;
   raise ETexture.CreateFmt(glsFailedOpenFile, [fileName]);
end;

// FriendlyName
//
class function TGLPersistentImage.FriendlyName : String;
begin
	Result:='Persistent Image';
end;

// FriendlyDescription
//
class function TGLPersistentImage.FriendlyDescription : String;
begin
	Result:='Image data is stored in its original format with other form resources,'
			 +'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

// ------------------
// ------------------ TGLPicFileImage ------------------
// ------------------

// Create
//
constructor TGLPicFileImage.Create(AOwner: TPersistent);
begin
	inherited;
end;

// Destroy
//
destructor TGLPicFileImage.Destroy;
begin
	inherited;
end;

// Assign
//
procedure TGLPicFileImage.Assign(Source: TPersistent);
begin
   if Source is TGLPicFileImage then begin
      FPictureFileName:=TGLPicFileImage(Source).FPictureFileName
  	end else inherited;
end;

// SetPictureFileName
//
procedure TGLPicFileImage.SetPictureFileName(const val : String);
begin
	if val<>FPictureFileName then begin
		FPictureFileName:=val;
      FAlreadyWarnedAboutMissingFile:=False;
		Invalidate;
	end;
end;

// Invalidate
//
procedure TGLPicFileImage.Invalidate;
begin
	Picture.OnChange:=nil;
	try
		Picture.Assign(nil);
		FBitmap:=nil;
	finally
		Picture.OnChange:=PictureChanged;
	end;
	inherited;
end;

// GetBitmap32
//
function TGLPicFileImage.GetBitmap32(target : TGLUInt) : TGLBitmap32;
var
   buf : String;
   gr : TGLGraphic;
begin
	if (GetWidth<=0) and (PictureFileName<>'') then begin
		Picture.OnChange:=nil;
		try
         buf:=PictureFileName;
         if Assigned(FOnTextureNeeded) then
            FOnTextureNeeded(Self, buf);
         if FileStreamExists(buf) then begin
            gr:=CreateGraphicFromFile(buf);
            Picture.Graphic:=gr;
            gr.Free;
         end else begin
            Picture.Graphic:=nil;
            if not FAlreadyWarnedAboutMissingFile then begin
               FAlreadyWarnedAboutMissingFile:=True;
               Assert(False, Format(glsFailedOpenFile, [PictureFileName]));
            end;
         end;
         Result:=inherited GetBitmap32(target);
         Picture.Graphic:=nil;
		finally
			Picture.OnChange:=PictureChanged;
		end;
	end else	Result:=inherited GetBitmap32(target);
end;

// SaveToFile
//
procedure TGLPicFileImage.SaveToFile(const fileName : String);
begin
	SaveStringToFile(fileName, PictureFileName);
end;

// LoadFromFile
//
procedure TGLPicFileImage.LoadFromFile(const fileName : String);
var
   buf : String;
begin
   inherited;
   // attempt to autodetect if we are pointed to a file containing
   // a filename or directly to an image
   if SizeOfFile(fileName)<512 then begin
   	buf:=LoadStringFromFile(fileName);
      if Pos(#0, buf)>0 then
         PictureFileName:=fileName
      else PictureFileName:=buf;
   end else PictureFileName:=fileName;
end;

// FriendlyName
//
class function TGLPicFileImage.FriendlyName : String;
begin
	Result:='PicFile Image';
end;

// FriendlyDescription
//
class function TGLPicFileImage.FriendlyDescription : String;
begin
	Result:='Image data is retrieved from a file.';
end;

// ------------------
// ------------------ TGLCubeMapImage ------------------
// ------------------

// Create
//
constructor TGLCubeMapImage.Create(AOwner: TPersistent);
var
   i : TGLCubeMapTarget;
begin
	inherited;
   for i:=Low(FPicture) to High(FPicture) do begin
      FPicture[i]:=TGLPicture.Create;
      FPicture[i].OnChange:=PictureChanged;
   end;
end;

// Destroy
//
destructor TGLCubeMapImage.Destroy;
var
   i : TGLCubeMapTarget;
begin
	ReleaseBitmap32;
   for i:=Low(FPicture) to High(FPicture) do
      FPicture[i].Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLCubeMapImage.Assign(Source: TPersistent);
var
   i : TGLCubeMapTarget;
begin
	if Assigned(Source) then begin
      if (Source is TGLCubeMapImage) then begin
         for i:=Low(FPicture) to High(FPicture) do
            FPicture[i].Assign(TGLCubeMapImage(Source).FPicture[i]);
         Invalidate;
      end else inherited;
	end else inherited;
end;

// GetWidth
//
function TGLCubeMapImage.GetWidth: Integer;
begin
	Result:=FPicture[cmtPX].Width;
end;

// GetHeight
//
function TGLCubeMapImage.GetHeight: Integer;
begin
	Result:=FPicture[cmtPX].Height;
end;

// GetBitmap32
//
function TGLCubeMapImage.GetBitmap32(target : TGLUInt) : TGLBitmap32;
var
   i : TGLCubeMapTarget;
begin
   i:=cmtPX;
   case target of
      GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB : i:=cmtPX;
      GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB : i:=cmtNX;
      GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB : i:=cmtPY;
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB : i:=cmtNY;
      GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB : i:=cmtPZ;
      GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB : i:=cmtNZ;
   end;
   if Assigned(FBitmap) then
      FBitmap.Free;
   FBitmap:=TGLBitmap32.Create;
   FPicture[i].OnChange:=nil;
   try
      FBitmap.VerticalReverseOnAssignFromBitmap:=True;
      FBitmap.Assign(FPicture[i].Graphic);
   finally
      FPicture[i].OnChange:=PictureChanged;
   end;
	Result:=FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLCubeMapImage.ReleaseBitmap32;
begin
	if Assigned(FBitmap) then begin
   	FBitmap.Free;
		FBitmap:=nil;
	end;
end;

// BeginUpdate
//
procedure TGLCubeMapImage.BeginUpdate;
var
   i : TGLCubeMapTarget;
begin
	Inc(FUpdateCounter);
   for i:=Low(FPicture) to High(FPicture) do
      FPicture[i].OnChange:=nil;
end;

// EndUpdate
//
procedure TGLCubeMapImage.EndUpdate;
var
   i : TGLCubeMapTarget;
begin
	Assert(FUpdateCounter>0, ClassName+': Unbalanced Begin/EndUpdate');
	Dec(FUpdateCounter);
   for i:=Low(FPicture) to High(FPicture) do
      FPicture[i].OnChange:=PictureChanged;
	if FUpdateCounter=0 then
		PictureChanged(FPicture[cmtPX]);
end;

// SaveToFile
//
procedure TGLCubeMapImage.SaveToFile(const fileName : String);
var
   fs : TFileStream;
   bmp : TGLBitmap;
   i : TGLCubeMapTarget;
   version : Word;
begin
   fs:=TFileStream.Create(fileName, fmCreate);
   bmp:=TGLBitmap.Create;
   try
      version:=$0100;
      fs.Write(version, 2);
      for i:=Low(FPicture) to High(FPicture) do begin
         bmp.Assign(FPicture[i].Graphic);
         bmp.SaveToStream(fs);
      end;
   finally
      bmp.Free;
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TGLCubeMapImage.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
   bmp : TGLBitmap;
   i : TGLCubeMapTarget;
   version : Word;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
   bmp:=TGLBitmap.Create;
   try
      fs.Read(version, 2);
      Assert(version=$0100);
      for i:=Low(FPicture) to High(FPicture) do begin
         bmp.LoadFromStream(fs);
         FPicture[i].Graphic:=bmp;
      end;
   finally
      bmp.Free;
      fs.Free;
   end;
end;

// FriendlyName
//
class function TGLCubeMapImage.FriendlyName : String;
begin
   Result:='CubeMap Image';
end;

// NativeTextureTarget
//
class function TGLCubeMapImage.NativeTextureTarget : TGLUInt;
begin
   Result:=GL_TEXTURE_CUBE_MAP_ARB;
end;

// PictureChanged
//
procedure TGLCubeMapImage.PictureChanged(Sender: TObject);
begin
	Invalidate;
end;

// SetPicture
//
procedure TGLCubeMapImage.SetPicture(index : TGLCubeMapTarget; const val : TGLPicture);
begin
   FPicture[index].Assign(val);
end;

// GetPicture
//
function TGLCubeMapImage.GetPicture(index : TGLCubeMapTarget) : TGLPicture;
begin
   Result:=FPicture[index];
end;

// ------------------
// ------------------ TGLShader ------------------
// ------------------

// Create
//
constructor TGLShader.Create(AOwner : TComponent);
begin
   FLibMatUsers:=TList.Create;
   FVirtualHandle:=TGLVirtualHandle.Create;
   FShaderStyle:=ssLowLevel;
   FEnabled:=True;
	inherited;
end;

// Destroy
//
destructor TGLShader.Destroy;
var
   i : Integer;
   list : TList;
begin
   FVirtualHandle.DestroyHandle;
   FinalizeShader;
	inherited;
   list:=FLibMatUsers;
   FLibMatUsers:=nil;
   for i:=list.Count-1 downto 0 do
      TGLLibMaterial(list[i]).Shader:=nil;
   list.Free;
   FVirtualHandle.Free;
end;

// NotifyChange
//
procedure TGLShader.NotifyChange(Sender : TObject);
var
   i : Integer;
begin
   if FUpdateCount=0 then begin
      for i:=FLibMatUsers.Count-1 downto 0 do
         TGLLibMaterial(FLibMatUsers[i]).NotifyUsers;
      FinalizeShader;
   end;
end;

// BeginUpdate
//
procedure TGLShader.BeginUpdate;
begin
   Inc(FUpdateCount);
end;

// EndUpdate
//
procedure TGLShader.EndUpdate;
begin
   Dec(FUpdateCount);
   if FUpdateCount=0 then
      NotifyChange(Self);
end;

// DoInitialize
//
procedure TGLShader.DoInitialize;
begin
   // nothing here
end;

// DoFinalize
//
procedure TGLShader.DoFinalize;
begin
   // nothing here
end;

// GetShaderInitialized
//
function TGLShader.GetShaderInitialized : Boolean;
begin
   Result:=(FVirtualHandle.Handle<>0);
end;

// InitializeShader
//
procedure TGLShader.InitializeShader;
begin
   if FVirtualHandle.Handle=0 then begin
      FVirtualHandle.OnAllocate:=OnVirtualHandleAllocate;
      FVirtualHandle.OnDestroy:=OnVirtualHandleDestroy;
      FVirtualHandle.AllocateHandle;
      DoInitialize;
      FShaderInitialized:=True;
   end;
end;

// FinalizeShader
//
procedure TGLShader.FinalizeShader;
var
   activateContext : Boolean;
begin
   if FVirtualHandle.Handle<>0 then begin
      if FShaderInitialized then begin
         activateContext:=(not FVirtualHandle.RenderingContext.Active);
         if activateContext then
            FVirtualHandle.RenderingContext.Activate;
         try
            FShaderInitialized:=False;
            DoFinalize;
         finally
            if activateContext then
               FVirtualHandle.RenderingContext.Deactivate;
         end;
         FVirtualHandle.DestroyHandle;
      end;
   end;
end;

// Apply
//
procedure TGLShader.Apply(var rci : TRenderContextInfo; Sender : TObject);
begin
   //Assert(not FShaderActive, 'Unbalanced shader application.');
   if Enabled then begin
      if FVirtualHandle.Handle=0 then
         InitializeShader;
      DoApply(rci, Sender);
   end;
   FShaderActive:=True;
end;

// UnApply
//
function TGLShader.UnApply(var rci : TRenderContextInfo) : Boolean;
begin
   //Assert(FShaderActive, 'Unbalanced shader application.');
   if Enabled then begin
      Result:=DoUnApply(rci);
      if not Result then
         FShaderActive:=False;
   end else begin
      FShaderActive:=False;
      Result:=False;
   end;
end;

// OnVirtualHandleDestroy
//
procedure TGLShader.OnVirtualHandleDestroy(sender : TGLVirtualHandle; var handle : Integer);
begin
   FinalizeShader;
   handle:=0;
end;

// OnVirtualHandleAllocate
//
procedure TGLShader.OnVirtualHandleAllocate(sender : TGLVirtualHandle; var handle : Integer);
begin
   handle:=1;
end;

// SetEnabled
//
procedure TGLShader.SetEnabled(val : Boolean);
begin
   Assert(not FShaderActive, 'Shader is active.');
   if val<>FEnabled then begin
      FEnabled:=val;
      NotifyChange(Self);
   end;
end;

// RegisterUser
//
procedure TGLShader.RegisterUser(libMat : TGLLibMaterial);
var
   i : Integer;
begin
   i:=FLibMatUsers.IndexOf(libMat);
   if i<0 then
      FLibMatUsers.Add(libMat);
end;

// UnRegisterUser
//
procedure TGLShader.UnRegisterUser(libMat : TGLLibMaterial);
begin
   if Assigned(FLibMatUsers) then
      FLibMatUsers.Remove(libMat);
end;

// ------------------
// ------------------ TGLTexture ------------------
// ------------------

// Create
//
constructor TGLTexture.Create(AOwner: TPersistent);
begin
	inherited;
	FDisabled:=True;
	FChanges:=[tcImage, tcParams];
	FImage:=TGLPersistentImage.Create(Self);
   FImage.FOnTextureNeeded:=DoOnTextureNeeded;
	FImageAlpha:=tiaDefault;
   FImageBrightness:=1.0;
   FImageGamma:=1.0;
	FMagFilter:=maLinear;
	FMinFilter:=miLinearMipMapLinear;
   FFilteringQuality:=tfIsotropic;
   FRequiredMemorySize:=-1;
   FTextureHandle:=TGLTextureHandle.Create;
   FMappingMode:=tmmUser;
   FEnvColor:=TGLColor.CreateInitialized(Self, clrTransparent);
   FNormalMapScale:=cDefaultNormalMapScale;
end;

// Destroy
//
destructor TGLTexture.Destroy;
begin
   FEnvColor.Free;
   FMapSCoordinates.Free;
   FMapTCoordinates.Free;
	DestroyHandles;
   FTextureHandle.Free;
	FImage.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLTexture.Assign(Source: TPersistent);
begin
   if Assigned(Source) then begin
      if (Source is TGLTexture) then begin
		   if Source<>Self then begin
			   FImageAlpha:=TGLTexture(Source).FImageAlpha;
   			FTextureMode:=TGLTexture(Source).FTextureMode;
	   		FTextureWrap:=TGLTexture(Source).FTextureWrap;
      		FTextureFormat:=TGLTexture(Source).FTextureFormat;
      		FCompression:=TGLTexture(Source).FCompression;
		   	FMinFilter:=TGLTexture(Source).FMinFilter;
			   FMagFilter:=TGLTexture(Source).FMagFilter;
            FMappingMode:=TGLTexture(Source).FMappingMode;
            MappingSCoordinates.Assign(TGLTexture(Source).MappingSCoordinates);
            MappingTCoordinates.Assign(TGLTexture(Source).MappingTCoordinates);
   			FDisabled:=TGLTexture(Source).FDisabled;
	   		SetImage(TGLTexture(Source).FImage);
		   	FChanges:=[tcParams, tcImage];
   		end;
      end else if (Source is TGLGraphic) then begin
         Image.Assign(Source);
      end else if (Source is TGLPicture) then begin
         Image.Assign(TGLPicture(Source).Graphic);
      end else inherited Assign(Source);
   end else begin
      FDisabled:=True;
  		SetImage(nil);
     	FChanges:=[tcParams, tcImage];
   end;
end;

// NotifyChange
//
procedure TGLTexture.NotifyChange(Sender : TObject);
begin
  if Assigned(Owner) then begin
    if Owner is TGLTextureExItem then
      TGLTextureExItem(Owner).NotifyChange(Sender);
  end;

  inherited;
end;

// NotifyImageChange
//
procedure TGLTexture.NotifyImageChange;
begin
   Include(FChanges, tcImage);
   NotifyChange(Self);
end;

// NotifyParamsChange
//
procedure TGLTexture.NotifyParamsChange;
begin
   Include(FChanges, tcParams);
   NotifyChange(Self);
end;

// SetImage
//
procedure TGLTexture.SetImage(AValue: TGLTextureImage);
begin
   if Assigned(aValue) then begin
   	if FImage.ClassType<>AValue.ClassType then begin
	   	FImage.Free;
		   FImage:=TGLTextureImageClass(AValue.ClassType).Create(Self);
         FImage.OnTextureNeeded:=DoOnTextureNeeded;
	   end;
   	FImage.Assign(AValue);
   end else begin
      FImage.Free;
   	FImage:=TGLPersistentImage.Create(Self);
      FImage.FOnTextureNeeded:=DoOnTextureNeeded;
   end;
end;

// SetImageClassName
//
procedure TGLTexture.SetImageClassName(const val : String);
begin
	if val<>'' then if FImage.ClassName<>val then begin
		FImage.Free;
		FImage:=TGLTextureImageClass(FindGLTextureImageClass(val)).Create(Self);
      FImage.OnTextureNeeded:=DoOnTextureNeeded;
      NotifyImageChange;
	end;
end;

// GetImageClassName
//
function TGLTexture.GetImageClassName : String;
begin
	Result:=FImage.ClassName;
end;

// TextureImageRequiredMemory
//
function TGLTexture.TextureImageRequiredMemory : Integer;
const
   cTextureFormatToPixelSize : array [tfRGB..high(TGLTextureFormat)] of Integer = ( // float_type
                                                     3, 4, 2, 2, 1, 8, 16, 1, 2, 1, 3);
var
   tf : TGLTextureFormat;
begin
   if FRequiredMemorySize>0 then
      Result:=FRequiredMemorySize
   else begin
      if TextureFormat=tfDefault then
         if vDefaultTextureFormat=tfDefault then
            tf:=tfRGBA
         else tf:=vDefaultTextureFormat
      else tf:=TextureFormat;
      Result:=cTextureFormatToPixelSize[tf]*Image.Width*Image.Height;
   end;
end;

// SetImageAlpha
//
procedure TGLTexture.SetImageAlpha(const val : TGLTextureImageAlpha);
begin
   if FImageAlpha<>val then begin
   	FImageAlpha:=val;
      NotifyImageChange;
   end;
end;

// SetImageBrightness
//
procedure TGLTexture.SetImageBrightness(const val : Single);
begin
   if FImageBrightness<>val then begin
   	FImageBrightness:=val;
      NotifyImageChange;
   end;
end;

// StoreBrightness
//
function TGLTexture.StoreBrightness : Boolean;
begin
	Result:=(FImageBrightness<>1.0);
end;

// SetImageGamma
//
procedure TGLTexture.SetImageGamma(const val : Single);
begin
   if FImageGamma<>val then begin
   	FImageGamma:=val;
      NotifyImageChange;
   end;
end;

// StoreGamma
//
function TGLTexture.StoreGamma : Boolean;
begin
	Result:=(FImageGamma<>1.0);
end;

// SetMagFilter
//
procedure TGLTexture.SetMagFilter(AValue: TGLMagFilter);
begin
	if AValue <> FMagFilter then begin
		FMagFilter:=AValue;
      NotifyParamsChange;
	end;
end;

// SetMinFilter
//
procedure TGLTexture.SetMinFilter(AValue: TGLMinFilter);
begin
	if AValue <> FMinFilter then begin
		FMinFilter:=AValue;
      NotifyParamsChange;
	end;
end;

// SetTextureMode
//
procedure TGLTexture.SetTextureMode(AValue: TGLTextureMode);
begin
	if AValue <> FTextureMode then begin
		FTextureMode:=AValue;
      NotifyParamsChange;
	end;
end;

// SetDisabled
//
procedure TGLTexture.SetDisabled(AValue: Boolean);
begin
	if AValue <> FDisabled then begin
   	FDisabled:=AValue;
    if Assigned(Owner) or ((Owner is TGLMaterial) or (Owner is TGLTextureExItem)) then begin
       if Owner is TGLMaterial then
          TGLMaterial(Owner).NotifyTexMapChange(Self);
       if Owner is TGLTextureExItem then
          TGLTextureExItem(Owner).NotifyTexMapChange(Self);
    end else NotifyChange(Self);
	end;
end;

// SetEnabled
//
procedure TGLTexture.SetEnabled(const val : Boolean);
begin
   Disabled:=not val;
end;

// GetEnabled
//
function TGLTexture.GetEnabled : Boolean;
begin
   Result:=not Disabled;
end;

// SetEnvColor
//
procedure TGLTexture.SetEnvColor(const val : TGLColor);
begin
   FEnvColor.Assign(val);
   NotifyParamsChange;
end;

// SetNormalMapScale
//
procedure TGLTexture.SetNormalMapScale(const val : Single);
begin
   if val<>FNormalMapScale then begin
      FNormalMapScale:=val;
      if TextureFormat=tfNormalMap then
   		NotifyImageChange;
   end;
end;

// StoreNormalMapScale
//
function TGLTexture.StoreNormalMapScale : Boolean;
begin
   Result:=(FNormalMapScale<>cDefaultNormalMapScale);
end;

// SetTextureWrap
//
procedure TGLTexture.SetTextureWrap(AValue: TGLTextureWrap);
begin
	if AValue <> FTextureWrap then begin
		FTextureWrap:=AValue;
      NotifyParamsChange;
	end;
end;

// SetTextureFormat
//
procedure TGLTexture.SetTextureFormat(const val : TGLTextureFormat);
begin
	if val <> FTextureFormat then begin
		FTextureFormat:=val;
      NotifyParamsChange;
	end;
end;

// SetCompression
//
procedure TGLTexture.SetCompression(const val : TGLTextureCompression);
begin
	if val <> FCompression then begin
		FCompression:=val;
      NotifyParamsChange;
	end;
end;

// SetFilteringQuality
//
procedure TGLTexture.SetFilteringQuality(const val : TGLTextureFilteringQuality);
begin
	if val<>FFilteringQuality then begin
		FFilteringQuality:=val;
      NotifyParamsChange;
	end;
end;

// SetMappingMode
//
procedure TGLTexture.SetMappingMode(const val : TGLTextureMappingMode);
var
   texMapChange : Boolean;
begin
   if val<>FMappingMode then begin
      texMapChange:=((val=tmmUser) and (FMappingMode<>tmmUser))
                    or ((val=tmmUser) and (FMappingMode<>tmmUser));
      FMappingMode:=val;
      if texMapChange then begin
         // when switching between texGen modes and user mode, the geometry
         // must be rebuilt in whole (to specify/remove texCoord data!)
         if Assigned(Owner) and (Owner is TGLMaterial) then
            TGLMaterial(Owner).NotifyTexMapChange(Self);
      end else	NotifyChange(Self);
   end;
end;

// SetMappingSCoordinates
//
procedure TGLTexture.SetMappingSCoordinates(const val : TGLCoordinates4);
begin
   MappingSCoordinates.Assign(val);
end;

// GetMappingSCoordinates
//
function TGLTexture.GetMappingSCoordinates : TGLCoordinates4;
begin
   if not Assigned(FMapSCoordinates) then
      FMapSCoordinates:=TGLCoordinates4.CreateInitialized(Self, XHmgVector, csVector);
   Result:=FMapSCoordinates;
end;

// SetMappingTCoordinates
//
procedure TGLTexture.SetMappingTCoordinates(const val : TGLCoordinates4);
begin
   MappingTCoordinates.Assign(val);
end;

// GetMappingTCoordinates
//
function TGLTexture.GetMappingTCoordinates : TGLCoordinates4;
begin
   if not Assigned(FMapTCoordinates) then
      FMapTCoordinates:=TGLCoordinates4.CreateInitialized(Self, XHmgVector, csVector);
   Result:=FMapTCoordinates;
end;

// StoreImageClassName
//
function TGLTexture.StoreImageClassName : Boolean;
begin
   Result:=(FImage.ClassName<>TGLPersistentImage.ClassName);
end;

// PrepareBuildList
//
procedure TGLTexture.PrepareBuildList;
begin
   GetHandle;
end;

// ApplyMappingMode
//
procedure TGLTexture.ApplyMappingMode;
begin
   case MappingMode of
      tmmUser : ; // nothing to do, but checked first (common case)
      tmmObjectLinear : begin
         glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
         glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
         glTexGenfv(GL_S, GL_OBJECT_PLANE, @MappingSCoordinates.DirectVector);
         glTexGenfv(GL_T, GL_OBJECT_PLANE, @MappingTCoordinates.DirectVector);
         glEnable(GL_TEXTURE_GEN_S);
         glEnable(GL_TEXTURE_GEN_T);
      end;
      tmmEyeLinear : begin
         glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
         glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
         glTexGenfv(GL_S, GL_EYE_PLANE, @MappingSCoordinates.DirectVector);
         glTexGenfv(GL_T, GL_EYE_PLANE, @MappingTCoordinates.DirectVector);
         glEnable(GL_TEXTURE_GEN_S);
         glEnable(GL_TEXTURE_GEN_T);
      end;
      tmmSphere : begin
         glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
         glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
         glEnable(GL_TEXTURE_GEN_S);
         glEnable(GL_TEXTURE_GEN_T);
      end;
      tmmCubeMapReflection, tmmCubeMapCamera : if GL_ARB_texture_cube_map then begin
         glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
         glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
         glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
         glEnable(GL_TEXTURE_GEN_S);
         glEnable(GL_TEXTURE_GEN_T);
  	      glEnable(GL_TEXTURE_GEN_R);
      end;
      tmmCubeMapNormal, tmmCubeMapLight0 : if GL_ARB_texture_cube_map then begin
         glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
         glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
         glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
         glEnable(GL_TEXTURE_GEN_S);
         glEnable(GL_TEXTURE_GEN_T);
  	      glEnable(GL_TEXTURE_GEN_R);
      end;
   else
      Assert(False);
   end;
end;

// ApplyMappingMode
//
procedure TGLTexture.UnApplyMappingMode;
begin
   if MappingMode<>tmmUser then begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
      glDisable(GL_TEXTURE_GEN_R);
   end;
end;

// Apply
//
procedure TGLTexture.Apply(var rci : TRenderContextInfo);
var
   m : TMatrix;
begin
	if not Disabled then begin
      if Image.NativeTextureTarget=GL_TEXTURE_2D then begin
   		 rci.GLStates.SetGLState(stTexture2D);
   	   rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_2D, Handle);
      end
      else
        // NV float needs GL_TEXTURE_RECTANGLE_NV; doesn't affect ATI_float
        if Image.NativeTextureTarget=GL_TEXTURE_RECTANGLE_NV then begin
   		   rci.GLStates.SetGLState(stTextureRect);
     	   rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_RECTANGLE_NV, Handle);
        end
      else
      if GL_ARB_texture_cube_map then begin
         rci.GLStates.SetGLState(stTextureCubeMap);
   	     rci.GLStates.SetGLCurrentTexture(0, GL_TEXTURE_CUBE_MAP_ARB, Handle);
         // compute model view matrix for proper viewing
         glMatrixMode(GL_TEXTURE);
         case MappingMode of
            tmmCubeMapReflection, tmmCubeMapNormal : begin
               m:=rci.modelViewMatrix^;
               NormalizeMatrix(m);
               // Transposition = Matrix inversion (matrix is now orthonormal)
               if GL_ARB_transpose_matrix then
                  glLoadTransposeMatrixfARB(@m)
               else begin
                  TransposeMatrix(m);
                  glLoadMatrixf(@m);
               end;
            end;
            tmmCubeMapLight0 : begin
               with TGLScene(rci.scene).Lights do if Count>0 then begin
                  m:=TGLLightSource(Items[0]).AbsoluteMatrix;
                  NormalizeMatrix(m);
                  if GL_ARB_transpose_matrix then
                     glLoadTransposeMatrixfARB(@m)
                  else begin
                     TransposeMatrix(m);
                     glLoadMatrixf(@m);
                  end;

                  m:=rci.modelViewMatrix^;
                  NormalizeMatrix(m);
                  TransposeMatrix(m);
                  glMultMatrixf(@m);
               end;
            end;
            tmmCubeMapCamera : begin
               m[0]:=VectorCrossProduct(rci.cameraUp, rci.cameraDirection);
               m[1]:=VectorNegate(rci.cameraDirection);
               m[2]:=rci.cameraUp;
               m[3]:=WHmgPoint;
               if GL_ARB_transpose_matrix then
                  glLoadTransposeMatrixfARB(@m)
               else begin
                  TransposeMatrix(m);
                  glLoadMatrixf(@m);
               end;

               m:=rci.modelViewMatrix^;
               NormalizeMatrix(m);
               TransposeMatrix(m);
               glMultMatrixf(@m);
            end;
         end;

         glMatrixMode(GL_MODELVIEW);
      end; // if GL_ARB_texture_cube_map
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
    	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
      ApplyMappingMode;
      xglMapTexCoordToMain;
	end else begin //if disabled
      rci.GLStates.UnSetGLState(stTexture2D);
      rci.GLStates.UnSetGLState(stTextureRECT);
      xglMapTexCoordToMain;
   end;
end;

// UnApply
//
procedure TGLTexture.UnApply(var rci : TRenderContextInfo);
begin
   if not Disabled then begin
      if stTextureCubeMap in rci.GLStates.States then begin
         rci.GLStates.UnSetGLState(stTextureCubeMap);
         glMatrixMode(GL_TEXTURE);
         glLoadIdentity;
         glMatrixMode(GL_MODELVIEW);
      end else
      if stTexture2D in rci.GLStates.States then  
        rci.GLStates.UnSetGLState(stTexture2D)
      else
        rci.GLStates.UnSetGLState(stTextureRECT);

      UnApplyMappingMode;
   end;
end;

// ApplyAsTexture2
//
procedure TGLTexture.ApplyAsTexture2(var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
begin
   ApplyAsTextureN(2, rci, libMaterial);
end;

// UnApplyAsTexture2
//
procedure TGLTexture.UnApplyAsTexture2(var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
begin
   UnApplyAsTextureN(2, rci, libMaterial);
end;

// ApplyAsTextureN
//
procedure TGLTexture.ApplyAsTextureN(n : Integer; var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
var
   m : TMatrix;
begin
   if not Disabled then begin
      glActiveTextureARB(GL_TEXTURE0_ARB+n-1);

      if Image.NativeTextureTarget=GL_TEXTURE_2D then begin
         glEnable(Image.NativeTextureTarget);
         rci.GLStates.SetGLCurrentTexture(n-1, Image.NativeTextureTarget, Handle);

         if Assigned(libMaterial) and (not libMaterial.FTextureMatrixIsIdentity) then begin
            glMatrixMode(GL_TEXTURE);
            glLoadMatrixf(PGLFloat(@libMaterial.FTextureMatrix[0][0]));
            glMatrixMode(GL_MODELVIEW);
         end;

      end else if GL_ARB_texture_cube_map then begin
         glEnable(Image.NativeTextureTarget);
         rci.GLStates.SetGLCurrentTexture(n-1, Image.NativeTextureTarget, Handle);

         // compute model view matrix for proper viewing
         glMatrixMode(GL_TEXTURE);
         m:=rci.modelViewMatrix^;
         NormalizeMatrix(m);
         // Transposition = Matrix inversion (matrix is now orthonormal)
         if GL_ARB_transpose_matrix then
            glLoadTransposeMatrixfARB(@m)
         else begin
            TransposeMatrix(m);
            glLoadMatrixf(@m);
         end;
         glMatrixMode(GL_MODELVIEW);
      end;

      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
    	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, FEnvColor.AsAddress);
      
      ApplyMappingMode;
      glActiveTextureARB(GL_TEXTURE0_ARB);
   end;
end;

// UnApplyAsTextureN
//
procedure TGLTexture.UnApplyAsTextureN(n : Integer; var rci : TRenderContextInfo; libMaterial : TGLLibMaterial);
begin
   glActiveTextureARB(GL_TEXTURE0_ARB+n-1);
   UnApplyMappingMode;
   if    (Image.NativeTextureTarget<>GL_TEXTURE_2D)
      or (Assigned(libMaterial) and (not libMaterial.FTextureMatrixIsIdentity)) then begin
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
   end;
   glDisable(Image.NativeTextureTarget);
   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

// AllocateHandle
//
function TGLTexture.AllocateHandle : TGLuint;
var
   target : TGLUInt;
begin
   if FTextureHandle.Handle=0 then begin
      FTextureHandle.AllocateHandle;
      Assert(FTextureHandle.Handle<>0);
   end;
   // bind texture
   target:=Image.NativeTextureTarget;
   if (target<>GL_TEXTURE_CUBE_MAP_ARB) or GL_ARB_texture_cube_map then begin
      glBindTexture(target, FTextureHandle.Handle);
      PrepareParams(target);
   end;
	Result:=FTextureHandle.Handle;
   FChanges:=[];
end;

// IsHandleAllocated
//
function TGLTexture.IsHandleAllocated : Boolean;
begin
   Result:=(FTextureHandle.Handle<>0);
end;

// GetHandle
//
function TGLTexture.GetHandle : TGLuint;
var
   i, target : TGLUInt;
   cubeMapSize : Integer;
   cmt : TGLCubeMapTarget;
   cubeMapOk : Boolean;
   cubeMapImage : TGLCubeMapImage;
begin
	if (FTextureHandle.Handle=0) or (FChanges<>[]) then begin
      AllocateHandle;
      // Load images
      target:=Image.NativeTextureTarget;
      if (target<>GL_TEXTURE_CUBE_MAP_ARB) or GL_ARB_texture_cube_map then begin
         if target=GL_TEXTURE_CUBE_MAP_ARB then begin
            // first check if everything is coherent, otherwise, bail out
            cubeMapImage:=(Image as TGLCubeMapImage);
            cubeMapSize:=cubeMapImage.Picture[cmtPX].Width;
            cubeMapOk:=(cubeMapSize>0);
            if cubeMapOk then begin
               for cmt:=cmtPX to cmtNZ do with cubeMapImage.Picture[cmt] do begin
                  cubeMapOk:=(Width=cubeMapSize) and (Height=cubeMapSize);
                  if not cubeMapOk then Break;
               end;
            end;
            if cubeMapOk then begin
               for i:=GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB to GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB do
                  PrepareImage(i);
            end;
         end else PrepareImage(target);
      end;
	end;
	Result:=FTextureHandle.Handle;
end;

// DestroyHandles
//
procedure TGLTexture.DestroyHandles;
begin
   FTextureHandle.DestroyHandle;
   FChanges:=[tcParams,tcImage];
   FRequiredMemorySize:=-1;
end;

function TGLTexture.IsFloatType: boolean;
begin
  result:=TextureFormat in [tfRGBAFloat16, tfRGBAFloat32];
end;

// OpenGLTextureFormat
//
function TGLTexture.OpenGLTextureFormat : Integer;
const
   cTextureFormatToOpenGL : array [tfRGB..high(TGLTextureFormat)] of Integer =
      (GL_RGB8, GL_RGBA8, GL_RGB5, GL_RGBA4, GL_ALPHA8, GL_LUMINANCE8,
       GL_LUMINANCE8_ALPHA8, GL_INTENSITY8, GL_RGB8,
       GL_RGBA_FLOAT16_ATI, GL_RGBA_FLOAT32_ATI); // float_type default is ATI types
   cCompressedTextureFormatToOpenGL : array [tfRGB..high(TGLTextureFormat)] of Integer =
      (GL_COMPRESSED_RGB_ARB, GL_COMPRESSED_RGBA_ARB, GL_COMPRESSED_RGB_ARB,
       GL_COMPRESSED_RGBA_ARB, GL_COMPRESSED_ALPHA_ARB, GL_COMPRESSED_LUMINANCE_ARB,
       GL_COMPRESSED_LUMINANCE_ALPHA_ARB, GL_COMPRESSED_INTENSITY_ARB,
       GL_COMPRESSED_RGB_ARB, 0, 0); // compression not supported for float_type 
var
   texForm : TGLTextureFormat;
   texComp : TGLTextureCompression;
begin
   if TextureFormat=tfDefault then
      if vDefaultTextureFormat=tfDefault then
         texForm:=tfRGBA
      else texForm:=vDefaultTextureFormat
   else texForm:=TextureFormat;

   if GL_ARB_texture_compression then begin
      if Compression=tcDefault then
         if vDefaultTextureCompression=tcDefault then
            texComp:=tcNone
         else texComp:=vDefaultTextureCompression
      else texComp:=Compression;
   end else texComp:=tcNone;

   if IsFloatType then texComp:=tcNone; // no compression support for float_type

   if texComp<>tcNone then begin
      case texComp of
         tcStandard : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_DONT_CARE);
         tcHighQuality : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_NICEST);
         tcHighSpeed : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_FASTEST);
      else
         Assert(False);
      end;
      Result:=cCompressedTextureFormatToOpenGL[texForm];
   end else Result:=cTextureFormatToOpenGL[texForm];

   if IsFloatType and not GL_ATI_texture_float then begin // override if NV
     case texForm of
       tfRGBAFloat16 : Result:=GL_FLOAT_RGBA16_NV;
       tfRGBAFloat32 : Result:=GL_FLOAT_RGBA32_NV;
     end;
   end;
end;

// PrepareImage
//
procedure TGLTexture.PrepareImage(target : TGLUInt);
var
   bitmap32 : TGLBitmap32;
   targetFormat : Integer;
begin
   bitmap32:=Image.GetBitmap32(target);

   if (bitmap32=nil) or bitmap32.IsEmpty then Exit;
   // select targetFormat from texture format & compression options
   targetFormat:=OpenGLTextureFormat;
   if TextureFormat=tfNormalMap then begin
      bitmap32.GrayScaleToNormalMap(NormalMapScale,
                                    TextureWrap in [twBoth, twHorizontal],
                                    TextureWrap in [twBoth, twVertical]);
   end;
   // prepare AlphaChannel
   case ImageAlpha of
      tiaDefault : ;// nothing to do
      tiaAlphaFromIntensity :
         bitmap32.SetAlphaFromIntensity;
      tiaSuperBlackTransparent :
         bitmap32.SetAlphaTransparentForColor($000000);
      tiaLuminance :
         bitmap32.SetAlphaFromIntensity;
      tiaLuminanceSqrt : begin
         bitmap32.SetAlphaFromIntensity;
         bitmap32.SqrtAlpha;
      end;
      tiaOpaque :
         bitmap32.SetAlphaToValue(255);
      tiaTopLeftPointColorTransparent :
         bitmap32.SetAlphaTransparentForColor(bitmap32.Data[0]);
      tiaInverseLuminance : begin
         bitmap32.SetAlphaFromIntensity;
         bitmap32.InvertAlpha;
      end;
      tiaInverseLuminanceSqrt : begin
         bitmap32.SetAlphaFromIntensity;
         bitmap32.SqrtAlpha;
         bitmap32.InvertAlpha;
      end;
   else
      Assert(False);
   end;
   // apply brightness correction
   if FImageBrightness<>1.0 then 
      bitmap32.BrightnessCorrection(FImageBrightness);
   // apply gamma correction
   if FImageGamma<>1.0 then
      bitmap32.GammaCorrection(FImageGamma);

   CheckOpenGLError;
   bitmap32.RegisterAsOpenGLTexture(target, MinFilter, targetFormat, FTexWidth, FTexHeight);
   glGetTexLevelParameteriv(target, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB,
                            @FRequiredMemorySize);
   if glGetError<>GL_NO_ERROR then
      FRequiredMemorySize:=-1;
   ClearGLError; // ignore texture-size errors
   image.ReleaseBitmap32;
end;

// PrepareParams
//
procedure TGLTexture.PrepareParams(target : TGLUInt);
const
	cTextureSWrap : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_REPEAT );
	cTextureTWrap : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP_TO_EDGE, GL_REPEAT, GL_CLAMP_TO_EDGE );
{	cTextureSWrapARB : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP_TO_BORDER_ARB, GL_CLAMP_TO_BORDER_ARB, GL_REPEAT );
	cTextureTWrapARB : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP_TO_BORDER_ARB, GL_REPEAT, GL_CLAMP_TO_BORDER_ARB ); }
	cTextureSWrapOld : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP, GL_CLAMP, GL_REPEAT );
	cTextureTWrapOld : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP, GL_REPEAT, GL_CLAMP );
	cTextureMagFilter : array [maNearest..maLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR );
	cTextureMinFilter : array [miNearest..miLinearMipmapLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
							  GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
							  GL_LINEAR_MIPMAP_LINEAR );
   cFilteringQuality : array [tfIsotropic..tfAnisotropic] of Integer = (1, 2);
begin
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

{   if GL_ARB_texture_border_clamp then begin
   	glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapARB[FTextureWrap]);
	   glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapARB[FTextureWrap]);
   end else }

   if IsFloatType then begin // float_type
     // Note: HW accerl. only with GL_CLAMP_TO_EDGE for Nvidia GPUs
     glTexParameteri(target, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
     glTexParameteri(target, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
     // Linear filtering works with nv40, 16-bit float only  (via GL_ATI_texture_float)
     if GL_ATI_texture_float and (TextureFormat = tfRGBAFloat16) then begin
	      glTexParameteri(target, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
        glTexParameteri(target, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
       end
     else
       begin
  	     glTexParameteri(target, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
         glTexParameteri(target, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
       end;
   end else begin
     if GL_VERSION_1_2 or GL_EXT_texture_edge_clamp then begin
       glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrap[FTextureWrap]);
       glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrap[FTextureWrap]);
     end else begin
       glTexParameteri(target, GL_TEXTURE_WRAP_S, cTextureSWrapOld[FTextureWrap]);
       glTexParameteri(target, GL_TEXTURE_WRAP_T, cTextureTWrapOld[FTextureWrap]);
     end;

	   glTexParameteri(target, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
	   glTexParameteri(target, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

     if GL_EXT_texture_filter_anisotropic then
       glTexParameteri(target, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                       cFilteringQuality[FFilteringQuality]);
   end;
end;

// DoOnTextureNeeded
//
procedure TGLTexture.DoOnTextureNeeded(Sender : TObject; var textureFileName : String);
begin
   if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(Sender, textureFileName);
end;


// ---------------
// --------------- TGLTextureExItem ---------------
// ---------------

// Create
//
constructor TGLTextureExItem.Create(ACollection : TCollection);
begin
  inherited;

  FTexture:=TGLTexture.Create(Self);
  FTextureOffset:=TGLCoordinates.CreateInitialized(Self, NullHMGVector);
  FTextureOffset.OnNotifyChange:=OnNotifyChange;
  FTextureScale:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector);
  FTextureScale.OnNotifyChange:=OnNotifyChange;

  FTextureIndex:=ID;
  FTextureMatrix:=IdentityHMGMatrix;
end;

// Destroy
//
destructor TGLTextureExItem.Destroy;
begin
  FTexture.Free;
  FTextureOffset.Free;
  FTextureScale.Free;

  inherited;
end;

// Assign
//
procedure TGLTextureExItem.Assign(Source: TPersistent);
begin
  if Source is TGLTextureExItem then begin
    Texture:=TGLTextureExItem(Source).Texture;
    TextureIndex:=TGLTextureExItem(Source).TextureIndex;
    TextureOffset:=TGLTextureExItem(Source).TextureOffset;
    TextureScale:=TGLTextureExItem(Source).TextureScale;
    NotifyChange(Self);
  end else inherited;
end;

// NotifyChange
//
procedure TGLTextureExItem.NotifyChange(Sender : TObject);
begin
  if Assigned(Collection) then
    TGLTextureEx(Collection).NotifyChange(Sender);
end;

// Apply
//
procedure TGLTextureExItem.Apply(var rci : TRenderContextInfo);
begin
  FApplied:=False;
  if FTexture.Enabled then begin
    glActiveTextureARB(GL_TEXTURE0_ARB+FTextureIndex);
    glMatrixMode(GL_TEXTURE);
    glPushMatrix;
    if FTextureMatrixIsIdentity then glLoadIdentity
    else glLoadMatrixf(@FTextureMatrix[0][0]);
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
    if FTextureIndex = 0 then
      FTexture.Apply(rci)
    else if FTextureIndex = 1 then
      FTexture.ApplyAsTexture2(rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.ApplyAsTextureN(FTextureIndex+1, rci, nil);
    FApplied:=True;
  end;
end;

// UnApply
//
procedure TGLTextureExItem.UnApply(var rci : TRenderContextInfo);
begin
  if FApplied then begin
    if FTextureIndex = 0 then
      FTexture.UnApply(rci)
    else if FTextureIndex = 1 then
      FTexture.UnApplyAsTexture2(rci, nil)
    else if FTextureIndex >= 2 then
      FTexture.UnApplyAsTextureN(FTextureIndex+1, rci, nil);
    glActiveTextureARB(GL_TEXTURE0_ARB+FTextureIndex);
    glMatrixMode(GL_TEXTURE);
    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glActiveTextureARB(GL_TEXTURE0_ARB);
    FApplied:=False;
  end;
end;

// GetDisplayName
//
function TGLTextureExItem.GetDisplayName : String;
begin
  Result:=Format('Tex [%d]', [FTextureIndex]);
end;

// GetOwner
//
function TGLTextureExItem.GetOwner : TPersistent;
begin
  Result:=Collection;
end;

// NotifyTexMapChange
//
procedure TGLTextureExItem.NotifyTexMapChange(Sender : TObject);
begin
  if Assigned(TGLTextureEx(Collection).FMaterial) then
    TGLTextureEx(Collection).FMaterial.NotifyTexMapChange(Sender);
end;

// SetTexture
//
procedure TGLTextureExItem.SetTexture(const Value : TGLTexture);
begin
  FTexture.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureIndex
//
procedure TGLTextureExItem.SetTextureIndex(const Value : Integer);
var
  temp : Integer;
begin
  temp:=Value;
  if temp<0 then temp:=0;
  if temp<>FTextureIndex then begin
    FTextureIndex:=temp;
    NotifyChange(Self);
  end;
end;

// SetTextureOffset
//
procedure TGLTextureExItem.SetTextureOffset(const Value : TGLCoordinates);
begin
  FTextureOffset.Assign(Value);
  NotifyChange(Self);
end;

// SetTextureScale
//
procedure TGLTextureExItem.SetTextureScale(const Value : TGLCoordinates);
begin
  FTextureScale.Assign(Value);
  NotifyChange(Self);
end;

// CalculateTextureMatrix
//
procedure TGLTextureExItem.CalculateTextureMatrix;
begin
  if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) then
    FTextureMatrixIsIdentity:=True
  else begin
    FTextureMatrixIsIdentity:=False;
    FTextureMatrix:=CreateScaleAndTranslationMatrix(TextureScale.AsVector,
                                                    TextureOffset.AsVector);
  end;
  NotifyChange(Self);
end;

// OnNotifyChange
//
procedure TGLTextureExItem.OnNotifyChange(Sender : TObject);
begin
   CalculateTextureMatrix;
end;


// ---------------
// --------------- TGLTextureEx ---------------
// ---------------

// Create
//
constructor TGLTextureEx.Create(AOwner : TGLMaterial);
begin
  inherited Create(TGLTextureExItem);

  FMaterial:=AOwner;
end;

// NotifyChange
//
procedure TGLTextureEx.NotifyChange(Sender : TObject);
begin
  if Assigned(FMaterial) then
    FMaterial.NotifyChange(Sender);
end;

// Apply
//
procedure TGLTextureEx.Apply(var rci : TRenderContextInfo);
var
  i, texUnits : Integer;
  units : Cardinal;
begin
  if not GL_ARB_multitexture then exit;

  units:=0;
  glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @texUnits);
  for i:=0 to Count-1 do begin
    if Items[i].TextureIndex<texUnits then begin
      Items[i].Apply(rci);
      if Items[i].FApplied then
        if (Items[i].TextureIndex>0) and (Items[i].Texture.MappingMode=tmmUser) then
          units:=units or (1 shl Items[i].TextureIndex);
    end;
  end;
  if units>0 then
    xglMapTexCoordToArbitraryAdd(units);
end;

// UnApply
//
procedure TGLTextureEx.UnApply(var rci : TRenderContextInfo);
var
  i : Integer;
begin
  if not GL_ARB_multitexture then exit;
  for i:=0 to Count-1 do
    Items[i].UnApply(rci);
end;

// Add
//
function TGLTextureEx.Add : TGLTextureExItem;
begin
  Result:=TGLTextureExItem(inherited Add);
end;

// Loaded
//
procedure TGLTextureEx.Loaded;
var
  i : Integer;
begin
  for i:=0 to Count-1 do
    Items[i].CalculateTextureMatrix;
end;

// GetOwner
//
function TGLTextureEx.GetOwner : TPersistent;
begin
  Result:=FMaterial;
end;

// SetItems
//
procedure TGLTextureEx.SetItems(index : Integer; const Value : TGLTextureExItem);
begin
  inherited SetItem(index, Value);
end;

// GetItems
//
function TGLTextureEx.GetItems(index : Integer) : TGLTextureExItem;
begin
  Result:=TGLTextureExItem(inherited GetItem(index));
end;

// IsTextureEnabled
//
function TGLTextureEx.IsTextureEnabled(Index : Integer) : Boolean;
var
   i : Integer;
begin
   Result:=False;
   if Self=nil then Exit;
   for i:=0 to Count-1 do
      if Items[i].TextureIndex = Index then
         Result:=Result or Items[i].Texture.Enabled;
end;


//----------------- TGLMaterial --------------------------------------------------

// Create
//
constructor TGLMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FFrontProperties:=TGLFaceProperties.Create(Self);
  FTexture:=nil; // AutoCreate
  FFaceCulling:=fcBufferDefault;
end;

// Destroy
//
destructor TGLMaterial.Destroy;
begin
   if Assigned(currentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
   FGLBackProperties.Free;
   FFrontProperties.Free;
   FTexture.Free;
   FTextureEx.Free;
   inherited Destroy;
end;

// SetBackProperties
//
procedure TGLMaterial.SetBackProperties(Values: TGLFaceProperties);
begin
	BackProperties.Assign(Values);
	NotifyChange(Self);
end;

// GetBackProperties
//
function TGLMaterial.GetBackProperties : TGLFaceProperties;
begin
   if not Assigned(FGLBackProperties) then
      FGLBackProperties:=TGLFaceProperties.Create(Self);
   Result:=FGLBackProperties;
end;

// SetFrontProperties
//
procedure TGLMaterial.SetFrontProperties(Values: TGLFaceProperties);
begin
	FFrontProperties.Assign(Values);
	NotifyChange(Self);
end;

// SetBlendingMode
//
procedure TGLMaterial.SetBlendingMode(const val : TBlendingMode);
begin
   if val <> FBlendingMode then begin
      FBlendingMode := val;
   	NotifyChange(Self);
   end;
end;

// SetMaterialOptions
//
procedure TGLMaterial.SetMaterialOptions(const val : TMaterialOptions);
begin
   if val<>FMaterialOptions then begin
      FMaterialOptions:=val;
   	NotifyChange(Self);
   end;
end;

// GetTexture
//
function TGLMaterial.GetTexture : TGLTexture;
begin
   if not Assigned(FTexture) then
      FTexture:=TGLTexture.Create(Self);
   Result:=FTexture;
end;

// SetTexture
//
procedure TGLMaterial.SetTexture(aTexture : TGLTexture);
begin
   if Assigned(aTexture) then
   	Texture.Assign(ATexture)
   else FreeAndNil(FTexture);
end;

// SetFaceCulling
//
procedure TGLMaterial.SetFaceCulling(const val : TFaceCulling);
begin
   if val<>FFaceCulling then begin
      FFaceCulling:=val;
   	NotifyChange(Self);
   end;
end;

// SetMaterialLibrary
//
procedure TGLMaterial.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   FMaterialLibrary:=val;
   SetLibMaterialName(LibMaterialName);
end;

// SetLibMaterialName
//
procedure TGLMaterial.SetLibMaterialName(const val : TGLLibMaterialName);

   function MaterialLoopFrom(curMat : TGLLibMaterial) : Boolean;
   var
      loopCount : Integer;
   begin
      loopCount:=0;
      while Assigned(curMat) and (loopCount<16) do begin
         with curMat.Material do begin
            if MaterialLibrary<>nil then
               curMat:=MaterialLibrary.Materials.GetLibMaterialByName(LibMaterialName)
            else curMat:=nil;
         end;
         Inc(loopCount)
      end;
      Result:=(loopCount>=16);
   end;

var
   newLibMaterial : TGLLibMaterial;
begin
   // locate new libmaterial
   if Assigned(FMaterialLibrary) then
      newLibMaterial:=MaterialLibrary.Materials.GetLibMaterialByName(val)
   else newLibMaterial:=nil;
   // make sure new won't trigger an infinite loop
   Assert(not MaterialLoopFrom(newLibMaterial),
          'Error: Cyclic material reference detected!');
   FLibMaterialName:=val;
   // unregister if required
   if newLibMaterial<>currentLibMaterial then begin
      // unregister from old
      if Assigned(currentLibMaterial) then
         currentLibMaterial.UnregisterUser(Self);
      currentLibMaterial:=newLibMaterial;
      // register with new
      if Assigned(currentLibMaterial) then
         currentLibMaterial.RegisterUser(Self);
      NotifyTexMapChange(Self);
   end;
end;

// GetTextureEx
//
function TGLMaterial.GetTextureEx : TGLTextureEx;
begin
   if not Assigned(FTextureEx) then
      FTextureEx:=TGLTextureEx.Create(Self);
   Result:=FTextureEx;
end;

// SetTextureEx
//
procedure TGLMaterial.SetTextureEx(const Value : TGLTextureEx);
begin
   if Assigned(Value) or Assigned(FTextureEx) then
      TextureEx.Assign(Value);
end;

// StoreTextureEx
//
function TGLMaterial.StoreTextureEx : Boolean;
begin
   Result:=(Assigned(FTextureEx) and (TextureEx.Count>0));
end;

// NotifyLibMaterialDestruction
//
procedure TGLMaterial.NotifyLibMaterialDestruction;
begin
   FMaterialLibrary:=nil;
   FLibMaterialName:='';
   currentLibMaterial:=nil;
end;

// Loaded
//
procedure TGLMaterial.Loaded;
begin
   inherited;
   if Assigned(FTextureEx) then
      TextureEx.Loaded;
end;

// StoreMaterialProps
//
function TGLMaterial.StoreMaterialProps : Boolean;
begin
	Result:=not Assigned(currentLibMaterial);
end;

// PrepareBuildList
//
procedure TGLMaterial.PrepareBuildList;
begin
   if Assigned(FTexture) and (not FTexture.Disabled) then
      FTexture.PrepareBuildList;
end;

// Apply
//
procedure TGLMaterial.Apply(var rci : TRenderContextInfo);
begin
	if Assigned(currentLibMaterial) then
		currentLibMaterial.Apply(rci)
	else begin
      // Lighting switch
      if moNoLighting in MaterialOptions then begin
         if stLighting in rci.GLStates.States then begin
            rci.GLStates.UnSetGLState(stLighting);
            Inc(rci.lightingDisabledCounter);
         end;
      end;
      if stLighting in rci.GLStates.States then
         FFrontProperties.Apply(rci, GL_FRONT)
      else FFrontProperties.ApplyNoLighting(rci, GL_FRONT);
      // Apply FaceCulling and BackProperties (if needs be)
      if (stCullFace in rci.GLStates.States) then begin
         // currently culling
         case FFaceCulling of
            fcBufferDefault : if not rci.bufferFaceCull then begin
               rci.GLStates.UnSetGLState(stCullFace);
               BackProperties.Apply(rci, GL_BACK);
            end;
            fcCull : ; // nothing to do
            fcNoCull : begin
               rci.GLStates.UnSetGLState(stCullFace);
               BackProperties.Apply(rci, GL_BACK);
            end;
         else
            Assert(False);
         end;
      end else begin
         // currently NOT culling
         case FFaceCulling of
            fcBufferDefault : begin
               if rci.bufferFaceCull then
                  rci.GLStates.SetGLState(stCullFace)
               else BackProperties.Apply(rci, GL_BACK);
            end;
            fcCull : rci.GLStates.SetGLState(stCullFace);
            fcNoCull : BackProperties.Apply(rci, GL_BACK);
         else
            Assert(False);
         end;
      end;
      // Apply Blending mode
      if not rci.ignoreBlendingRequests then case FBlendingMode of
			bmOpaque : begin
            rci.GLStates.UnSetGLState(stBlend);
            rci.GLStates.UnSetGLState(stAlphaTest);
         end;
			bmTransparency : begin
				rci.GLStates.SetGLState(stBlend);
            rci.GLStates.SetGLState(stAlphaTest);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			end;
			bmAdditive : begin
				rci.GLStates.SetGLState(stBlend);
            rci.GLStates.SetGLState(stAlphaTest);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE);
			end;
         bmAlphaTest50 : begin
            rci.GLStates.UnSetGLState(stBlend);
            rci.GLStates.SetGLState(stAlphaTest);
            glAlphaFunc(GL_GEQUAL, 0.5);
         end;
         bmAlphaTest100 : begin
            rci.GLStates.UnSetGLState(stBlend);
            rci.GLStates.SetGLState(stAlphaTest);
            glAlphaFunc(GL_GEQUAL, 1.0);
         end;
         bmModulate : begin
            rci.GLStates.SetGLState(stBlend);
            rci.GLStates.SetGLState(stAlphaTest);
            glBlendFunc(GL_DST_COLOR, GL_ZERO);
         end;
      else
         Assert(False);
		end;
      // Fog switch
      if moIgnoreFog in MaterialOptions then begin
         if stFog in rci.GLStates.States then begin
            rci.GLStates.UnSetGLState(stFog);
            Inc(rci.fogDisabledCounter);
         end;
      end;
      if not Assigned(FTextureEx) then begin
         if Assigned(FTexture) then
         	FTexture.Apply(rci)
      end else begin
         if Assigned(FTexture) and not FTextureEx.IsTextureEnabled(0) then
         	FTexture.Apply(rci)
         else if FTextureEx.Count>0 then
            FTextureEx.Apply(rci);
      end;
	end;
end;

// UnApply
//
function TGLMaterial.UnApply(var rci : TRenderContextInfo) : Boolean;
begin
	if Assigned(currentLibMaterial) then
		Result:=currentLibMaterial.UnApply(rci)
   else begin
      if BlendingMode in [bmAlphaTest50, bmAlphaTest100] then
         glAlphaFunc(GL_GREATER, 0);
      if moNoLighting in MaterialOptions then begin
         if rci.lightingDisabledCounter>0 then begin
            Dec(rci.lightingDisabledCounter);
            if rci.lightingDisabledCounter=0 then
               rci.GLStates.SetGLState(stLighting);
         end;
      end;
      if moIgnoreFog in MaterialOptions then begin
         if rci.fogDisabledCounter>0 then begin
            Dec(rci.fogDisabledCounter);
            if rci.fogDisabledCounter=0 then
               rci.GLStates.SetGLState(stFog);
         end;
      end;
      if Assigned(FTexture) and (not FTexture.Disabled) and (not FTextureEx.IsTextureEnabled(0)) then
         FTexture.UnApply(rci)
      else if Assigned(FTextureEx) then
         FTextureEx.UnApply(rci);
      Result:=False;
   end;
end;

// Assign
//
procedure TGLMaterial.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLMaterial) then begin
      if Assigned(TGLMaterial(Source).FGLBackProperties) then
         BackProperties.Assign(TGLMaterial(Source).BackProperties)
      else FreeAndNil(FGLBackProperties);
      FFrontProperties.Assign(TGLMaterial(Source).FFrontProperties);
		FBlendingMode:=TGLMaterial(Source).FBlendingMode;
      FMaterialOptions:=TGLMaterial(Source).FMaterialOptions;
      if Assigned(TGLMaterial(Source).FTexture) then
         Texture.Assign(TGLMaterial(Source).FTexture)
      else FreeAndNil(FTexture);
      FFaceCulling:=TGLMaterial(Source).FFaceCulling;
		FMaterialLibrary:=TGLMaterial(Source).MaterialLibrary;
      SetLibMaterialName(TGLMaterial(Source).LibMaterialName);
      TextureEx.Assign(TGLMaterial(Source).TextureEx);
   	NotifyChange(Self);
   end else inherited;
end;

// NotifyChange
//
procedure TGLMaterial.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then
      if Owner is TGLBaseSceneObject then
         TGLBaseSceneObject(Owner).NotifyChange(Self)
      else if Owner is TGLLibMaterial then
         TGLLibMaterial(Owner).NotifyUsers;
end;

// NotifyTexMapChange
//
procedure TGLMaterial.NotifyTexMapChange(Sender : TObject);
begin
   if Assigned(Owner) then
      if Owner is TGLBaseSceneObject then
         TGLBaseSceneObject(Owner).StructureChanged
      else if Owner is TGLLibMaterial then
         TGLLibMaterial(Owner).NotifyUsersOfTexMapChange;
end;

// DestroyHandles
//
procedure TGLMaterial.DestroyHandles;
begin
   if Assigned(FTexture) then
      FTexture.DestroyHandles;
end;

// Blended
//
function TGLMaterial.Blended : Boolean;
begin
   if Assigned(currentLibMaterial) then
      Result:=currentLibMaterial.Material.Blended
   else Result:=not (BlendingMode in [bmOpaque, bmAlphaTest50, bmAlphaTest100]);
end;

// HasSecondaryTexture
//
function TGLMaterial.HasSecondaryTexture : Boolean;
begin
   Result:=Assigned(currentLibMaterial) and Assigned(currentLibMaterial.libMatTexture2);
end;

// MaterialIsLinkedToLib
//
function TGLMaterial.MaterialIsLinkedToLib : Boolean;
begin
   Result:=Assigned(currentLibMaterial);
end;

// GetActualPrimaryTexture
//
function TGLMaterial.GetActualPrimaryTexture : TGLTexture;
begin
   if Assigned(currentLibMaterial) then
      Result:=currentLibMaterial.Material.Texture
   else Result:=Texture;
end;

// ------------------
// ------------------ TGLLibMaterial ------------------
// ------------------

// Create
//
constructor TGLLibMaterial.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   userList:=TList.Create;
   FName:=TGLLibMaterials(Collection).MakeUniqueName('LibMaterial');
   FNameHashKey:=ComputeNameHashKey(FName);
   FMaterial:=TGLMaterial.Create(Self);
   FMaterial.Texture.OnTextureNeeded:=DoOnTextureNeeded;
   FTextureOffset:=TGLCoordinates.CreateInitialized(Self, NullHmgVector);
   FTextureOffset.OnNotifyChange:=OnNotifyChange;
   FTextureScale:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector);
   FTextureScale.OnNotifyChange:=OnNotifyChange;
   FTextureMatrixIsIdentity:=True;
end;

// Destroy
//
destructor TGLLibMaterial.Destroy;
var
   i : Integer;
   matObj : TObject;
begin
   Shader:=nil; // drop dependency
   Texture2Name:=''; // drop dependency
   for i:=0 to userList.Count-1 do begin
      matObj:=TObject(userList[i]);
      if matObj is TGLMaterial then
         TGLMaterial(matObj).NotifyLibMaterialDestruction
      else if matObj is TGLLibMaterial then begin
         TGLLibMaterial(matObj).libMatTexture2:=nil;
         TGLLibMaterial(matObj).FTexture2Name:='';
      end;
   end;
   userList.Free;
   FMaterial.Free;
   FTextureOffset.Free;
   FTextureScale.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLLibMaterial.Assign(Source: TPersistent);
begin
	if Source is TGLLibMaterial then begin
      FName:=TGLLibMaterials(Collection).MakeUniqueName(TGLLibMaterial(Source).Name);
      FNameHashKey:=ComputeNameHashKey(FName);
      FMaterial.Assign(TGLLibMaterial(Source).Material);
      FTextureOffset.Assign(TGLLibMaterial(Source).TextureOffset);
      FTextureScale.Assign(TGLLibMaterial(Source).TextureScale);
      FTexture2Name:=TGLLibMaterial(Source).Texture2Name;
      CalculateTextureMatrix;
	end else inherited;
end;

// PrepareBuildList
//
procedure TGLLibMaterial.PrepareBuildList;
begin
   if Assigned(Self) then
      Material.PrepareBuildList;
end;

// Apply
//
procedure TGLLibMaterial.Apply(var rci : TRenderContextInfo);
var
   multitextured : Boolean;
begin
   xglBeginUpdate;
   if Assigned(FShader) then begin
      case Shader.ShaderStyle of
         ssHighLevel : Shader.Apply(rci, Self);
         ssReplace : begin
            Shader.Apply(rci, Self);
            Exit;
         end;
      end;
   end;
   if (Texture2Name<>'') then begin //and GL_ARB_multitexture and (not vSecondTextureUnitForbidden)
      if not Assigned(libMatTexture2) then begin
         libMatTexture2:=TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
         if Assigned(libMatTexture2) then
            libMatTexture2.RegisterUser(Self)
         else FTexture2Name:='';
      end;
      multitextured:=Assigned(libMatTexture2);
                     //and (not libMatTexture2.Material.Texture.Disabled);
   end else multitextured:=False;
   if not multitextured then begin
      // no multitexturing ("standard" mode)
      if not Material.Texture.Disabled then
         if not FTextureMatrixIsIdentity then
            rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
      Material.Apply(rci);
   end else begin
      // multitexturing is ON
      if not FTextureMatrixIsIdentity then
         rci.GLStates.SetGLTextureMatrix(FTextureMatrix);
      Material.Apply(rci);

      libMatTexture2.Material.Texture.ApplyAsTexture2(rci, libMatTexture2);
      if (not Material.Texture.Disabled) and (Material.Texture.MappingMode=tmmUser) then
         if libMatTexture2.Material.Texture.MappingMode=tmmUser then
            xglMapTexCoordToDual
         else xglMapTexCoordToMain
      else if libMatTexture2.Material.Texture.MappingMode=tmmUser then
         xglMapTexCoordToSecond
      else xglMapTexCoordToMain;

      // EG: TextureEx stuff below doesn't seem to work for libmaterials,
      //     and interferes with Texture2, not sure what should be done to get
      //     it operational with shaders and a 2nd material active, so I just
      //     restored classic behaviour
{      if Assigned(Material.FTextureEx) and not Material.TextureEx.IsTextureEnabled(1) then begin
         libMatTexture2.Material.Texture.ApplyAsTexture2(rci, libMatTexture2);
         // calculate and apply appropriate xgl mode
         if (not Material.Texture.Disabled) and (Material.Texture.MappingMode=tmmUser) then
            xglMapTexCoordToArbitraryAdd(1);
         if libMatTexture2.Material.Texture.MappingMode=tmmUser then
           xglMapTexCoordToArbitraryAdd(2); }

         // OLD TEXCOORD MAPPING
         //if (not Material.Texture.Disabled) and (Material.Texture.MappingMode=tmmUser) then
         //   if libMatTexture2.Material.Texture.MappingMode=tmmUser then
         //      xglMapTexCoordToDual
         //   else xglMapTexCoordToMain
         //else if libMatTexture2.Material.Texture.MappingMode=tmmUser then
         //   xglMapTexCoordToSecond
         //else xglMapTexCoordToMain;
//      end;
   end;
   if Assigned(FShader) then begin
      case Shader.ShaderStyle of
         ssLowLevel : Shader.Apply(rci, Self);
      end;
   end;
   xglEndUpdate;
end;

// UnApply
//
function TGLLibMaterial.UnApply(var rci : TRenderContextInfo) : Boolean;
begin
   Result:=False;
   if Assigned(FShader) then begin
      case Shader.ShaderStyle of
         ssLowLevel : Result:=Shader.UnApply(rci);
         ssReplace : begin
            Result:=Shader.UnApply(rci);
            Exit;
         end;
      end;
   end;
   if not Result then begin
      // if multipassing, this will occur upon last pass only
{      if Assigned(Material.FTextureEx) then begin
         if not Material.TextureEx.IsTextureEnabled(1) then begin}
            if Assigned(libMatTexture2) and (not vSecondTextureUnitForbidden) then begin
               libMatTexture2.Material.Texture.UnApplyAsTexture2(rci, libMatTexture2);
               xglMapTexCoordToMain;
            end;
{         end;
      end; }
      Material.UnApply(rci);
      if not Material.Texture.Disabled then
         if not FTextureMatrixIsIdentity then
            rci.GLStates.ResetGLTextureMatrix;
      if Assigned(FShader) then begin
         case Shader.ShaderStyle of
            ssHighLevel : Result:=Shader.UnApply(rci);
         end;
      end;
   end;
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(obj : TGLUpdateAbleObject);
begin
   Assert(userList.IndexOf(obj)<0);
   userList.Add(obj);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(obj : TGLUpdateAbleObject);
begin
   userList.Remove(obj);
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(comp : TGLUpdateAbleComponent);
begin
   Assert(userList.IndexOf(comp)<0);
   userList.Add(comp);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(comp : TGLUpdateAbleComponent);
begin
   userList.Remove(comp);
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(libMaterial : TGLLibMaterial);
begin
   Assert(userList.IndexOf(libMaterial)<0);
   userList.Add(libMaterial);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(libMaterial : TGLLibMaterial);
begin
   userList.Remove(libMaterial);
end;

// NotifyUsers
//
procedure TGLLibMaterial.NotifyUsers;
var
   i : Integer;
   obj : TObject;
begin
   if notifying then Exit;
   notifying:=True;
   try
      for i:=0 to userList.Count-1 do begin
         obj:=TObject(userList[i]);
         if obj is TGLUpdateAbleObject then
            TGLUpdateAbleObject(userList[i]).NotifyChange(Self)
         else if obj is TGLUpdateAbleComponent then
            TGLUpdateAbleComponent(userList[i]).NotifyChange(Self)
         else begin
            Assert(obj is TGLLibMaterial);
            TGLLibMaterial(userList[i]).NotifyUsers;
         end;
      end;
   finally
      notifying:=False;
   end;
end;

// NotifyUsersOfTexMapChange
//
procedure TGLLibMaterial.NotifyUsersOfTexMapChange;
var
   i : Integer;
   obj : TObject;
begin
   if notifying then Exit;
   notifying:=True;
   try
      for i:=0 to userList.Count-1 do begin
         obj:=TObject(userList[i]);
         if obj is TGLMaterial then
            TGLMaterial(userList[i]).NotifyTexMapChange(Self)
         else if obj is TGLLibMaterial then
            TGLLibMaterial(userList[i]).NotifyUsersOfTexMapChange
         else if obj is TGLUpdateAbleObject then
            TGLUpdateAbleObject(userList[i]).NotifyChange(Self)
         else if obj is TGLUpdateAbleComponent then
            TGLUpdateAbleComponent(userList[i]).NotifyChange(Self);
      end;
   finally
      notifying:=False;
   end;
end;

// GetDisplayName
//
function TGLLibMaterial.GetDisplayName : String;
begin
	Result:=Name;
end;

// Loaded
//
procedure TGLLibMaterial.Loaded;
begin
   CalculateTextureMatrix;
   Material.Loaded;
end;

// ComputeNameHashKey
//
class function TGLLibMaterial.ComputeNameHashKey(const name : String) : Integer;
var
   i, n : Integer;
begin
   n:=Length(name);
   Result:=n;
   for i:=1 to n do
      Result:=(Result shl 1)+Byte(name[i]);
end;

// SetName
//
procedure TGLLibMaterial.SetName(const val : TGLLibMaterialName);
begin
   if val<>FName then begin
      if not (csLoading in TComponent(TGLLibMaterials(Collection).GetOwner).ComponentState) then begin
         if TGLLibMaterials(Collection).GetLibMaterialByName(val)<>Self then
            FName:=TGLLibMaterials(Collection).MakeUniqueName(val)
         else FName:=val;
      end else FName:=val;
      FNameHashKey:=ComputeNameHashKey(FName);
   end;
end;

// SetMaterial
//
procedure TGLLibMaterial.SetMaterial(const val : TGLMaterial);
begin
   FMaterial.Assign(val);
end;

// SetTextureOffset
//
procedure TGLLibMaterial.SetTextureOffset(const val : TGLCoordinates);
begin
   FTextureOffset.AsVector:=val.AsVector;
   CalculateTextureMatrix;
end;

// SetTextureScale
//
procedure TGLLibMaterial.SetTextureScale(const val : TGLCoordinates);
begin
   FTextureScale.AsVector:=val.AsVector;
   CalculateTextureMatrix;
end;

// SetTexture2
//
procedure TGLLibMaterial.SetTexture2Name(const val : TGLLibMaterialName);
begin
   if val<>Texture2Name then begin
      if Assigned(libMatTexture2) then begin
         libMatTexture2.UnregisterUser(Self);
         libMatTexture2:=nil;
      end;
      FTexture2Name:=val;
      NotifyUsers;
   end;
end;

// SetShader
//
procedure TGLLibMaterial.SetShader(const val : TGLShader);
begin
   if val<>FShader then begin
      if Assigned(FShader) then
         FShader.UnRegisterUser(Self);
      FShader:=val;
      if Assigned(FShader) then
         FShader.RegisterUser(Self);
      NotifyUsers;
   end;
end;

// CalculateTextureMatrix
//
procedure TGLLibMaterial.CalculateTextureMatrix;
begin
   if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) then
      FTextureMatrixIsIdentity:=True
   else begin
      FTextureMatrixIsIdentity:=False;
      FTextureMatrix:=CreateScaleAndTranslationMatrix(TextureScale.AsVector,
                                                      TextureOffset.AsVector);
   end;
   NotifyUsers;
end;

// DestroyHandles
//
procedure TGLLibMaterial.DestroyHandles;
var
   libMat : TGLLibMaterial;
begin
	FMaterial.DestroyHandles;
   if FTexture2Name<>'' then begin
      libMat:=TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMat) then
         libMat.DestroyHandles;
   end;
end;

// OnNotifyChange
//
procedure TGLLibMaterial.OnNotifyChange(Sender : TObject);
begin
   CalculateTextureMatrix;
end;

// DoOnTextureNeeded
//
procedure TGLLibMaterial.DoOnTextureNeeded(Sender : TObject; var textureFileName : String);
var
   mLib : TGLMaterialLibrary;
   i : Integer;
   tryName : String;
begin
   mLib:=TGLMaterialLibrary((Collection as TGLLibMaterials).GetOwner);
   if mLib is TGLMaterialLibrary then with mLib do
      if Assigned(FOnTextureNeeded) then
         FOnTextureNeeded(mLib, textureFileName);
   // if a ':' is present, or if it starts with a '\', consider it as an absolute path
   if (Pos(':', textureFileName)>0) or (Copy(textureFileName, 1, 1)='\') then Exit;
   // ok, not an absolute path, try given paths
   with mLib do begin
      if FTexturePathList<>nil then for i:=0 to FTexturePathList.Count-1 do begin
         tryName:=IncludeTrailingBackslash(FTexturePathList[i])+textureFileName;
         if (Assigned(vAFIOCreateFileStream) and FileStreamExists(tryName)) or FileExists(tryName) then begin
            textureFileName:=tryName;
            Break;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TGLLibMaterials ------------------
// ------------------

// Create
//
constructor TGLLibMaterials.Create(AOwner : TComponent);
begin
	inherited Create(AOwner, TGLLibMaterial);
end;

// Loaded
//
procedure TGLLibMaterials.Loaded;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Loaded;
end;

// SetItems
//
procedure TGLLibMaterials.SetItems(index : Integer; const val : TGLLibMaterial);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLLibMaterials.GetItems(index : Integer) : TGLLibMaterial;
begin
	Result:=TGLLibMaterial(inherited Items[index]);
end;

// DestroyHandles
//
procedure TGLLibMaterials.DestroyHandles;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].DestroyHandles;
end;

// Owner
//
function TGLLibMaterials.Owner : TPersistent;
begin
   Result:=GetOwner;
end;

// Add
//
function TGLLibMaterials.Add: TGLLibMaterial;
begin
	Result:=(inherited Add) as TGLLibMaterial;
end;

// FindItemID
//
function TGLLibMaterials.FindItemID(ID: Integer): TGLLibMaterial;
begin
	Result:=(inherited FindItemID(ID)) as TGLLibMaterial;
end;

// MakeUniqueName
//
function TGLLibMaterials.MakeUniqueName(const nameRoot : TGLLibMaterialName) : TGLLibMaterialName;
var
   i : Integer;
begin
   Result:=nameRoot;
   i:=1;
   while GetLibMaterialByName(Result)<>nil do begin
      Result:=nameRoot+IntToStr(i);
      Inc(i);
   end;
end;

// GetLibMaterialByName
//
function TGLLibMaterials.GetLibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;
var
   i, hk : Integer;
   lm : TGLLibMaterial;
begin
   hk:=TGLLibMaterial.ComputeNameHashKey(name);
   for i:=0 to Count-1 do begin
      lm:=TGLLibMaterial(inherited Items[i]);
      if (lm.NameHashKey=hk) and (lm.Name=name) then begin
         Result:=lm;
         Exit;
      end;
   end;
   Result:=nil;
end;

// PrepareBuildList
//
procedure TGLLibMaterials.PrepareBuildList;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TGLLibMaterial(inherited Items[i]).PrepareBuildList;
end;

// SetNamesToTStrings
//
procedure TGLLibMaterials.SetNamesToTStrings(aStrings : TStrings);
var
   i : Integer;
   lm : TGLLibMaterial;
begin
   with aStrings do begin
      BeginUpdate;
      Clear;
      for i:=0 to Self.Count-1 do begin
         lm:=TGLLibMaterial(inherited Items[i]);
         AddObject(lm.Name, lm);
      end;
      EndUpdate;
   end;
end;

// DeleteUnusedMaterials
//
procedure TGLLibMaterials.DeleteUnusedMaterials;
var
   i : Integer;
   gotNone : Boolean;
begin
   BeginUpdate;
   repeat
      gotNone:=True;
      for i:=Count-1 downto 0 do begin
         if TGLLibMaterial(inherited Items[i]).userList.Count=0 then begin
            TGLLibMaterial(inherited Items[i]).Free;
            gotNone:=False;
         end;
      end;
   until gotNone;
   EndUpdate;
end;

// ------------------
// ------------------ TGLMaterialLibrary ------------------
// ------------------

// Create
//
constructor TGLMaterialLibrary.Create(AOwner : TComponent);
begin
   inherited;
   FMaterials:=TGLLibMaterials.Create(Self);
end;

// Destroy
//
destructor TGLMaterialLibrary.Destroy;
begin
   Assert(FLastAppliedMaterial=nil, 'Unbalanced material application');
   FTexturePathList.Free;
   FMaterials.Free;
   FMaterials:=nil;
   inherited;
end;

// DestroyHandles
//
procedure TGLMaterialLibrary.DestroyHandles;
begin
   if Assigned(FMaterials) then
      FMaterials.DestroyHandles;
end;

// Loaded
//
procedure TGLMaterialLibrary.Loaded;
begin
   FMaterials.Loaded;
   inherited;
end;

// SetMaterials
//
procedure TGLMaterialLibrary.SetMaterials(const val : TGLLibMaterials);
begin
   FMaterials.Assign(val);
end;

// StoreMaterials
//
function TGLMaterialLibrary.StoreMaterials : Boolean;
begin
   Result:=(FMaterials.Count>0);
end;

// SetTexturePaths
//
procedure TGLMaterialLibrary.SetTexturePaths(const val : String);
var
   i, lp : Integer;

   procedure AddCurrent;
   var
      buf : String;
   begin
      buf:=Trim(Copy(val, lp+1, i-lp-1));
      if Length(buf)>0 then begin
         // make sure '\' is the terminator
         if buf[Length(buf)]<>'\' then buf:=buf+'\';
         FTexturePathList.Add(buf);
      end;
   end;

begin
	FTexturePathList.Free;
	FTexturePathList:=nil;
	FTexturePaths:=val;
   if val<>'' then begin
      FTexturePathList:=TStringList.Create;
      lp:=0;
      for i:=1 to Length(val) do begin
         if val[i]=';' then begin
            AddCurrent;
            lp:=i;
         end;
      end;
      i:=Length(val)+1;
      AddCurrent;
   end;
end;

// WriteToFiler
//
procedure TGLMaterialLibrary.WriteToFiler(writer : TVirtualWriter);
var
   i, j : Integer;
   libMat : TGLLibMaterial;
   tex : TGLTexture;
   img : TGLTextureImage;
   pim : TGLPersistentImage;
   ss : TStringStream;
   bmp : TGLBitmap;
   texExItem : TGLTextureExItem;
begin
   with writer do begin
      WriteInteger(2); // archive version 0, texture persistence only
                       // archive version 1, libmat properties
                       // archive version 2, Material.TextureEx properties
      WriteInteger(Materials.Count);
      for i:=0 to Materials.Count-1 do begin
         // version 0
         libMat:=Materials[i];
         WriteString(libMat.Name);
         tex:=libMat.Material.Texture;
         img:=tex.Image;
         pim:=TGLPersistentImage(img);
         if tex.Enabled and (img is TGLPersistentImage) and (pim.Picture.Graphic<>nil) then begin
            WriteBoolean(true);
            ss:=TStringStream.Create('');
            try
               bmp:=TGLBitmap.Create;
               try
                  bmp.Assign(pim.Picture.Graphic);
                  bmp.SaveToStream(ss);
               finally
                  bmp.Free;
               end;
               WriteString(ss.DataString);
            finally
               ss.Free;
            end;
         end else WriteBoolean(False);
         with libMat.Material.FrontProperties do begin
            Write(Ambient.AsAddress^, SizeOf(Single)*3);
            Write(Diffuse.AsAddress^, SizeOf(Single)*4);
            Write(Emission.AsAddress^, SizeOf(Single)*3);
            Write(Specular.AsAddress^, SizeOf(Single)*3);
         end;

         //version 1
         with libMat.Material.FrontProperties do begin
            Write(FShininess,1);
            WriteInteger(Integer(PolygonMode));
         end;
         with libMat.Material.BackProperties do begin
            Write(Ambient.AsAddress^, SizeOf(Single)*3);
            Write(Diffuse.AsAddress^, SizeOf(Single)*4);
            Write(Emission.AsAddress^, SizeOf(Single)*3);
            Write(Specular.AsAddress^, SizeOf(Single)*3);
            Write(Byte(FShininess),1);
            WriteInteger(Integer(PolygonMode));
         end;
         WriteInteger(Integer(libMat.Material.BlendingMode));
         WriteInteger(SizeOf(TMaterialOptions));
         Write(libMat.Material.MaterialOptions, SizeOf(TMaterialOptions));
         Write(libMat.TextureOffset.AsAddress^, SizeOf(Single)*3);
         Write(libMat.TextureScale.AsAddress^, SizeOf(Single)*3);
         WriteString(libMat.Texture2Name);

         // version 2
         WriteInteger(libMat.Material.TextureEx.Count);
         for j:=0 to libMat.Material.TextureEx.Count-1 do begin
            texExItem:=libMat.Material.TextureEx[j];
            img:=texExItem.Texture.Image;
            pim:=TGLPersistentImage(img);
            if texExItem.Texture.Enabled and (img is TGLPersistentImage)
            and (pim.Picture.Graphic<>nil) then begin
               WriteBoolean(True);
               ss:=TStringStream.Create('');
               try
                  bmp:=TGLBitmap.Create;
                  try
                     bmp.Assign(pim.Picture.Graphic);
                     bmp.SaveToStream(ss);
                  finally
                     bmp.Free;
                  end;
                  WriteString(ss.DataString);
               finally
                  ss.Free;
               end;
            end else WriteBoolean(False);
            WriteInteger(texExItem.TextureIndex);
            Write(texExItem.TextureOffset.AsAddress^, SizeOf(Single)*3);
            Write(texExItem.TextureScale.AsAddress^, SizeOf(Single)*3);
         end;
      end;
   end;
end;

// ReadFromFiler
//
procedure TGLMaterialLibrary.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
   libMat : TGLLibMaterial;
   i, n, size, tex, texCount : Integer;
   name : String;
   ss : TStringStream;
   bmp : TGLBitmap;
   texExItem : TGLTextureExItem;
begin
   archiveVersion:=reader.ReadInteger;
   if (archiveVersion>=0) and (archiveVersion<=2) then with reader do begin
      if not FDoNotClearMaterialsOnLoad then
         Materials.Clear;
      n:=ReadInteger;
      for i:=0 to n-1 do begin
         // version 0
         name:=ReadString;
         if FDoNotClearMaterialsOnLoad then
            libMat:=LibMaterialByName(name)
         else libMat:=nil;
         if ReadBoolean then begin
            ss:=TStringStream.Create(ReadString);
            try
               bmp:=TGLBitmap.Create;
               try
                  bmp.LoadFromStream(ss);
                  if libMat=nil then
                     libMat:=AddTextureMaterial(name, bmp)
                  else libMat.Material.Texture.Image.Assign(bmp);
               finally
                  bmp.Free;
               end;
            finally
               ss.Free;
            end;
         end else begin
            if libMat=nil then begin
               libMat:=Materials.Add;
               libMat.Name:=name;
            end;
         end;
         with libMat.Material.FrontProperties do begin
            Read(Ambient.AsAddress^, SizeOf(Single)*3);
            Read(Diffuse.AsAddress^, SizeOf(Single)*4);
            Read(Emission.AsAddress^, SizeOf(Single)*3);
            Read(Specular.AsAddress^, SizeOf(Single)*3);
         end;

         // version 1
         if archiveVersion >= 1 then begin
            with libMat.Material.FrontProperties do begin
               Read(FShininess,1);
               PolygonMode:=TPolygonMode(ReadInteger);
            end;
            with libMat.Material.BackProperties do begin
               Read(Ambient.AsAddress^, SizeOf(Single)*3);
               Read(Diffuse.AsAddress^, SizeOf(Single)*4);
               Read(Emission.AsAddress^, SizeOf(Single)*3);
               Read(Specular.AsAddress^, SizeOf(Single)*3);
               Read(FShininess,1);
               PolygonMode:=TPolygonMode(ReadInteger);
            end;
            libMat.Material.BlendingMode:=TBlendingMode(ReadInteger);
            size:=ReadInteger;
            Read(libMat.Material.FMaterialOptions, size);
            Read(libMat.TextureOffset.AsAddress^, SizeOf(Single)*3);
            Read(libMat.TextureScale.AsAddress^, SizeOf(Single)*3);
            libMat.Texture2Name:=ReadString;
         end;

         // version 2
         if archiveVersion >= 2 then begin
            texCount:=ReadInteger;
            for tex:=0 to texCount-1 do begin
               texExItem:=libMat.Material.TextureEx.Add;
               if ReadBoolean then begin
                  ss:=TStringStream.Create(ReadString);
                  bmp:=TGLBitmap.Create;
                  try
                     bmp.LoadFromStream(ss);
                     texExItem.Texture.Image.Assign(bmp);
                     texExItem.Texture.Enabled:=True;
                  finally
                     bmp.Free;
                     ss.Free;
                  end;
               end;
               texExItem.TextureIndex:=ReadInteger;
               Read(texExItem.TextureOffset.AsAddress^, SizeOf(Single)*3);
               Read(texExItem.TextureScale.AsAddress^, SizeOf(Single)*3);
            end;
         end;
      end;
   end else RaiseFilerException(Self.ClassType, archiveVersion);
end;

// SaveToStream
//
procedure TGLMaterialLibrary.SaveToStream(aStream : TStream);
var
   wr : TBinaryWriter;
begin
   wr:=TBinaryWriter.Create(aStream);
   try
      Self.WriteToFiler(wr);
   finally
      wr.Free;
   end;
end;

// LoadFromStream
//
procedure TGLMaterialLibrary.LoadFromStream(aStream : TStream);
var
   rd : TBinaryReader;
begin
   rd:=TBinaryReader.Create(aStream);
   try
      Self.ReadFromFiler(rd);
   finally
      rd.Free;
   end;
end;

// AddMaterialsFromStream
//
procedure TGLMaterialLibrary.AddMaterialsFromStream(aStream : TStream);
begin
   FDoNotClearMaterialsOnLoad:=True;
   try
      LoadFromStream(aStream);
   finally
      FDoNotClearMaterialsOnLoad:=False;
   end;
end;

// SaveToFile
//
procedure TGLMaterialLibrary.SaveToFile(const fileName : String);
var
   fs : TStream;
begin
   fs:=CreateFileStream(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TGLMaterialLibrary.LoadFromFile(const fileName : String);
var
   fs : TStream;
begin
   fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// AddMaterialsFromFile
//
procedure TGLMaterialLibrary.AddMaterialsFromFile(const fileName : String);
var
   fs : TStream;
begin
   fs:=CreateFileStream(fileName, fmOpenRead+fmShareDenyNone);
   try
      AddMaterialsFromStream(fs);
   finally
      fs.Free;
   end;
end;

// AddTextureMaterial
//
function TGLMaterialLibrary.AddTextureMaterial(const materialName, fileName : String;
                                               persistent : Boolean = True) : TGLLibMaterial;
begin
   Result:=Materials.Add;
   with Result do begin
      Name:=materialName;
      with Material.Texture do begin
         MinFilter:=miLinearMipmapLinear;
         MagFilter:=maLinear;
         TextureMode:=tmModulate;
         Disabled:=False;
         if persistent then begin
            ImageClassName:=TGLPersistentImage.ClassName;
            if fileName<>'' then
               Image.LoadFromFile(fileName);
         end else begin
            ImageClassName:=TGLPicFileImage.ClassName;
            TGLPicFileImage(Image).PictureFileName:=fileName;
         end;
      end;
   end;
end;

// AddTextureMaterial
//
function TGLMaterialLibrary.AddTextureMaterial(const materialName : String; graphic : TGLGraphic) : TGLLibMaterial;
begin
   Result:=Materials.Add;
   with Result do begin
      Name:=materialName;
      with Material.Texture do begin
         MinFilter:=miLinearMipmapLinear;
         MagFilter:=maLinear;
         TextureMode:=tmModulate;
         Disabled:=False;
         Image.Assign(graphic);
      end;
   end;
end;

// LibMaterialByName
//
function TGLMaterialLibrary.LibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;
begin
   if Assigned(Self) then
      Result:=Materials.GetLibMaterialByName(name)
   else Result:=nil;
end;

// ApplyMaterial
//
function TGLMaterialLibrary.ApplyMaterial(const materialName : String; var rci : TRenderContextInfo) : Boolean;
begin
   FLastAppliedMaterial:=Materials.GetLibMaterialByName(materialName);
   Result:=Assigned(FLastAppliedMaterial);
   if Result then
      FLastAppliedMaterial.Apply(rci);
end;

// UnApplyMaterial
//
function TGLMaterialLibrary.UnApplyMaterial(var rci : TRenderContextInfo) : Boolean;
begin
   if Assigned(FLastAppliedMaterial) then begin
      Result:=FLastAppliedMaterial.UnApply(rci);
      if not Result then
         FLastAppliedMaterial:=nil;
   end else Result:=False;
end;

// ------------------
// ------------------ TGLColorManager ------------------
// ------------------

// Find Color
//
function TGLColorManager.FindColor(const aName: String): TColorVector;
var
   i : Integer;
begin
   Result:=clrBlack;
   for i:=0 to Count-1 do
      if CompareText(TColorEntry(Items[i]^).Name, AName)=0 then begin
         SetVector(Result, TColorEntry(Items[i]^).Color);
         Break;
      end;
end;

// GetColor
//
function TGLColorManager.GetColor(const aName : String): TColorVector;
var
   workCopy  : String;
   delimiter : Integer;
begin
   if aName='' then
      Result:=clrBlack
   else begin
      workCopy:=Trim(AName);
      if AName[1] in ['(','[','<'] then
         workCopy:=Copy(workCopy, 2, Length(AName)-2);
      if CompareText(Copy(workCopy,1,3),'clr')=0 then
         SetVector(Result, FindColor(workCopy))
      else try
         // initialize result
         Result:=clrBlack;
         workCopy:=Trim(workCopy);
         delimiter:=Pos(' ', workCopy);
         if (Length(workCopy)>0) and (delimiter>0) then begin
            Result[0]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
            System.Delete(workCopy, 1, delimiter);
            workCopy:=TrimLeft(workCopy);
            delimiter:=Pos(' ',workCopy);
            if (Length(workCopy)>0) and (delimiter>0) then begin
               Result[1]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
               System.Delete(workCopy, 1, delimiter);
               workCopy:=TrimLeft(workCopy);
               delimiter:=Pos(' ', workCopy);
               if (Length(workCopy)>0) and (delimiter>0) then begin
                  Result[2]:=StrToFloat(Copy(workCopy, 1, delimiter-1));
                  System.Delete(workCopy, 1, delimiter);
                  workCopy:=TrimLeft(workCopy);
                  Result[3]:=StrToFloat(workCopy);
               end else Result[2]:=StrToFloat(workCopy);
            end else Result[1]:=StrToFloat(workCopy);
         end else Result[0]:=StrToFloat(workCopy);
      except
         InformationDlg('Wrong vector format. Use: ''<red green blue alpha>''!');
         Abort;
      end;
   end;
end;

//------------------------------------------------------------------------------

function TGLColorManager.GetColorName(const aColor: TColorVector): String;

const MinDiff = 1e-6;

var I : Integer;

begin
  for I:=0 to Count-1 do
    with TColorEntry(Items[I]^) do
      if (Abs(Color[0]-AColor[0]) < MinDiff) and
         (Abs(Color[1]-AColor[1]) < MinDiff) and
         (Abs(Color[2]-AColor[2]) < MinDiff) and
         (Abs(Color[3]-AColor[3]) < MinDiff) then Break;
  if I < Count then Result:=TColorEntry(Items[I]^).Name
               else
      Result:=Format('<%.3f %.3f %.3f %.3f>',[AColor[0],AColor[1],AColor[2],AColor[3]]);
end;

// Destroy
//
destructor TGLColorManager.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      FreeMem(Items[i], SizeOf(TColorEntry));
   inherited Destroy;
end;

// AddColor
//
procedure TGLColorManager.AddColor(const aName: String; const aColor: TColorVector);
var
   newEntry : PColorEntry;
begin
   New(newEntry);
   if newEntry = nil then
      raise Exception.Create('Could not allocate memory for color registration!');
   with newEntry^ do begin
     Name:=AName;
     SetVector(Color, aColor);
   end;
   Add(newEntry);
end;

// EnumColors
//
procedure TGLColorManager.EnumColors(Proc: TGetStrProc);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Proc(TColorEntry(Items[i]^).Name);
end;

// RegisterDefaultColors
//
procedure TGLColorManager.RegisterDefaultColors;
begin
   Capacity:=150;
   AddColor('clrTransparent',clrTransparent);
   AddColor('clrBlack',clrBlack);
   AddColor('clrGray05',clrGray05);
   AddColor('clrGray10',clrGray10);
   AddColor('clrGray15',clrGray15);
   AddColor('clrGray20',clrGray20);
   AddColor('clrGray25',clrGray25);
   AddColor('clrGray30',clrGray30);
   AddColor('clrGray35',clrGray35);
   AddColor('clrGray40',clrGray40);
   AddColor('clrGray45',clrGray45);
   AddColor('clrGray50',clrGray50);
   AddColor('clrGray55',clrGray55);
   AddColor('clrGray60',clrGray60);
   AddColor('clrGray65',clrGray65);
   AddColor('clrGray70',clrGray70);
   AddColor('clrGray75',clrGray75);
   AddColor('clrGray80',clrGray80);
   AddColor('clrGray85',clrGray85);
   AddColor('clrGray90',clrGray90);
   AddColor('clrGray95',clrGray95);
   AddColor('clrWhite',clrWhite);
   AddColor('clrDimGray',clrDimGray);
   AddColor('clrGray',clrGray);
   AddColor('clrLightGray',clrLightGray);
   AddColor('clrAquamarine',clrAquamarine);
   AddColor('clrBakersChoc',clrBakersChoc);
   AddColor('clrBlueViolet',clrBlueViolet);
   AddColor('clrBrass',clrBrass);
   AddColor('clrBrightGold',clrBrightGold);
   AddColor('clrBronze',clrBronze);
   AddColor('clrBronze2',clrBronze2);
   AddColor('clrBrown',clrBrown);
   AddColor('clrCadetBlue',clrCadetBlue);
   AddColor('clrCoolCopper',clrCoolCopper);
   AddColor('clrCopper',clrCopper);
   AddColor('clrCoral',clrCoral);
   AddColor('clrCornflowerBlue',clrCornflowerBlue);
   AddColor('clrDarkBrown',clrDarkBrown);
   AddColor('clrDarkGreen',clrDarkGreen);
   AddColor('clrDarkOliveGreen',clrDarkOliveGreen);
   AddColor('clrDarkOrchid',clrDarkOrchid);
   AddColor('clrDarkPurple',clrDarkPurple);
   AddColor('clrDarkSlateBlue',clrDarkSlateBlue);
   AddColor('clrDarkSlateGray',clrDarkSlateGray);
   AddColor('clrDarkSlateGrey',clrDarkSlateGrey);
   AddColor('clrDarkTan',clrDarkTan);
   AddColor('clrDarkTurquoise',clrDarkTurquoise);
   AddColor('clrDarkWood',clrDarkWood);
   AddColor('clrDkGreenCopper',clrDkGreenCopper);
   AddColor('clrDustyRose',clrDustyRose);
   AddColor('clrFeldspar',clrFeldspar);
   AddColor('clrFirebrick',clrFirebrick);
   AddColor('clrFlesh',clrFlesh);
   AddColor('clrForestGreen',clrForestGreen);
   AddColor('clrGold',clrGold);
   AddColor('clrGoldenrod',clrGoldenrod);
   AddColor('clrGreenCopper',clrGreenCopper);
   AddColor('clrGreenYellow',clrGreenYellow);
   AddColor('clrHuntersGreen',clrHuntersGreen);
   AddColor('clrIndian',clrIndian);
   AddColor('clrKhaki',clrKhaki);
   AddColor('clrLightBlue',clrLightBlue);
   AddColor('clrLightPurple',clrLightPurple);
   AddColor('clrLightSteelBlue',clrLightSteelBlue);
   AddColor('clrLightWood',clrLightWood);
   AddColor('clrLimeGreen',clrLimeGreen);
   AddColor('clrMandarinOrange',clrMandarinOrange);
   AddColor('clrMaroon',clrMaroon);
   AddColor('clrMediumAquamarine',clrMediumAquamarine);
   AddColor('clrMediumBlue',clrMediumBlue);
   AddColor('clrMediumForestGreen',clrMediumForestGreen);
   AddColor('clrMediumGoldenrod',clrMediumGoldenrod);
   AddColor('clrMediumOrchid',clrMediumOrchid);
   AddColor('clrMediumPurple',clrMediumPurple);
   AddColor('clrMediumSeaGreen',clrMediumSeaGreen);
   AddColor('clrMediumSlateBlue',clrMediumSlateBlue);
   AddColor('clrMediumSpringGreen',clrMediumSpringGreen);
   AddColor('clrMediumTurquoise',clrMediumTurquoise);
   AddColor('clrMediumViolet',clrMediumViolet);
   AddColor('clrMediumWood',clrMediumWood);
   AddColor('clrMidnightBlue',clrMidnightBlue);
   AddColor('clrNavy',clrNavy);
   AddColor('clrNavyBlue',clrNavyBlue);
   AddColor('clrNeonBlue',clrNeonBlue);
   AddColor('clrNeonPink',clrNeonPink);
   AddColor('clrNewMidnightBlue',clrNewMidnightBlue);
   AddColor('clrNewTan',clrNewTan);
   AddColor('clrOldGold',clrOldGold);
   AddColor('clrOrange',clrOrange);
   AddColor('clrOrangeRed',clrOrangeRed);
   AddColor('clrOrchid',clrOrchid);
   AddColor('clrPaleGreen',clrPaleGreen);
   AddColor('clrPink',clrPink);
   AddColor('clrPlum',clrPlum);
   AddColor('clrQuartz',clrQuartz);
   AddColor('clrRichBlue',clrRichBlue);
   AddColor('clrSalmon',clrSalmon);
   AddColor('clrScarlet',clrScarlet);
   AddColor('clrSeaGreen',clrSeaGreen);
   AddColor('clrSemiSweetChoc',clrSemiSweetChoc);
   AddColor('clrSienna',clrSienna);
   AddColor('clrSilver',clrSilver);
   AddColor('clrSkyBlue',clrSkyBlue);
   AddColor('clrSlateBlue',clrSlateBlue);
   AddColor('clrSpicyPink',clrSpicyPink);
   AddColor('clrSpringGreen',clrSpringGreen);
   AddColor('clrSteelBlue',clrSteelBlue);
   AddColor('clrSummerSky',clrSummerSky);
   AddColor('clrTan',clrTan);
   AddColor('clrThistle',clrThistle);
   AddColor('clrTurquoise',clrTurquoise);
   AddColor('clrViolet',clrViolet);
   AddColor('clrVioletRed',clrVioletRed);
   AddColor('clrVeryDarkBrown',clrVeryDarkBrown);
   AddColor('clrVeryLightPurple',clrVeryLightPurple);
   AddColor('clrWheat',clrWheat);
   AddColor('clrYellowGreen',clrYellowGreen);
   AddColor('clrGreen',clrGreen);
   AddColor('clrOlive',clrOlive);
   AddColor('clrPurple',clrPurple);
   AddColor('clrTeal',clrTeal);
   AddColor('clrRed',clrRed);
   AddColor('clrLime',clrLime);
   AddColor('clrYellow',clrYellow);
   AddColor('clrBlue',clrBlue);
   AddColor('clrFuchsia',clrFuchsia);
   AddColor('clrAqua',clrAqua);

   AddColor('clrScrollBar',clrScrollBar);
   AddColor('clrBackground',clrBackground);
   AddColor('clrActiveCaption',clrActiveCaption);
   AddColor('clrInactiveCaption',clrInactiveCaption);
   AddColor('clrMenu',clrMenu);
   AddColor('clrWindow',clrWindow);
   AddColor('clrWindowFrame',clrWindowFrame);
   AddColor('clrMenuText',clrMenuText);
   AddColor('clrWindowText',clrWindowText);
   AddColor('clrCaptionText',clrCaptionText);
   AddColor('clrActiveBorder',clrActiveBorder);
   AddColor('clrInactiveBorder',clrInactiveBorder);
   AddColor('clrAppWorkSpace',clrAppWorkSpace);
   AddColor('clrHighlight',clrHighlight);
   AddColor('clrHighlightText',clrHighlightText);
   AddColor('clrBtnFace',clrBtnFace);
   AddColor('clrBtnShadow',clrBtnShadow);
   AddColor('clrGrayText',clrGrayText);
   AddColor('clrBtnText',clrBtnText);
   AddColor('clrInactiveCaptionText',clrInactiveCaptionText);
   AddColor('clrBtnHighlight',clrBtnHighlight);
   AddColor('clr3DDkShadow',clr3DDkShadow);
   AddColor('clr3DLight',clr3DLight);
   AddColor('clrInfoText',clrInfoText);
   AddColor('clrInfoBk',clrInfoBk);
end;

// RemoveColor
//
procedure TGLColorManager.RemoveColor(const aName: String);
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      if CompareText(TColorEntry(Items[i]^).Name, aName)=0 then begin
         Delete(i);
         Break;
	   end;
   end;
end;

// ConvertWinColor
//
function ConvertWinColor(aColor : TColor; alpha : Single = 1) : TColorVector;
var
   winColor : Integer;
begin
	// Delphi color to Windows color
   winColor:=GLCrossPlatform.ColorToRGB(aColor);
   // convert 0..255 range into 0..1 range
   Result[0]:=(winColor and $FF)*(1/255);
   Result[1]:=((winColor shr 8) and $FF)*(1/255);
   Result[2]:=((winColor shr 16) and $FF)*(1/255);
   Result[3]:=alpha;
end;

// ConvertColorVector
//
function ConvertColorVector(const aColor : TColorVector) : TColor;
begin
   Result:=RGB(Round(255*aColor[0]),
               Round(255*aColor[1]),
               Round(255*aColor[2]));
end;

// ConvertColorVector
//
function ConvertColorVector(const aColor: TColorVector; intensity : Single) : TColor;
begin
   intensity:=255*intensity;
   Result:=RGB(Round(intensity*aColor[0]),
               Round(intensity*aColor[1]),
               Round(intensity*aColor[2]));
end;

// ConvertRGBColor
//
function ConvertRGBColor(const aColor : array of Byte): TColorVector;
var
   n : Integer;
begin
   // convert 0..255 range into 0..1 range
   n:=High(AColor);
   Result[0]:=AColor[0]*(1/255);
   if n>0 then
      Result[1]:=AColor[1]*(1/255)
   else Result[1]:=0;
   if n>1 then
      Result[2]:=AColor[2]*(1/255)
   else Result[2]:=0;
   if n>2 then
      Result[3]:=AColor[3]*(1/255)
   else Result[3]:=1;
end;

// RegisterColor
//
procedure RegisterColor(const aName : String; const aColor : TColorVector);
begin
   ColorManager.AddColor(AName, AColor);
end;

// UnregisterColor
//
procedure UnregisterColor(const aName : String);
begin
   ColorManager.RemoveColor(AName);
end;


// ------------------
// ------------------ TGLFloatDataImage ------------------
// ------------------

// Create
//
constructor TGLFloatDataImage.Create(AOwner: TPersistent);
begin
	inherited;
	FWidth:=256;
	FHeight:=256;
end;

// Destroy
//
destructor TGLFloatDataImage.Destroy;
begin
	ReleaseBitmap32;
	inherited Destroy;
end;

// Assign
//
procedure TGLFloatDataImage.Assign(Source: TPersistent);
begin
	if Assigned(Source) then begin
      if (Source is TGLFloatDataImage) then begin
         FWidth:=TGLFloatDataImage(Source).FWidth;
         FHeight:=TGLFloatDataImage(Source).FHeight;
         Invalidate;
      end else inherited;
	end else inherited;
end;

// SetWidth
//
procedure TGLFloatDataImage.SetWidth(val : Integer);
begin
   if val<>FWidth then begin
      FWidth:=val;
      if FWidth<1 then FWidth:=1;
      Invalidate;
   end;
end;

// GetWidth
//
function TGLFloatDataImage.GetWidth: Integer;
begin
	Result:=FWidth;
end;

// SetHeight
//
procedure TGLFloatDataImage.SetHeight(val : Integer);
begin
   if val<>FHeight then begin
      FHeight:=val;
      if FHeight<1 then FHeight:=1;
      Invalidate;
   end;
end;

// GetHeight
//
function TGLFloatDataImage.GetHeight: Integer;
begin
	Result:=FHeight;
end;

// GetBitmap32
//
function TGLFloatDataImage.GetBitmap32(target : TGLUInt) : TGLBitmap32;
begin
	if not Assigned(FBitmap) then begin
      FBitmap:=TGLBitmap32.Create;
      FBitmap.Width:=FWidth;
      FBitmap.Height:=FHeight;
	end;
	Result:=FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLFloatDataImage.ReleaseBitmap32;
begin
	if Assigned(FBitmap) then begin
   	FBitmap.Free;
		FBitmap:=nil;
	end;
end;

// SaveToFile
//
procedure TGLFloatDataImage.SaveToFile(const fileName : String);
begin
   SaveStringToFile(fileName, '[FloatDataImage]'#13#10'Width='+IntToStr(Width)
                              +#13#10'Height='+IntToStr(Height));
end;

// LoadFromFile
//
procedure TGLFloatDataImage.LoadFromFile(const fileName : String);
var
   sl : TStringList;
   buf : String;
begin
   buf:=fileName;
   if Assigned(FOnTextureNeeded) then
      FOnTextureNeeded(Self, buf);
   if FileExists(buf) then begin
      sl:=TStringList.Create;
      try
         sl.LoadFromFile(buf);
         FWidth:=StrToInt(sl.Values['Width']);
         FHeight:=StrToInt(sl.Values['Height']);
      finally
         sl.Free;
      end;
   end else begin
      Assert(False, Format(glsFailedOpenFile, [fileName]));
   end;
end;

// FriendlyName
//
class function TGLFloatDataImage.FriendlyName : String;
begin
   Result:='FloatData Image';
end;

// FriendlyDescription
//
class function TGLFloatDataImage.FriendlyDescription : String;
begin
   Result:='Image to be dynamically generated by OpenGL';
end;

class function TGLFloatDataImage.NativeTextureTarget: TGLUInt;
{ Note: We still use GL_TEXTURE_RECTANGLE_NV for GL_ATI_texture_float, so that
        the same Cg shaders (that use SampleRECT) would run with nv30 and nv40. }
begin
//   if GL_ATI_texture_float then
//     Result:=GL_TEXTURE_2D
//   else
     Result:=GL_TEXTURE_RECTANGLE_NV;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	InitWinColors;
	RegisterGLTextureImageClass(TGLBlankImage);
	RegisterGLTextureImageClass(TGLPersistentImage);
	RegisterGLTextureImageClass(TGLPicFileImage);
	RegisterGLTextureImageClass(TGLCubeMapImage);
	RegisterGLTextureImageClass(TGLFloatDataImage);
   RegisterClasses([TGLMaterialLibrary]);
   RegisterTGraphicClassFileExtension('.bmp', TGLBitmap);

finalization

	vColorManager.Free;
	vGLTextureImageClasses.Free;
   vGLTextureImageClasses:=nil;
   FreeAndNil(vTIEClass);
   FreeAndNil(vTIEEditor);

end.
