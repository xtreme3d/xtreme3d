window_set_caption("Xtreme3D");

dll_init("xtreme3d.dll");

EngineCreate();
EngineShowLoadingErrors(true);
EngineSetCulling(vcNone);
EngineSetObjectsSorting(osNone);
EngineSetMaxLights(8);

windowHandle = window_handle();
viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), PointerToReal(windowHandle));
ViewerSetBackgroundColor(viewer, c_gray);
ViewerSetLighting(viewer, true);
ViewerEnableFog(viewer, false);
ViewerSetFogColor(viewer, c_gray);
ViewerSetFogDistance(viewer, 100, 200);
ViewerSetAntiAliasing(viewer, csa8xHQ); //CSAA
ViewerEnableVSync(viewer, vsmNoSync);

matlib = MaterialLibraryCreate();
MaterialLibraryActivate(matlib);

global.preprocess = DummycubeCreate(0);
global.back = DummycubeCreate(0);
global.scene = DummycubeCreate(0);
global.front = DummycubeCreate(0);

shadowCasters = DummycubeCreate(global.scene);

camPos = DummycubeCreate(global.scene);

camera = CameraCreate(camPos);
ViewerSetCamera(viewer, camera);
ObjectSetPosition(camPos, 0, 2, 3);
CameraSetViewDepth(camera, 1000);
CameraSetFocal(camera, 100);
CameraSetNearPlaneBias(camera, 0.2);

shadowMapSize = 512;
shadowFboMatlib = MaterialLibraryCreate();
MaterialLibraryActivate(shadowFboMatlib);
MaterialCreate("fboShadowColor", "");
MaterialGenTexture("fboShadowColor", shadowMapSize, shadowMapSize);
MaterialSetOptions("fboShadowColor", true, true);
MaterialSetTextureWrap("fboShadowColor", false); 
MaterialSetTextureFilter("fboShadowColor", miLinear, maLinear);
MaterialCreate("fboShadowDepth", "");
MaterialGenTexture("fboShadowDepth", shadowMapSize, shadowMapSize);
MaterialSetOptions("fboShadowDepth", true, true);
MaterialSetTextureFormat("fboShadowDepth", tfExtended);
//MaterialSetTextureFormatEx("fboShadowDepth", 73); //tfDEPTH24_STENCIL8
MaterialSetTextureFormatEx("fboShadowDepth", 5); //tfDEPTH_COMPONENT24
MaterialSetTextureWrap("fboShadowDepth", false);
MaterialSetTextureFilter("fboShadowDepth", miLinear, maLinear);
MaterialSetTextureMode("fboShadowDepth", tmReplace);
MaterialSetTextureCompareMode("fboShadowDepth", 1); //tcmCompareRtoTexture

shadowCamera = ShadowCameraCreate(global.scene);
ObjectPitch(shadowCamera, -45);
ObjectSetPosition(shadowCamera, 0, 5, 5);
ObjectShowAxes(shadowCamera, true);

shadowFbo = FBOCreate(shadowMapSize, shadowMapSize, global.preprocess);
FBOSetMaterialLibrary(shadowFbo, shadowFboMatlib);
FBOSetCamera(shadowFbo, shadowCamera);
FBOSetAspect(shadowFbo, 1);
FBOSetRootObject(shadowFbo, shadowCasters);
FBOSetColorTextureName(shadowFbo, "fboShadowColor");
FBOSetDepthTextureName(shadowFbo, "fboShadowDepth");
FBOSetTargetVisibility(shadowFbo, 0);
FBOSetClearOptions(shadowFbo, true, true, true, true);
FBOSetStencilPrecision(shadowFbo, 3); //sp8bits

shadowMap = ShadowMapCreate(shadowFbo, viewer, shadowCamera);

shadowMapSprite = HUDSpriteCreate("fboShadowDepth", 256, 256, global.front); 
ObjectSetPosition(shadowMapSprite, 128, 128, 0);

MaterialLibraryActivate(matlib);

light1 = LightCreate(lsParallel, global.scene);
ObjectPitch(light1, -135);
LightSetAmbientColor(light1, c_black);
LightSetDiffuseColor(light1, c_white);
LightSetSpecularColor(light1, c_white);
ObjectSetPosition(light1, 3, 5, 3);

/*
bumpShaderPlane = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShaderPlane, "");
BumpShaderSetNormalTexture(bumpShaderPlane, "");
BumpShaderSetMaxLights(bumpShaderPlane, 8);
GLSLShaderForceDisableStencilTest(bumpShaderPlane, true);
*/

bumpShader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShader, "");
BumpShaderSetNormalTexture(bumpShader, "");
BumpShaderSetMaxLights(bumpShader, 1);
GLSLShaderForceDisableStencilTest(bumpShader, true);
BumpShaderSetShadowMap(bumpShader, shadowMap);
BumpShaderSetShadowBlurRadius(bumpShader, 2);

MaterialCreate("mStone", "");
TextureExLoad(MaterialAddTextureEx("mStone", 0), "textures/stone.png");
TextureExLoad(MaterialAddTextureEx("mStone", 1), "textures/stone-normal.png");
MaterialSetAmbientColor("mStone", c_black, 1);
MaterialSetDiffuseColor("mStone", c_white, 1);
MaterialSetSpecularColor("mStone", c_white, 1);
MaterialSetShininess("mStone", 8);
MaterialSetShader("mStone", bumpShader);

//plane = ShadowplaneCreate(20, 20, 10, 10, shadowCasters, light1, c_black, 0.5, global.scene);
//ShadowplaneSetOptions(plane, true, true, false, false);
plane = PlaneCreate(0, 20, 20, 10, 10, global.scene);
ObjectPitch(plane, 90);
ObjectSetMaterial(plane, "mStone");

matlib2 = MaterialLibraryCreate();

MaterialLibrarySetTexturePaths(matlib2, "data/hellknight");
MaterialLibraryActivate(matlib2);
hk = ActorCreate("data/hellknight/hellknight.md5mesh", matlib2, shadowCasters);
ActorAddObject(hk, "data/hellknight/idle.md5anim");
ActorAddObject(hk, "data/hellknight/attack.md5anim");
ActorSetAnimationMode(hk, aamLoop);
ActorSwitchToAnimation(hk, 0, true);
ObjectSetScale(hk, 0.02, 0.02, 0.02);
ObjectSetPosition(hk, 0, 0, 0);
MaterialCreate("mHellknight", "");
MaterialSetAmbientColor("mHellknight", c_black, 1); 
MaterialSetDiffuseColor("mHellknight", c_white, 1); 
MaterialSetSpecularColor("mHellknight", c_white, 1); 
MaterialSetShininess("mHellknight", 8);
TextureExLoad(MaterialAddTextureEx("mHellknight", 0), "data/hellknight/diffuse.png");
TextureExLoad(MaterialAddTextureEx("mHellknight", 1), "data/hellknight/normal.png");
ObjectSetMaterial(hk, "mHellknight");
MaterialSetShader("mHellknight", bumpShader);
//LightFXCreate(hk);

MaterialLibraryActivate(matlib);

/*
fboWidth = window_get_width();
fboHeight = window_get_height();
fboAspect = 1/1; //16/9 if using square texture
fboMatlib = MaterialLibraryCreate();
MaterialLibraryActivate(fboMatlib);
MaterialCreate("fboColor", "");
MaterialGenTexture("fboColor", fboWidth, fboHeight);
MaterialSetOptions("fboColor", false, false);
MaterialSetTextureWrap("fboColor", false); 
MaterialSetTextureFilter("fboColor", miLinear, maLinear);
MaterialCreate("fboDepth", "");
MaterialGenTexture("fboDepth", fboWidth, fboHeight);
MaterialSetOptions("fboDepth", false, false);
MaterialSetTextureFormat("fboDepth", tfExtended);
MaterialSetTextureFormatEx("fboDepth", 73); //tfDEPTH24_STENCIL8
MaterialSetTextureWrap("fboDepth", false);
MaterialSetTextureFilter("fboDepth", miNearest, maNearest);
MaterialSetTextureMode("fboDepth", tmReplace);

fbo = FBOCreate(fboWidth, fboHeight, global.scene);
FBOSetMaterialLibrary(fbo, fboMatlib);
FBOSetCamera(fbo, camera);
FBOSetAspect(fbo, fboAspect);
FBOSetRootObject(fbo, global.scene);
FBOSetColorTextureName(fbo, "fboColor");
FBOSetDepthTextureName(fbo, "fboDepth");
FBOSetTargetVisibility(fbo, 0);
FBOSetClearOptions(fbo, true, true, true, true);
FBOSetStencilPrecision(fbo, 3); //sp8bits

fxaa_vp1 = TextRead("shaders/fxaa-vp.glsl");
fxaa_fp1 = TextRead("shaders/fxaa-fp.glsl");
fxaaShader = GLSLShaderCreate(fxaa_vp1, fxaa_fp1);
fxaaParamTexture = GLSLShaderCreateParameter(fxaaShader, "colorBuffer");
GLSLShaderSetParameterFBOColorTexture(fxaaParamTexture, fbo, 0);
fxaaParamViewSize = GLSLShaderCreateParameter(fxaaShader, "viewSize");
GLSLShaderSetParameter2f(fxaaParamViewSize, window_get_width(), window_get_height());
MaterialCreate("mFXAA", "");
MaterialSetOptions("mFXAA", true, true);
MaterialSetTextureWrap("mFXAA", false);
MaterialSetShader("mFXAA", fxaaShader);

screenSprite = HUDSpriteCreate("mFXAA", window_get_width(), window_get_height(), global.front); 
ObjectSetPosition(screenSprite, window_get_width() / 2, window_get_height() / 2, 0);

MaterialLibraryActivate(matlib);
*/

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

visible = false;
alarm[0] = room_speed;
global.canRender = false;
