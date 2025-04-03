window_set_caption("Xtreme3D 4 Demo");

// Initialize Xtreme3D
dll_init("xtreme3d.dll");

EngineCreate();
EngineShowLoadingErrors(true);
EngineSetCulling(vcNone);
EngineSetObjectsSorting(osNone);
EngineSetMaxLights(8);

// Create viewer in the game window
windowHandle = window_handle();
viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), PointerToReal(windowHandle));
ViewerSetBackgroundColor(viewer, c_gray);
ViewerSetLighting(viewer, true);
ViewerEnableFog(viewer, true);
ViewerSetFogColor(viewer, c_gray);
ViewerSetFogDistance(viewer, 0, 50);
ViewerSetAntiAliasing(viewer, csa8xHQ); //CSAA
ViewerEnableVSync(viewer, vsmNoSync); 
ViewerSetAutoRender(viewer, false);

// Create main material library
matlib = MaterialLibraryCreate();
MaterialLibraryActivate(matlib);

// Create Kraft physics engine instance
kraft = KraftCreate();

// Create root scene objects
global.preprocess = DummycubeCreate(0);
global.back = DummycubeCreate(0);
global.scene = DummycubeCreate(0);
global.front = DummycubeCreate(0);

raycastObjects = DummycubeCreate(global.scene);
shadowCasters = DummycubeCreate(raycastObjects);

// Create camera
camPos = DummycubeCreate(global.scene);
camera = CameraCreate(camPos);
ViewerSetCamera(viewer, camera);
ObjectSetPosition(camPos, 0, 2, 3);
CameraSetViewDepth(camera, 1000);
CameraSetFocal(camera, 100);
CameraSetNearPlaneBias(camera, 0.2);

// Create shadow map
shadowMapSize = 2048;
shadowFboMatlib = MaterialLibraryCreate();
MaterialLibraryActivate(shadowFboMatlib);
MaterialCreate("fboShadowDepth", "");
MaterialGenTexture("fboShadowDepth", shadowMapSize, shadowMapSize);
MaterialSetOptions("fboShadowDepth", true, true);
MaterialSetTextureFormat("fboShadowDepth", tfExtended);
MaterialSetTextureFormatEx("fboShadowDepth", tfDEPTH24_STENCIL8);
MaterialSetTextureWrap("fboShadowDepth", twSeparate);
MaterialSetTextureWrapS("fboShadowDepth", twClampToBorder);
MaterialSetTextureWrapT("fboShadowDepth", twClampToBorder);
MaterialSetTextureWrapR("fboShadowDepth", twClampToBorder);
MaterialSetTextureBorderColor("fboShadowDepth", c_white);
MaterialSetTextureFilter("fboShadowDepth", miLinear, maLinear);
MaterialSetTextureMode("fboShadowDepth", tmReplace);
MaterialSetTextureCompareMode("fboShadowDepth", tcmCompareRtoTexture);
MaterialSetTextureDepthCompareFunc("fboShadowDepth", cfLess);

shadowCamera = ShadowCameraCreate(global.scene);
ShadowCameraSetProjectionSize(shadowCamera, 20);
ShadowCameraSetZClippingPlanes(shadowCamera, -100, 100)
ObjectPitch(shadowCamera, -45);
ObjectSetPosition(shadowCamera, 0, 2, 0);

shadowFbo = FBOCreate(shadowMapSize, shadowMapSize, global.preprocess);
FBOSetMaterialLibrary(shadowFbo, shadowFboMatlib);
FBOSetCamera(shadowFbo, shadowCamera);
FBOSetAspect(shadowFbo, 1);
FBOSetRootObject(shadowFbo, shadowCasters);
FBOSetDepthTextureName(shadowFbo, "fboShadowDepth");
FBOSetTargetVisibility(shadowFbo, tvDefault);
FBOSetClearOptions(shadowFbo, true, true, true, true);
FBOSetShadowMapMode(shadowFbo, true);

shadowMap = ShadowMapCreate(shadowFbo, viewer, shadowCamera);

MaterialLibraryActivate(matlib);

// Create light
light1 = LightCreate(lsParallel, global.scene);
ObjectPitch(light1, -135);
LightSetAmbientColor(light1, c_black);
LightSetDiffuseColor(light1, c_white);
LightSetSpecularColor(light1, c_white);
ObjectSetPosition(light1, 3, 5, 3);

// Create shader and scene objects
bumpShader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShader, "");
BumpShaderSetNormalTexture(bumpShader, "");
BumpShaderSetMaxLights(bumpShader, 1);
BumpShaderSetShadowMap(bumpShader, shadowMap);
BumpShaderSetShadowBlurRadius(bumpShader, 3);
GLSLShaderForceDisableStencilTest(bumpShader, true);
GLSLShaderSetOptions(bumpShader, true, true);

MaterialCreate("mStone", "");
TextureExLoad(MaterialAddTextureEx("mStone", 0), "textures/stone.png");
TextureExLoad(MaterialAddTextureEx("mStone", 1), "textures/stone-normal.png");
MaterialSetAmbientColor("mStone", c_gray, 1);
MaterialSetDiffuseColor("mStone", c_white, 1);
MaterialSetSpecularColor("mStone", c_white, 1);
MaterialSetShininess("mStone", 8);
MaterialSetShader("mStone", bumpShader);

plane = PlaneCreate(0, 20, 20, 10, 10, raycastObjects);
ObjectPitch(plane, 90);
ObjectSetMaterial(plane, "mStone");

rbPlane = KraftCreateRigidBody(kraft, krbtStatic);
sPlane = KraftCreateShapePlane(rbPlane, 0, 1, 0, 0);
KraftRigidBodyFinish(rbPlane);

MaterialCreate("mCrate", "textures/crate.png")
MaterialSetAmbientColor("mCrate", c_ltgray, 1.0)

for (var i = 0; i < 5; i += 1)
{
    var cube = CubeCreate(1, 1, 1, raycastObjects);
    ObjectSetPosition(cube, -2, 2 + i * 1.8, i * 0.05);
    ObjectSetMaterial(cube, "mCrate");
    var rbCube = KraftCreateRigidBody(kraft, krbtDynamic);
    KraftObjectSetRigidBody(cube, rbCube);
    var sCube = KraftCreateShapeBox(rbCube, 0.5, 0.5, 0.5);
    KraftShapeSetDensity(sCube, 200.0);
    KraftRigidBodyFinish(rbCube);
}

matlib2 = MaterialLibraryCreate();

MaterialLibrarySetTexturePaths(matlib2, "data/hellknight");
MaterialLibraryActivate(matlib2);
hk = ActorCreate("data/hellknight/hellknight.md5mesh", matlib2, shadowCasters);
ActorAddObject(hk, "data/hellknight/idle.md5anim");
ActorAddObject(hk, "data/hellknight/attack.md5anim");
ObjectSetScale(hk, 0.02, 0.02, 0.02);
ObjectSetPosition(hk, 0, 0, 0);
MaterialCreate("mHellknight", "");
MaterialSetAmbientColor("mHellknight", c_gray, 1); 
MaterialSetDiffuseColor("mHellknight", c_white, 1); 
MaterialSetSpecularColor("mHellknight", c_white, 1); 
MaterialSetShininess("mHellknight", 64);
TextureExLoad(MaterialAddTextureEx("mHellknight", 0), "data/hellknight/diffuse.png");
TextureExLoad(MaterialAddTextureEx("mHellknight", 1), "data/hellknight/normal.png");
ObjectSetMaterial(hk, "mHellknight");
MaterialSetShader("mHellknight", bumpShader);
ObjectHide(hk);

hk2 = ActorProxyObjectCreate(hk, shadowCasters);
ObjectSetScale(hk2, 0.02, 0.02, 0.02);
ObjectSetPosition(hk2, 0, 0, 0);
ActorProxyObjectSwitchToAnimation(hk2, 0);
ActorProxyObjectSetInterval(hk2, 200);

hk3 = ActorProxyObjectCreate(hk, shadowCasters);
ObjectSetScale(hk3, 0.02, 0.02, 0.02);
ObjectSetPosition(hk3, 2, 0, 0);
ActorProxyObjectSwitchToAnimation(hk3, 1);
ActorProxyObjectSetInterval(hk3, 200);

MaterialLibraryActivate(matlib);

sphereMarker = SphereCreate(0.1, 8, 4, global.scene);
MaterialCreate("mRed", "");
MaterialSetAmbientColor("mRed", c_red, 1);
MaterialSetDiffuseColor("mRed", c_red, 1);
ObjectSetMaterial(sphereMarker, "mRed");

// Create HUD
ftfont = TTFontCreate("data/fonts/DroidSans.ttf", 20);
text = HUDTextCreate(ftfont, "FPS: 0", global.front);
HUDTextSetColor(text, c_white, 1.0);
ObjectSetPosition(text, 20, 20, 0);

MaterialCreate("mSprites", "textures/sprites.png");
MaterialSetOptions("mSprites", true, true);
MaterialSetTextureImageAlpha("mSprites", tiaDefault);
MaterialSetBlendingMode("mSprites", bmTransparency);
crosshair = HUDSpriteCreate("mSprites", 64, 64, global.front);
SpriteSetBounds(crosshair, 0, 0, 64, 64);
ObjectSetPosition(crosshair, window_get_width() / 2, window_get_height() / 2, 0);

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

application_surface_draw_enable(false);
game_set_speed(60, gamespeed_fps);

visible = false;
alarm[0] = room_speed;
