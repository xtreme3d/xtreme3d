window_set_caption("Xtreme3D");
dll_init("xtreme3d.dll");
EngineCreate();

matlib = MaterialLibraryCreate();
MaterialLibraryActivate(matlib);

viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), real(window_handle()));
ViewerSetBackgroundColor(viewer, c_gray);
ViewerSetLighting(viewer, true);
ViewerEnableFog(viewer, true);
ViewerSetFogColor(viewer, c_gray);
ViewerSetFogDistance(viewer, 100, 200);
ViewerSetAntiAliasing(viewer, aa4xHQ); //MSAA
ViewerSetAutoRender(viewer, false);

global.back = DummycubeCreate(0);
global.scene = DummycubeCreate(0);
global.front = DummycubeCreate(0);

camPos = DummycubeCreate(global.scene);

camera = CameraCreate(camPos);
ViewerSetCamera(viewer, camera);
ObjectSetPosition(camPos, 0, 3, 5);
CameraSetViewDepth(camera, 500);
CameraSetFocal(camera, 100);
CameraSetNearPlaneBias(camera, 0.2);

light = LightCreate(lsOmni, global.scene);
LightSetAmbientColor(light, c_gray);
LightSetDiffuseColor(light, c_white);
LightSetSpecularColor(light, c_white);
ObjectSetPosition(light, 3, 5, 3);

shadowCasters = DummycubeCreate(global.scene);

shadowCamera = CameraCreate(light);
CameraSetViewDepth(shadowCamera, 500);
target = DummycubeCreate(global.scene);
ObjectSetPosition(target, 0, 0, 0);
CameraSetTargetObject(shadowCamera, target);
    
shadowRes = 1024;
shadowMap = ShadowMapCreate(shadowRes, viewer, shadowCasters);
ShadowMapSetCamera(shadowMap, shadowCamera);
ShadowMapSetProjectionSize(shadowMap, 5);
ShadowMapSetZClippingPlanes(shadowMap, -10.0, 200.0);

plane = PlaneCreate(true, 20, 20, 5, 5, global.scene);
ObjectPitch(plane, 90);
MaterialCreate("mFloor", "textures/ground-diffuse.jpg");
MaterialLoadTextureEx("mFloor", "textures/ground-normal.jpg", 1);
MaterialSetShininess("mFloor", 16);
MaterialSetAmbientColor("mFloor", c_dkgray, 1);
MaterialSetDiffuseColor("mFloor", c_white, 1);
MaterialSetSpecularColor("mFloor", c_dkgray, 1);

bumpShader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShader, "");
BumpShaderSetNormalTexture(bumpShader, "");
BumpShaderSetMaxLights(bumpShader, 1);
BumpShaderUseAutoTangentSpace(bumpShader, true);
BumpShaderSetShadowMap(bumpShader, shadowMap);
BumpShaderSetShadowBlurRadius(bumpShader, 2);
MaterialSetShader("mFloor", bumpShader);
ObjectSetMaterial(plane, "mFloor");

teapot = TeapotCreate(shadowCasters);
ObjectScale(teapot, 3, 3, 3);
ObjectSetPosition(teapot, 0, 1, 0);
MaterialCreate("mTeapot", "textures/envmap.jpg");
MaterialSetFaceCulling("mTeapot", fcNoCull);
MaterialSetTextureMappingMode("mTeapot", tmmSphere);
MaterialSetOptions("mTeapot", false, true);
ObjectSetMaterial(teapot, "mTeapot");

ftfont = TTFontCreate("fonts/NotoSans-Regular.ttf", 20);
text = HUDTextCreate(ftfont, "Hello, World!\rПривет, мир!", global.front);
HUDTextSetColor(text, c_white, 1.0);
ObjectSetPosition(text, 16, 16, 0);

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2
display_mouse_set(mx, my);
MouseShowCursor(not mouselookActive);
