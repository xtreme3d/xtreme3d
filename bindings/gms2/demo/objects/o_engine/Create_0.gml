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
ViewerSetFogDistance(viewer, 50, 100);
ViewerSetAntiAliasing(viewer, aa2x); //MSAA

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

shadowObjects = DummycubeCreate(global.scene);

plane = ShadowplaneCreate(20, 20, 5, 5, shadowObjects, light, c_black, 0.5, global.scene);
ObjectPitch(plane, 90);
MaterialCreate("mFloor", "");
MaterialLoadTextureEx("mFloor", "textures/ground-diffuse.jpg", 0);
MaterialLoadTextureEx("mFloor", "textures/ground-normal.jpg", 1);
MaterialSetShininess("mFloor", 32);
MaterialSetAmbientColor("mFloor", c_black, 1);
MaterialSetDiffuseColor("mFloor", c_white, 1);
MaterialSetSpecularColor("mFloor", c_ltgray, 1);

bumpShader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShader, "");
BumpShaderSetNormalTexture(bumpShader, "");
BumpShaderSetMaxLights(bumpShader, 8);
BumpShaderUseAutoTangentSpace(bumpShader, true);
MaterialSetShader("mFloor", bumpShader);
ObjectSetMaterial(plane, "mFloor");

teapot = TeapotCreate(shadowObjects);
ObjectScale(teapot, 3, 3, 3);
ObjectSetPosition(teapot, 0, 1, 0);
MaterialCreate("mTeapot", "textures/envmap.jpg");
MaterialSetFaceCulling("mTeapot", fcNoCull);
MaterialSetTextureMappingMode("mTeapot", tmmSphere);
MaterialSetOptions("mTeapot", false, true);
ObjectSetMaterial(teapot, "mTeapot");

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2
display_mouse_set(mx, my);
MouseShowCursor(not mouselookActive);

alarm[0] = room_speed;
