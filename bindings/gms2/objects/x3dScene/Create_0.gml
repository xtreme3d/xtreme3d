window_set_caption("Xtreme3D");
dll_init("xtreme3d.dll");
EngineCreate();

matlib = MaterialLibraryCreate();
MaterialLibraryActivate(matlib);

viewer = ViewerCreate(0, 0, window_get_width(), window_get_height(), PointerToReal(window_handle()));
ViewerSetBackgroundColor(viewer, c_gray);
ViewerSetLighting(viewer, true);
ViewerEnableFog(viewer, true);
ViewerSetFogColor(viewer, c_gray);
ViewerSetFogDistance(viewer, 100, 200);
ViewerSetAntiAliasing(viewer, csa8xHQ); //CSAA

global.back = DummycubeCreate(0);
global.scene = DummycubeCreate(0);
global.front = DummycubeCreate(0);

camPos = DummycubeCreate(global.scene);

camera = CameraCreate(camPos);
ViewerSetCamera(viewer, camera);
ObjectSetPosition(camPos, 0, 2, 3);
CameraSetViewDepth(camera, 500);
CameraSetFocal(camera, 100);
CameraSetNearPlaneBias(camera, 0.2);

light = LightCreate(lsOmni, global.scene);
LightSetAmbientColor(light, c_gray);
LightSetDiffuseColor(light, c_white);
LightSetSpecularColor(light, c_white);
ObjectSetPosition(light, 3, 5, 3);

plane = PlaneCreate(false, 20, 20, 5, 5, global.scene);
ObjectPitch(plane, 90);
MaterialCreate("mFloor", "textures/stone.png");
MaterialSetShininess("mFloor", 16);
MaterialSetAmbientColor("mFloor", c_dkgray, 1);
MaterialSetDiffuseColor("mFloor", c_white, 1);
MaterialSetSpecularColor("mFloor", c_dkgray, 1);
ObjectSetMaterial(plane, "mFloor");

teapot = TeapotCreate(global.scene);
ObjectScale(teapot, 3, 3, 3);
ObjectSetPosition(teapot, 0, 0, 0);
MaterialCreate("mTeapot", "textures/envmap.jpg");
MaterialSetFaceCulling("mTeapot", fcNoCull);
MaterialSetTextureMappingMode("mTeapot", tmmSphere);
MaterialSetOptions("mTeapot", false, true);
ObjectSetMaterial(teapot, "mTeapot");

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

alarm[0] = room_speed;
global.canRender = false;
