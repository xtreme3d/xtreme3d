window_set_caption("Xtreme3D");

dll_init("xtreme3d.dll");

EngineCreate();
EngineShowLoadingErrors(true);
EngineSetCulling(vcNone);
EngineSetObjectsSorting(osNone);

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

global.back = DummycubeCreate(0);
global.scene = DummycubeCreate(0);
global.front = DummycubeCreate(0);

DummycubeSetVisible(global.scene, true);

camPos = DummycubeCreate(global.scene);

camera = CameraCreate(camPos);
ViewerSetCamera(viewer, camera);
ObjectSetPosition(camPos, 0, 2, 3);
CameraSetViewDepth(camera, 1000);
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

/*
teapot = TeapotCreate(global.scene);
ObjectScale(teapot, 3, 3, 3);
ObjectSetPosition(teapot, 0, 0, 0);
MaterialCreate("mTeapot", "textures/envmap.jpg");
MaterialSetFaceCulling("mTeapot", fcNoCull);
MaterialSetTextureMappingMode("mTeapot", tmmSphere);
MaterialSetOptions("mTeapot", false, true);
ObjectSetMaterial(teapot, "mTeapot");
*/

//ffm = FreeformCreate("data/suzanne.obj", matlib, matlib, global.scene);
//FreeformGenTangents(ffm);
//FreeformUseMeshMaterials(ffm, false);
//ObjectSetMaterial(ffm, "mFloor");

matlib2 = MaterialLibraryCreate();
MaterialLibrarySetTexturePaths(matlib2, "data/trinity");
MaterialLibraryActivate(matlib2);
//hk = ActorCreate("mummy/mummy.md2", matlib2, global.scene);
actor = ActorCreate("data/trinity/trinity_mesh.smd", matlib2, global.scene);
ActorSetAnimationMode(actor, aamNone);
//show_message(string(ActorMeshObjectsCount(hk)));
//ActorSetAnimationRange(hk, 0, 1);
//ActorAddObject(hk, "data/hellknight/idle.md5anim");
ActorAddObject(actor, "data/trinity/trinity_walk.smd");
//ActorMakeSkeletalTranslationStatic(hk, true);
ActorSwitchToAnimation(actor, 0, false);
ObjectSetScale(actor, 0.05, 0.05, 0.05);
//ObjectSetScale(hk, 0.001, 0.001, 0.001);
//show_message(string(ObjectGetScale(hk, 0)));
//ObjectSetPosition(actor, 0, -1.2, 0);
ObjectPitch(actor, 90);
ObjectShowAxes(actor, true);
/*
MaterialCreate("mHellknight", "diffuse.png");
ObjectSetMaterial(hk, "mHellknight");
MaterialSetBlendingMode("mHellknight", bmOpaque);
*/
MaterialLibraryActivate(matlib);

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

alarm[0] = room_speed;
global.canRender = false;
