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

shadowCasters = DummycubeCreate(global.scene);

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

plane = ShadowplaneCreate(20, 20, 10, 10, shadowCasters, light, c_black, 0.5, global.scene);
ObjectPitch(plane, 90);
MaterialCreate("mFloor", "textures/ground.jpg");
MaterialSetShininess("mFloor", 16);
MaterialSetAmbientColor("mFloor", c_dkgray, 1);
MaterialSetDiffuseColor("mFloor", c_white, 1);
MaterialSetSpecularColor("mFloor", c_dkgray, 1);
ObjectSetMaterial(plane, "mFloor");

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
MaterialCreate("mHellknight", "diffuse.png");
MaterialSetAmbientColor("mHellknight", c_dkgray, 1); 
MaterialSetDiffuseColor("mHellknight", c_white, 1); 
MaterialSetSpecularColor("mHellknight", c_grey, 1); 
MaterialSetShininess("mHellknight", 8); 
MaterialCreate("mHellknightNormal", "normal.png");
ObjectSetMaterial(hk, "mHellknight");
shader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(shader, "mHellknight");
BumpShaderSetNormalTexture(shader, "mHellknightNormal");
MaterialSetShader("mHellknight", shader);

/*
vp1 = TextRead("shaders/simple-vp.glsl");
fp1 = TextRead("shaders/simple-fp.glsl");
simpleShader = GLSLShaderCreate(vp1, fp1);
paramColor = GLSLShaderCreateParameter(simpleShader, "color");
GLSLShaderSetParameter4f(paramColor, 1.0, 0.5, 0.0, 1.0);
MaterialSetShader("mHellknight", simpleShader);
*/

/*
MaterialLibrarySetTexturePaths(matlib2, "data/trinity");
MaterialLibraryActivate(matlib2);
trinity = ActorCreate("data/trinity/TRINITYrage.smd", matlib2, global.scene);
ActorAddObject(trinity, "data/trinity/walk.smd");
ActorAddObject(trinity, "data/trinity/run.smd");
ActorMakeSkeletalTranslationStatic(trinity, 1);
ActorMakeSkeletalTranslationStatic(trinity, 2);
ActorSwitchToAnimation(trinity, 1, false);
ObjectSetScale(trinity, 0.03, 0.03, 0.03);
ObjectSetPosition(trinity, 0, 0, 0);
ObjectPitch(trinity, 90);
*/

MaterialLibraryActivate(matlib);

MaterialCreate("mCloth", "data/cloth/tartan.jpg");
MaterialSetFaceCulling("mCloth", fcNoCull);
MaterialSetOptions("mCloth", true, true);
MaterialSetAmbientColor("mCloth", c_dkgray, 1);
MaterialSetDiffuseColor("mCloth", c_white, 1);
MaterialSetSpecularColor("mCloth", c_white, 1);
MaterialSetEmissionColor("mCloth", c_white, 1);

cloth = FreeformCreate("data/cloth/cloth.3ds", 0, 0, shadowCasters);
ObjectSetPosition(cloth, 5, 3, 0);
ObjectPitch(cloth, -90);
ObjectSetMaterial(cloth, "mCloth");

verlet = VerletWorldCreate(3, uspEveryFrame, 0.02);
VerletWorldSetMaxDeltaTime(verlet, 1.0 / 60.0);
//VerletWorldCreateOctree(verlet, -20, -5.5, -20, 20, 20, 20, 25, 5);
VerletWorldGravityCreate(verlet, 0, -9.81, 0);
VerletWorldSetSimTime(verlet, 0.0);

flr = VerletConstraintFloorCreate(verlet, 0, 0);
VerletConstraintSetFrictionRatio(flr, 1);
VerletConstraintSetPosition(flr, 0, 0.06, 0);

EdgeDetectorCreate(verlet, cloth);
VerletNodeNailedDown(verlet, 0, true);
VerletNodeNailedDown(verlet, 7, true);
VerletNodeNailedDown(verlet, 14, true);
numNodes = VerletGetNodeCount(verlet);
for (i = 0; i < numNodes; i += 1) {
    VerletNodeSetWeight(verlet, i, 0.1);
}

playerCollider = VerletConstraintCapsuleCreate(verlet, 1, 2);
VerletConstraintCapsuleSetAxis(playerCollider, 0, 1, 0);

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

alarm[0] = room_speed;
global.canRender = false;
