window_set_caption("Xtreme3D");

dll_init("xtreme3d.dll");

EngineCreate();
EngineShowLoadingErrors(true);
EngineSetCulling(vcNone);
EngineSetObjectsSorting(osNone);
EngineSetMaxLights(64);

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

bumpShader = BumpShaderCreate();
BumpShaderSetDiffuseTexture(bumpShader, "");
BumpShaderSetNormalTexture(bumpShader, "");
BumpShaderSetMaxLights(bumpShader, 8);

MaterialCreate("mStone", "");
TextureExLoad(MaterialAddTextureEx("mStone", 0), "textures/stone.png");
TextureExLoad(MaterialAddTextureEx("mStone", 1), "textures/stone-normal.png");
MaterialSetShininess("mStone", 8);
MaterialSetAmbientColor("mStone", c_dkgray, 1);
MaterialSetDiffuseColor("mStone", c_white, 1);
MaterialSetSpecularColor("mStone", c_dkgray, 1);
MaterialSetShader("mStone", bumpShader);

MaterialCreate("mFloor", "textures/ground.jpg");
MaterialSetShininess("mFloor", 8);
MaterialSetAmbientColor("mFloor", c_dkgray, 1);
MaterialSetDiffuseColor("mFloor", c_white, 1);
MaterialSetSpecularColor("mFloor", c_dkgray, 1);

/*
var gridSize, gx, gy, p, li, col;
gridSize = 4;
for (gy = -gridSize; gy < gridSize+1; gy = gy + 1)
{
	for (gx = -gridSize; gx < gridSize+1; gx = gx + 1)
	{
	    p = PlaneCreate(1, 3, 3, 1, 1, global.scene);
	    ObjectSetPosition(p, gx * 3, 0, gy * 3);
	    ObjectPitch(p, 90);
	    ObjectSetMaterial(p, "mStone");
	    fx = LightFXCreate(p);
                
	    li = LightCreate(lsOmni, global.scene);
	    col = MakeColorRGBFloat(random(256) / 255.0, random(256) / 255.0, random(256) / 255.0);
	    LightSetAmbientColor(li, c_black);
	    LightSetDiffuseColor(li, col);
	    LightSetSpecularColor(li, col);
	    LightSetAttenuation(li, 2.0, 0.0, 0.8);
	    ObjectSetPosition(li, gx * 3, 1, gy * 3);
	}
}

s1 = SphereCreate(0.5, 16, 8, camPos)
ObjectSetPosition(s1, 0, -1.8 + 0.5, -2)
ObjectSetMaterial(s1, "mStone")
fx = LightFXCreate(s1);
*/

plane = ShadowplaneCreate(20, 20, 10, 10, shadowCasters, light, c_black, 0.5, global.scene);
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
MaterialSetAmbientColor("mHellknight", c_dkgray, 1); 
MaterialSetDiffuseColor("mHellknight", c_white, 1); 
MaterialSetSpecularColor("mHellknight", c_grey, 1); 
MaterialSetShininess("mHellknight", 8);
tex0 = MaterialAddTextureEx("mHellknight", 0);
tex1 = MaterialAddTextureEx("mHellknight", 1);
TextureExLoad(tex0, "data/hellknight/diffuse.png");
TextureExLoad(tex1, "data/hellknight/normal.png");
ObjectSetMaterial(hk, "mHellknight");
MaterialSetShader("mHellknight", bumpShader);
LightFXCreate(hk);


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

/*
MaterialCreate("mCloth", "data/cloth/tartan.jpg");
MaterialSetFaceCulling("mCloth", fcNoCull);
MaterialSetOptions("mCloth", true, true);
MaterialSetAmbientColor("mCloth", c_dkgray, 1);
MaterialSetDiffuseColor("mCloth", c_white, 1);
MaterialSetSpecularColor("mCloth", c_white, 1);
MaterialSetEmissionColor("mCloth", c_white, 1);
*/

/*
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
*/

fboCamera = CameraCreate(global.scene);
ObjectSetPosition(fboCamera, 0, 0, 0);

fboRootObject = DummycubeCreate(0);

fboWidth = window_get_width();
fboHeight = window_get_height();
fboAspect = 1/1; //16/9;
matlib3 = MaterialLibraryCreate();
MaterialLibraryActivate(matlib3);
MaterialCreate("fboColor", "");
MaterialGenTexture("fboColor", fboWidth, fboHeight);
MaterialSetOptions("fboColor", false, false);
MaterialSetTextureWrap("fboColor", false); 
MaterialCreate("fboDepth", "");
MaterialGenTexture("fboDepth", fboWidth, fboHeight);
MaterialSetOptions("fboDepth", false, false);
MaterialSetTextureWrap("fboDepth", false); 

fbo = FBOCreate(fboWidth, fboHeight, global.scene);
FBOSetMaterialLibrary(fbo, matlib3);
FBOSetCamera(fbo, camera);
FBOSetAspect(fbo, fboAspect);
//FBOSetEnabledRenderBuffers(fbo, true, true);
//FBOSetClearOptions(fbo, true, true, true, true);
FBOSetStencilPrecision(fbo, 3);
FBOSetRootObject(fbo, global.scene);
FBOSetColorTextureName(fbo, "fboColor");
FBOSetDepthTextureName(fbo, "fboDepth");
FBOSetTargetVisibility(fbo, 0);

vp1 = TextRead("shaders/fbo-vp.glsl");
fp1 = TextRead("shaders/fbo-fp.glsl");
simpleShader = GLSLShaderCreate(vp1, fp1);
paramTexture = GLSLShaderCreateParameter(simpleShader, "fboColor");
GLSLShaderSetParameterFBOColorTexture(paramTexture, fbo, 0);
MaterialCreate("mSimple", "");
MaterialSetOptions("mSimple", true, true);
MaterialSetTextureWrap("mSimple", false);
MaterialSetShader("mSimple", simpleShader);

spr = HUDSpriteCreate("mSimple", window_get_width(), window_get_height(), global.front); 
ObjectSetPosition(spr, window_get_width() / 2, window_get_height() / 2, 0);

MaterialLibraryActivate(matlib);

mouselookActive = true;
mb_left_released = true;
mx = display_get_width() / 2;
my = display_get_height() / 2;
MouseSetPosition(mx, my);
MouseShowCursor(not mouselookActive);

visible = false;
alarm[0] = room_speed;
global.canRender = false;
