Xtreme3D version history
========================
v4.0.0 (by Gecko) - ??/??/????
------------------------------
- Uses GLScene 2.1 and Delphi 10.4
- Xtreme3D is now 64 bit and compatible with recent versions of GameMaker Studio 2
- UTF-8 support for file names
- `Update` is deprecated, use `EngineUpdate` instead
- `TrisRendered` was removed
- `ViewerGetPickedObjectsList` now requires user-provied PickList
- New functions `PickListCreate`, `PickListClear`, `PickListGetCount`, `PickListGetHit`
- New constants for `ViewerSetAntiAliasing`: `aa6x = 6`, `aa8x = 7`, `aa16x = 8`, `csa8x = 9`, `csa8xHQ = 10`, `csa16x = 11`, `csa16xHQ = 12`
- `CameraZoomAll` now requires a Viewer as a second parameter
- `SpriteNoZWrite` was removed, use `MaterialSetDepthWrite` instead
- `MaterialCullFrontFaces` was removed
- `MaterialSetZWrite` is deprecated, use `MaterialSetDepthWrite` instead
- New function `MaterialSetDepthTest`
- `PtrToReal` is deprecated, use `PointerToReal` instead
- Polygon functions were removed
- `TextRead`, `TextConvertANSIToUTF8` were removed
- `OdeAddCone` was removed
- New function `BaseMeshBuildSilhouetteConnectivityData`
- New functions `ObjectNotifyChange`, `ObjectStructureChanged`, `ObjectClearStructureChanged`.

v3.9.2 (by Gecko) - 09/04/2022
------------------------------
- New function `PtrToReal` for compatibility with latest GameMaker Studio 2
- `MouseIsPressed`
- Fixed shadow map rendering when using with dynamic cubemaps
- BumpShader now uses 8 light sources and auto tangent space by default. 

v3.9.1 (by Gecko) - 20/09/2021
------------------------------
- Exported functions now use cdecl convention instead of stdcall
- ViewerCreate checks parent window handle and shows error message if the window is not valid
- GameMaker Studio 2 support added.

v3.9.0 (by Gecko) - 20/05/2021
--------------------------------
- EngineGetTimeStep 
- ObjectIgnoreDepthBuffer, ObjectIsPicked 
- WindowDispatch 
- Invisible objects are now ignored in raycast queries
- Missing textures are now ignored at model loading
- Kraft physics engine updated to latest version
- Lua binding added (uses LuaJIT).

v3.8.0 (by FireRun) - 27/07/2019
--------------------------------
- Verlet physics engine for soft bodies simulation
- Pipe primitive
- TilePlane primitive
- ViewerPixelRayToWorld, ViewerShadeModel, ViewerRenderToFilePNG
- ActorGetAnimationName, ActorGetAnimationCount, ActorAnimationDestroy, ActorAnimationNextFrame, ActorAnimationPrevFrame, ActorTriangleCount, ActorSetFrame
- MovementPathShow, MovementPathSetLoop, MovementPathDeleteNode
- HUDSpriteXTiles, HUDSpriteYTiles
- FireFXRingExplosion
- FreeformMeshObjectGetName, FreeformMeshObjectSetName, FreeformMeshObjectDestroy.

v3.7.2 (by Gecko) - 26/02/2019
------------------------------
- SpriteGetSize.
- MaterialDestroy, MaterialSetName.
- TextureEx now supports up to 16 texture units instead of 8.
- MaterialSetShader now allows to disable a shader by passing zero pointer to it: `MaterialSetShader('material', 0)`.
- LineSetNode.

v3.7.1 (by FireRun) - 05/08/2018
--------------------------------
- LightGetColor, LightGetShining, LightGetAttenuation
- SphereOptions, SphereGetOptions, CylinderOptions, CylinderGetOptions, etc.
- ObjectGetMaterial
- MaterialGetColor, MaterialGetAlpha
- GridSetTile, GridSetStep.

v3.7.0 (by Gecko) - 21/07/2018
------------------------------
- LightFX system that allows to use more than 8 light sources in a scene. Lights are sorted per object based on distance, and then 8 nearest lights are used to render an object. lsParallel lights have higher priority over lsOmni and lsSpot lights. The system is compatible with built-in shaders. To use the system, a LightFX effect should be applied to an object (LightFXCreate function). It works for object's children as well
- Total number of OpenGL lights can now be limited with EngineSetMaxLights function. This is necessary to avoid overhead when using more than 8 lights. If this number is set to 0, behaviour is the same as before: Xtreme3D uses maximum number of lights supported by the hardware. By default this number is 8, so Xtreme3D by default will use 8 lights even if the hardware supports more. This limitation does not affect LightFX, only default 8-light system
- Initial Kraft physics engine integration. Kraft is an open source Object Pascal physics library for 3D games written by Benjamin 'BeRo' Rosseaux. It supports rigid body dynamics, all basic shapes (sphere, box, capsule, convex hull, static trimesh) and joints (grab, distance, rope, ball-socket, hinge, slider, fixed). The engine is fast, robust and game-friendly - you can implement a plausible character controller on top of Kraft with relative ease, and it will work better than with ODE. Kraft is fully integrated into xtreme3d.dll and doesn't need any additional libraries. ODE is still available, but in future it can be deprecated
- Real-time terrain editing. The following functions have been added: BmpHDSCreateEmpty, BmpHDSSetHeight, BmpHDSGetHeight, BmpHDSSave, TerrainGetHDSPosition
- Basic FBX model format support (only for Freeforms for now, and without materials). Only binary FBX is supported. The loader is based on OpenFBX library, so OpenFBX.dll is required to use it (as always with third-party DLLs, this dependency is optional). You can find its sources at https://github.com/xtreme3d/openfbx-dll
- New ActorProxyObject functions: ActorProxyObjectSetAnimationRange, ActorProxyObjectSetInterval
- Functions for reading and extracting PAK files: PakGetFileCount, PakGetFileName, PakExtract, PakExtractFile. SetPakArchive now returns PAK file id that is used to access it. LZRW1-compressed PAK files are now supported.
- New HUDSprite function: HUDSpriteGetMouseOver
- New Object function: ObjectGetScale
- ObjectDestroy now works for all Xtreme3D objects
- New Window functions: WindowSetIcon, WindowIsShowing
- New Viewer function: ViewerResetPerformanceMonitor
- Now engine doesn't crash if a resource (Freeform, Actor or texture) fails to load. Error message is now shown instead, and the game continues. Error message showing can be switched with EngineShowLoadingErrors function
- Thanks to Bill Collins, English translation of a help file have been created. Currently it's a mere machine translation from Russian, so we need volunteers to improve it manually - pull requests are welcome. You can also send your translations to Gecko: gecko0307@gmail.com
- PakEdit utility now supports LZRW1-compressed PAK files.

v3.6.0 (by Gecko) - 17/02/2017
------------------------------
- Windows/ANSI encoding support for TTFont (TTFontSetEncoding)
- New Material functions: MaterialSetTextureExFromLibrary, MaterialGetNameFromLibrary 
- New Freeform functions: FreeformSetMaterialLibraries, FreeformMeshFaceGroupSetLightmapIndex, FreeformMeshFaceGroupGetLightmapIndex 
- ActorProxy objects
- New Actor functions: ActorMoveBone, ActorRotateBone, ActorMeshSetVisible 
- New Object function: ObjectInFrustrum 
- Mouse and keyboard input functions
- Window creation and management functions
- RGB color packing functions
- Saving and loading scene files (experimental): EngineSaveScene, EngineLoadScene
- GLSLShader doesn't show empty error message if there are no errors 
- Fixed specular highlights in PhongShader and BumpShader
- Python and D language bindings added

v3.5.0 (by Gecko) - 04/02/2017
------------------------------
* ClipPlane functions
* Spot lights, fog, transparency and shadeless rendering support for BumpShader and PhongShader
* New Material functions: MaterialSetZWrite, MaterialCullFrontFaces
* New ODE functions: OdeDynamicSetVelocity, OdeDynamicSetAngularVelocity, OdeDynamicGetVelocity, OdeDynamicGetAngularVelocity, OdeDynamicSetPosition, OdeDynamicSetRotationQuaternion

v3.4.0 (by Gecko) - 01/01/2017
------------------------------
* TTF fonts and Unicode text support
* ObjectHash functions
* Improved FBORenderObjectEx
* FreeformGenTangents and FreeformBuildOctree
* ViewMatrix and InvViewMatrix parameters for GLSLShader
* Added GLSLShaderSetParameterHasTextureEx
* Seamless cubemapping in shaders
* Fixed MaterialCubeMapLoadImage

v3.3.0 (by Gecko) - 27/11/2016
------------------------------
* Querying and modifying geometry data of Freeforms
* Getting size and position of Viewer
* Override materials for Viewer and FBO
* More color formats for FBO
* Rendering ShadowMap shadows into custom FBO
* Querying supported OpenGL extensions

v3.2.0 (by Gecko) - 21/10/2016
------------------------------
* FBO support for fast offscreen rendering and Z-buffer access in shaders
* Rendering separate objects and hierarchies - essential for multipass rendering
* New multitexture mechanism for Materials (8 texture slots)

v3.1.0 (by Gecko) - 30/09/2016
------------------------------
* Improved Freeform API, added an ability to manually construct Freeforms from vertices and triangles. Freeforms now can be saved to file (GLSM, OBJ, STL, or NMF)
* New file formats support: CSM and LMTS (these were absent in 3.0), X, ASE, DXS (experimental, without lightmaps for now)
* Ragdoll support in ODE
* Added Movement object that can be used to define smooth interpolated movement paths for objects
* Improved BumpShader, added shadows and automatic tangent space support
* Improved PhongShader, added diffuse texture support
* HUDShapes: rectangle, circle, line, and custom 2D mesh
* Improved Sprite API, added texture atlases support
* Functions to query Material's texture size
* Alpha channel support for PNG

v3.0.0 (by Gecko, Rutraple aka Hacker, Ghost) - 25/08/2016
----------------------------------------------------------
The library was reimplemented from scratch because of lacking source code for original Xtreme3D from Xception.

* GLSL shaders support
* Shadow maps
* Antialiasing for Viewer (2x, 4x, NVIDIA Quincunx support)
* MemoryViewer for fast offscreen rendering
* Proxy and MultiProxy objects
* New file formats support: LOD, B3D, MDC, WRL 
* Tag support for MD3 models
* Animation blending for Actors
* Object-to-bone parenting
* Material scripts support
* Improved BumpShader (rewrote shader in GLSL, added specular shading and parallax mapping)
* TexCombineShader
* PhongShader
* Perlin noise procedural textures
* Linear waves for Water object
* New geometric promitives - frustum, dodecahedron, icosahedron, teapot
* Improved mesh explosion effect, added an ability to reset mesh to its normal state
* Grid rendering
* Debug rendering of Dummycubes
* New Camera functions
* An ability to copy object matrices, and also manually set them
* Many improvements in DCE, added terrain support, gravity and impulses for dynamic objects
* Working Freeforms and Terrains in ODE, an ability to set local positions for geometries
* Saving renders to BMP
* Text reading function

v2.0.2.0 (by Xception) - 16/02/2007
-----------------------------------
* New ODE functions
* New Actor functions
* Dynamic Collision Engine (DCE)
* CLOD terrain

v2.0.0.0 (by Xception) - 11/02/2007
-----------------------------------
* Model/Scene loaders with lightmaps:
  * Blitz3D scene loader(with hierarchy) and special portal/zone culling functions
  * Cartography Shop CSM loader
  * Pulsar LM Tools LMTS loader
  * DeleD DMF loader partially: no lightmaps, only triangles and quads supported 
* Function to load lightmap coordinates from a second (lightmapped) model
* Function to split a freeform's internal meshes into single freeforms
* ODE Physics engine support 
* Dynamic Quadtree and Octree for visibility culling and collision checking
* Static and dynamic cubemaps
* Render to texture
* Loading resources from compressed PAK archives
* Bump-/Normalmapping Shader
* Celshader
* HiddenlineShader
* Multimaterialshader
* Multitexturing
* Texture format selection
* Texture compression selection
* Texture filtering quality selection
* DDS (DXT1, DXT3, DXT5) texture support
* Actor bone manipulation functions
* Multiple viewers and material libraries useable

