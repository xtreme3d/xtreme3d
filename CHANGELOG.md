Xtreme3D version history
========================
v3.6.0 (by Gecko) - ??/??/????
------------------------------
* Lua scripting
* Initial Squall audio engine integration
* Windows codepages for TTF fonts
* New Material functions: MaterialSetTextureExFromLibrary, MaterialGetNameFromLibrary
* New Freeform functions: FreeformMeshFaceGroupSetLightmapIndex, FreeformMeshFaceGroupGetLightmapIndex, FreeformSetMaterialLibraries
* ActorProxy functions
* New Actor functions: ActorMoveBone, ActorRotateBone, ActorMeshSetVisible
* Mouse and keyboard input
* Window creation and management
* Color construction functions: MakeColorRGB, MakeColorRGBFloat
* New Object function: ObjectInFrustum
* New Engine functions: EngineSaveScene, EngineLoadScene (experimental, doesn't support all objects)

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

