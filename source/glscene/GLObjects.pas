//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLObjects<p>

   Implementation of basic scene objects plus some management routines.<p>

   All objects declared in this unit are part of the basic GLScene package,
   these are only simple objects and should be kept simple and lightweight.<br>

   More complex or more specialized versions should be placed in dedicated
   units where they can grow and prosper untammed. "Generic" geometrical
   objects can be found GLGeomObjects.<p>

	<b>History : </b><font size=-1><ul>
      <li>08/10/05 - Mathx - Fixed TGLLines.nodes.assign problem (thanks to  Yong Yoon Kit);
                             Also fixed a TGLLineBase.assign problem (object being assigned to
                             was refering the base lists, not copying them). 
                             Bugtracker ID=830846
      <li>17/01/05 - SG - Added color support for bezier style TGLLines
      <li>03/12/04 - MF - Added TGLSprite.AxisAlignedDimensionsUnscaled override
      <li>06/07/04 - SG - TGLCube.RayCastIntersect fix (Eric Pascual)
      <li>20/01/04 - SG - Added IcosahedronBuildList
      <li>30/11/03 - MF - Added TGLSphere.GenerateSilhouette - it now takes the
                          stacks/slices of the sphere into account 
      <li>10/09/03 - EG - Introduced TGLNodedLines
      <li>18/08/03 - SG - Added MirrorU and MirrorV to TGLSprite for mirroring textures
      <li>21/07/03 - EG - TGLTeapot moved to new GLTeapot unit,
                          TGLDodecahedron moved to new GLPolyhedron unit,
                          TGLCylinder, TGLCone, TGLTorus, TGLDisk, TGLArrowLine,
                          TGLAnnulus, TGLFrustrum and TGLPolygon moved to new
                          GLGeomObjects unit
      <li>16/07/03 - EG - Style changes and cleanups
      <li>19/06/03 - MF - Added GenerateSilhouette to TGLCube and TGLPlane.
      <li>13/06/03 - EG - Fixed TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
      <li>03/06/03 - EG - Added TGLAnnulus.RayCastIntersect (Alexandre Hirzel)
      <li>01/05/03 - SG - Added NURBS Curve to TGLLines (color not supported yet)
      <li>14/04/03 - SG - Added a Simple Bezier Spline to TGLLines (color not supported yet)
      <li>02/04/03 - EG - TGLPlane.RayCastIntersect fix (Erick Schuitema)
      <li>13/02/03 - DanB - added AxisAlignedDimensionsUnscaled functions
      <li>22/01/03 - EG - TGLCube.RayCastIntersect fixes (Dan Bartlett)
      <li>10/01/03 - EG - TGLCube.RayCastIntersect (Stuart Gooding)
      <li>08/01/03 - RC - Added TGLPlane.XScope and YScope, to use just a part of the texture
      <li>27/09/02 - EG - Added TGLPointParameters
      <li>24/07/02 - EG - Added TGLCylinder.Alignment
      <li>23/07/02 - EG - Added TGLPoints (experimental)
      <li>20/07/02 - EG - TGLCylinder.RayCastIntersect and TGLPlane.RayCastIntersect
      <li>18/07/02 - EG - Added TGLCylinder.Align methods
      <li>07/07/02 - EG - Added TGLPlane.Style
      <li>03/07/02 - EG - TGLPolygon now properly setups normals (filippo)
      <li>17/03/02 - EG - Support for transparent lines
      <li>02/02/02 - EG - Fixed TGLSprite change notification
      <li>26/01/02 - EG - TGLPlane & TGLCube now osDirectDraw
      <li>20/01/02 - EG - TGLSpaceText moved to GLSpaceText
      <li>22/08/01 - EG - TGLTorus.RayCastIntersect fixes
      <li>30/07/01 - EG - Updated AxisAlignedDimensions implems
      <li>16/03/01 - EG - TGLCylinderBase, changed default Stacks from 8 to 4
      <li>27/02/01 - EG - Fix in TGLCube texcoords, added TGLFrustrum (thx Robin Gerrets)
      <li>22/02/01 - EG - Added AxisAlignedDimensions overrides by Uwe Raabe
      <li>05/02/01 - EG - Minor changes to TGLCube.BuildList
      <li>21/01/01 - EG - BaseProjectionMatrix fix for TGLHUDSprite (picking issue),
                          TGLHUDSprite moved to GLHUDObjects
      <li>14/01/01 - EG - Fixed TGLSphere texture coordinates
      <li>13/01/01 - EG - TGLSprite matrix compatibility update
      <li>09/01/01 - EG - TGLSpaceText now handles its TFont.OnFontChange
      <li>08/01/01 - EG - Added TGLLinesNode (color support) and Node size control
      <li>22/12/00 - EG - Sprites are no longer texture enabled by default,
                          updated TGLSprite.BuildList to work with new matrices
      <li>14/11/00 - EG - Added TGLDummyCube.Destroy (thx Airatz)
      <li>08/10/00 - EG - Fixed call to wglUseFontOutlines
      <li>06/08/00 - EG - TRotationSolid renamed to TGLRevolutionSolid & moved to GLExtrusion
      <li>04/08/00 - EG - Fixed sphere main body texture coords + slight speedup
      <li>02/08/00 - EG - Added TGLPolygonBase
      <li>19/07/00 - EG - Added TGLHUDSprite
      <li>18/07/00 - EG - Added TGLRevolutionSolid
      <li>15/07/00 - EG - Code reduction and minor speedup for all quadric objects,
                          Added TGLLineBase (split of TGLLines),
                          TGLDummyCube now uses osDirectDraw instead of special behaviour
      <li>13/07/00 - EG - Added TGLArrowLine (code by Aaron Hochwimmer)
      <li>28/06/00 - EG - Support for "ObjectStyle"
      <li>23/06/00 - EG - Reduced default Loop count for TGLDisk
      <li>18/06/00 - EG - TGLMesh and accompanying stuff moved to GLMesh
      <li>14/06/00 - EG - Added Capacity to TVertexList
      <li>09/06/00 - EG - First row of Geometry-related upgrades
      <li>08/06/00 - EG - Added ReleaseFontManager, fixed TGLSpaceText DestroyList,
      <li>01/06/00 - EG - Added TGLAnnulus (code by Aaron Hochwimmer)
      <li>29/05/00 - EG - TGLLines now uses TGLNode/TGLNodes
      <li>28/05/00 - EG - Added persistence ability to TGLLines,
                          Added defaults for all TGLLines properties
      <li>27/05/00 - EG - Moved in RogerCao's TGLLines object, added a TLineNode
                          class (currently private) and various enhancements + fixes,
                          DodecahedronBuildList now available as a procedure,
                          CubeWireframeBuildList now available as a procedure
      <li>26/05/00 - RoC - Added division property to TGLLines, and Spline supported
      <li>26/05/00 - EG - Moved vectorfile remnants to GLVectorFiles
      <li>14/05/00 - EG - Removed Top/Bottom checks for TGLSphere,
                          Added mmTriangleStrip support in CalcNormals
      <li>08/05/00 - EG - Uncommented DisableAutoTexture in TGLSpaceText.BuildList
      <li>07/05/00 - RoC - TGLLines added, to show a list of vertex
		<li>26/04/00 - EG - Reactivated stuff in SetupQuadricParams (thanks Nelson Chu)
		<li>18/04/00 - EG - Overriden TGLDummyCube.Render
		<li>16/04/00 - EG - FontManager now published and auto-creating
		<li>12/04/00 - EG - Added TGLCylinderBase.Loops (fixes a bug, thanks Uwe)
      <li>24/03/00 - EG - Added Rotation to TGLSprite, fixed sprite size
		<li>20/03/00 - EG - Enhanced FontManager
		<li>17/03/00 - EG - Fixed SpaceText glBaseList bug,
								  TGLSprite now uses a transposition of the globalmatrix
		<li>16/03/00 - EG - Enhanced TFontManager to allow lower quality
		<li>14/03/00 - EG - Added subobjects Barycenter support for TGLDummyCube
      <li>09/02/00 - EG - ObjectManager stuff moved to GLSceneRegister,
                          FreeForm and vector file stuff moved to new GLVectorFileObjects
      <li>08/02/00 - EG - Added TGLDummyCube
      <li>05/02/00 - EG - Javadocisation, fixes and enhancements :
                          TVertexList.AddVertex, "default"s to properties
   </ul></font>
}
unit GLObjects;

{$R-}

interface

{$i GLScene.inc}

uses Classes, VectorGeometry, GLScene, GLTexture, GLMisc, OpenGL1x, SysUtils,
   VectorLists, GLCrossPlatform, GLContext, GLSilhouette;

type

   // TGLVisibilityDeterminationEvent
   //
   TGLVisibilityDeterminationEvent = function (Sender : TObject;
                              var rci : TRenderContextInfo) : Boolean of object;

	// TGLDummyCube
	//
	{: A simple cube, invisible at run-time.<p>
      This is a usually non-visible object -except at design-time- used for
      building hierarchies or groups, when some kind of joint or movement
      mechanism needs be described, you can use DummyCubes.<br>
		DummyCube's barycenter is its children's barycenter.<br>
      The DummyCube can optionnally amalgamate all its children into a single
      display list (see Amalgamate property). }
	TGLDummyCube = class (TGLCameraInvariantObject)
		private
			{ Private Declarations }
			FCubeSize : TGLFloat;
			FEdgeColor : TGLColor;
			FVisibleAtRunTime, FAmalgamate : Boolean;
         FGroupList : TGLListHandle;
         FOnVisibilityDetermination : TGLVisibilityDeterminationEvent;

		protected
			{ Protected Declarations }
			procedure SetCubeSize(const val : TGLFloat);
			procedure SetEdgeColor(const val : TGLColor);
			procedure SetVisibleAtRunTime(const val : Boolean);
         procedure SetAmalgamate(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;
			procedure BuildList(var rci : TRenderContextInfo); override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;
         procedure StructureChanged; override;
			function BarycenterAbsolutePosition : TVector; override;


		published
			{ Published Declarations }
			property CubeSize : TGLFloat read FCubeSize write SetCubeSize;
			property EdgeColor : TGLColor read FEdgeColor write SetEdgeColor;
         {: If true the dummycube's edges will be visible at runtime.<p>
            The default behaviour of the dummycube is to be visible at design-time
            only, and invisible at runtime. }
			property VisibleAtRunTime : Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;
         {: Amalgamate the dummy's children in a single OpenGL entity.<p>
            This activates a special rendering mode, which will compile
            the rendering of all of the dummycube's children objects into a
            single display list. This may provide a significant speed up in some
            situations, however, this means that changes to the children will
            be ignored untill you call StructureChanged on the dummy cube.<br>
            Some objects, that have their own display list management, may not
            be compatible with this behaviour. This will also prevents sorting
            and culling to operate as usual.<p>
            In short, this features is best used for static, non-transparent
            geometry, or when the point of view won't change over a large
            number of frames. }
         property Amalgamate : Boolean read FAmalgamate write SetAmalgamate default False;
         {: Camera Invariance Options.<p>
            These options allow to "deactivate" sensitivity to camera, f.i. by
            centering the object on the camera or ignoring camera orientation. }
         property CamInvarianceMode default cimNone;
         {: Event for custom visibility determination.<p>
            Event handler should return True if the dummycube and its children
            are to be considered visible for the current render. }
         property OnVisibilityDetermination : TGLVisibilityDeterminationEvent read FOnVisibilityDetermination write FOnVisibilityDetermination;
	end;

   // TPlaneStyle
   //
   TPlaneStyle = (psSingleQuad, psTileTexture);
   TPlaneStyles = set of TPlaneStyle;

   // Plane
   //
   {: A simple plane object.<p>
      Note that a plane is always made of a single quad (two triangles) and the
      tiling is only applied to texture coordinates. }
	TGLPlane = class (TGLSceneObject)
	   private
			{ Private Declarations }
	      FXOffset, FYOffset : TGLFloat;
	      FXScope, FYScope : TGLFloat;
			FWidth, FHeight : TGLFloat;
		   FXTiles, FYTiles : Cardinal;
         FStyle : TPlaneStyles;
         FNoZWrite : Boolean;

		protected
			{ Protected Declarations }
		   procedure SetHeight(const aValue : Single);
		   procedure SetWidth(const aValue : Single);
		   procedure SetXOffset(const Value: TGLFloat);
		   procedure SetXScope(const Value: TGLFloat);
         function  StoreXScope : Boolean;
		   procedure SetXTiles(const Value: Cardinal);
		   procedure SetYOffset(const Value: TGLFloat);
		   procedure SetYScope(const Value: TGLFloat);
         function  StoreYScope : Boolean;
		   procedure SetYTiles(const Value: Cardinal);
         procedure SetStyle(const val : TPlaneStyles);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

		   procedure Assign(Source: TPersistent); override;

		   procedure BuildList(var rci : TRenderContextInfo); override;
         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;

         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;
         {: Computes the screen coordinates of the smallest rectangle encompassing the plane.<p>
            Returned extents are NOT limited to any physical screen extents. }
         function ScreenRect : TGLRect;

         {: Computes the signed distance to the point.<p>
            Point coordinates are expected in absolute coordinates. }
         function PointDistance(const aPoint : TVector) : Single;

		published
			{ Public Declarations }
			property Height : TGLFloat read FHeight write SetHeight;
         property Width : TGLFloat read FWidth write SetWidth;
         property XOffset : TGLFloat read FXOffset write SetXOffset;
         property XScope : TGLFloat read FXScope write SetXScope stored StoreXScope;
         property XTiles : Cardinal read FXTiles write SetXTiles default 1;
         property YOffset : TGLFloat read FYOffset write SetYOffset;
         property YScope : TGLFloat read FYScope write SetYScope stored StoreYScope;
         property YTiles : Cardinal read FYTiles write SetYTiles default 1;
         property Style : TPlaneStyles read FStyle write SetStyle default [psSingleQuad, psTileTexture];
         property NoZWrite : Boolean read FNoZWrite write FNoZWrite;
   end;

	// TGLSprite
	//
	{: A rectangular area, perspective projected, but always facing the camera.<p>
      A TGLSprite is perspective projected and as such is scaled with distance,
      if you want a 2D sprite that does not get scaled, see TGLHUDSprite. }
	TGLSprite = class (TGLSceneObject)
		private
			{ Private Declarations }
			FWidth : TGLFloat;
			FHeight : TGLFloat;
			FRotation : TGLFloat;
         FAlphaChannel : Single;
         FNoZWrite : Boolean;
         FMirrorU,
         FMirrorV : Boolean;
      FUVTop: TGLFloat;
      FUVLeft: TGLFloat;
      FUVBottom: TGLFloat;
      FUVRight: TGLFloat;

		protected
			{ Protected Declarations }
			procedure SetWidth(const val : TGLFloat);
			procedure SetHeight(const val : TGLFloat);
         procedure SetRotation(const val : TGLFloat);
         procedure SetAlphaChannel(const val : Single);
         function StoreAlphaChannel : Boolean;
         procedure SetNoZWrite(const val : Boolean);
         procedure SetMirrorU(const val : Boolean);
         procedure SetMirrorV(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

      function AxisAlignedDimensionsUnscaled : TVector; override;

			procedure SetSize(const width, height : TGLFloat);
			//: Set width and height to "size"
			procedure SetSquareSize(const size : TGLFloat);

		published
			{ Published Declarations }
         {: Sprite Width in 3D world units. }
			property Width : TGLFloat read FWidth write SetWidth;
         {: Sprite Height in 3D world units. }
			property Height : TGLFloat read FHeight write SetHeight;
			{: This the ON-SCREEN rotation of the sprite.<p>
            Rotatation=0 is handled faster. }
         property Rotation : TGLFloat read FRotation write SetRotation;
         {: If different from 1, this value will replace that of Diffuse.Alpha }
         property AlphaChannel : Single read FAlphaChannel write SetAlphaChannel stored StoreAlphaChannel;
         {: If True, sprite will not write to Z-Buffer.<p>
            Sprite will STILL be maskable by ZBuffer test. }
         property NoZWrite : Boolean read FNoZWrite write SetNoZWrite;
         {: Reverses the texture coordinates in the U and V direction to mirror 
            the texture. }
         property MirrorU : Boolean read FMirrorU write SetMirrorU;
         property MirrorV : Boolean read FMirrorV write SetMirrorV;

         property UVTop: TGLFloat read FUVTop write FUVTop;
         property UVLeft: TGLFloat read FUVLeft write FUVLeft;
         property UVBottom: TGLFloat read FUVBottom write FUVBottom;
         property UVRight: TGLFloat read FUVRight write FUVRight;
	end;

   // TGLPointStyle
   //
   TGLPointStyle = (psSquare, psRound, psSmooth, psSmoothAdditive, psSquareAdditive);

	// TGLPointParameters
	//
	{: Point parameters as in ARB_point_parameters.<p>
      Make sure to read the ARB_point_parameters spec if you want to understand
      what each parameter does. }
	TGLPointParameters = class (TGLUpdateAbleObject)
		private
			{ Private Declarations }
         FEnabled : Boolean;
         FMinSize, FMaxSize : Single;
         FFadeTresholdSize : Single;
         FDistanceAttenuation : TGLCoordinates;

		protected
			{ Protected Declarations }
         procedure SetEnabled(const val : Boolean);
         procedure SetMinSize(const val : Single);
         procedure SetMaxSize(const val : Single);
         procedure SetFadeTresholdSize(const val : Single);
         procedure SetDistanceAttenuation(const val : TGLCoordinates);

			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);

		public
			{ Public Declarations }
         constructor Create(AOwner : TPersistent); override;
         destructor Destroy; override;

         procedure Assign(Source : TPersistent); override;

         procedure Apply;
         procedure UnApply;

		published
			{ Published Declarations }
         property Enabled : Boolean read FEnabled write SetEnabled default False;
         property MinSize : Single read FMinSize write SetMinSize stored False;
         property MaxSize : Single read FMaxSize write SetMaxSize stored False;
         property FadeTresholdSize : Single read FFadeTresholdSize write SetFadeTresholdSize stored False;
         {: Components XYZ are for constant, linear and quadratic attenuation. }
         property DistanceAttenuation : TGLCoordinates read FDistanceAttenuation write SetDistanceAttenuation;
   end;

	// TGLPoints
	//
	{: Renders a set of non-transparent colored points.<p>
      The points positions and their color are defined through the Positions
      and Colors properties. }
	TGLPoints = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
         FPositions : TAffineVectorList;
         FColors : TVectorList;
         FSize : Single;
         FStyle : TGLPointStyle;
         FPointParameters : TGLPointParameters;
         FNoZWrite, FStatic : Boolean;

		protected
			{ Protected Declarations }
         function StoreSize : Boolean;
         procedure SetNoZWrite(const val : Boolean);
         procedure SetStatic(const val : Boolean);
         procedure SetSize(const val : Single);
         procedure SetPositions(const val : TAffineVectorList);
         procedure SetColors(const val : TVectorList);
         procedure SetStyle(const val : TGLPointStyle);
         procedure SetPointParameters(const val : TGLPointParameters);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

         {: Points positions.<p>
            If empty, a single point is assumed at (0, 0, 0) }
         property Positions : TAffineVectorList read FPositions write SetPositions;
         {: Defines the points colors.<p>
            <ul>
            <li>if empty, point color will be opaque white
            <li>if contains a single color, all points will use that color
            <li>if contains N colors, the first N points (at max) will be rendered
                using the corresponding colors.
            </ul> }
         property Colors : TVectorList read FColors write SetColors;

		published
			{ Published Declarations }
         {: If true points do not write their Z to the depth buffer. }
         property NoZWrite : Boolean read FNoZWrite write SetNoZWrite;
         {: Tells the component if point coordinates are static.<p>
            If static, changes to the positions should be notified via an
            explicit StructureChanged call, or may not refresh.<br>
            Static sets of points may render faster than dynamic ones. }
         property Static : Boolean read FStatic write SetStatic;
         {: Point size, all points have a fixed size. }
         property Size : Single read FSize write SetSize stored StoreSize;
         {: Points style.<p> }
         property Style : TGLPointStyle read FStyle write SetStyle default psSquare;
         {: Point parameters as of ARB_point_parameters.<p>
            Allows to vary the size and transparency of points depending
            on their distance to the observer. }
         property PointParameters : TGLPointParameters read FPointParameters write SetPointParameters;

	end;

   // TLineNodesAspect
   //
   {: Possible aspects for the nodes of a TLine. }
   TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);

   // TLineSplineMode
   //
   {: Available spline modes for a TLine. }
   TLineSplineMode = (lsmLines, lsmCubicSpline, lsmBezierSpline, lsmNURBSCurve,
                      lsmSegments);

   // TGLLinesNode
   //
   {: Specialized Node for use in a TGLLines objects.<p>
      Adds a Color property (TGLColor). }
   TGLLinesNode = class(TGLNode)
      private
			{ Private Declarations }
         FColor : TGLColor;

		protected
			{ Protected Declarations }
         procedure SetColor(const val : TGLColor);
         procedure OnColorChange(sender : TObject);
         function StoreColor : Boolean;

      public
         { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }

         {: The node color.<p>
            Can also defined the line color (interpolated between nodes) if
            loUseNodeColorForLines is set (in TGLLines). }
         property Color : TGLColor read FColor write SetColor stored StoreColor;
   end;

   // TGLLinesNodes
   //
   {: Specialized collection for Nodes in a TGLLines objects.<p>
      Stores TGLLinesNode items. }
   TGLLinesNodes = class(TGLNodes)
      public
        { Public Declarations }
	      constructor Create(AOwner : TComponent); overload;

         procedure NotifyChange; override;
   end;

   // TGLLineBase
   //
   {: Base class for line objects.<p>
      Introduces line style properties (width, color...). }
   TGLLineBase = class(TGLImmaterialSceneObject)
      private
			{ Private Declarations }
         FLineColor : TGLColor;
         FLinePattern : TGLushort;
         FLineWidth : Single;
         FAntiAliased : Boolean;

		protected
			{ Protected Declarations }
         procedure SetLineColor(const value: TGLColor);
         procedure SetLinePattern(const value: TGLushort);
         procedure SetLineWidth(const val : Single);
         function StoreLineWidth : Boolean;
         procedure SetAntiAliased(const val : Boolean);

         {: Setup OpenGL states according to line style.<p>
            You must call RestoreLineStyle after drawing your lines.<p>
            You may use nested calls with SetupLineStyle/RestoreLineStyle. }
         procedure SetupLineStyle;
         {: Restore OpenGL states, must follow a SetupLineStyle }
         procedure RestoreLineStyle;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }
         {: Indicates if OpenGL should smooth line edges.<p>
            Smoothed lines looks better but are poorly implemented in most OpenGL
            drivers and take *lots* of rendering time. }
         property AntiAliased : Boolean read FAntiAliased write SetAntiAliased default False;
         {: Default color of the lines. }
         property LineColor: TGLColor read FLineColor write SetLineColor;
         {: Bitwise line pattern.<p>
            For instance $FFFF (65535) is a white line (stipple disabled), $0000
            is a black line, $CCCC is the stipple used in axes and dummycube, etc. }
         property LinePattern: TGLushort read FLinePattern write SetLinePattern default $FFFF;
         {: Default width of the lines. }
         property LineWidth : Single read FLineWidth write SetLineWidth stored StoreLineWidth;
         property Visible;
   end;

   // TGLNodedLines
   //
   {: Class that defines lines via a series of nodes.<p>
      Base class, does not render anything. }
   TGLNodedLines = class(TGLLineBase)
      private
			{ Private Declarations }
         FNodes : TGLLinesNodes;
         FNodesAspect : TLineNodesAspect;
         FNodeColor : TGLColor;
         FNodeSize : Single;
         FOldNodeColor : TColorVector;

		protected
			{ Protected Declarations }
         procedure SetNodesAspect(const value : TLineNodesAspect);
         procedure SetNodeColor(const value: TGLColor);
         procedure OnNodeColorChanged(sender : TObject);
         procedure SetNodes(const aNodes : TGLLinesNodes);
         procedure SetNodeSize(const val : Single);
         function StoreNodeSize : Boolean;

         procedure DrawNode(var rci : TRenderContextInfo; X, Y, Z: Single; Color: TGLColor);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         function AxisAlignedDimensionsUnscaled : TVector; override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

      published
			{ Published Declarations }
         {: Default color for nodes.<p>
            lnaInvisible and lnaAxes ignore this setting. }
         property NodeColor: TGLColor read FNodeColor write SetNodeColor;
         {: The nodes list.<p> }
         property Nodes : TGLLinesNodes read FNodes write SetNodes;

         {: Default aspect of line nodes.<p>
            May help you materialize nodes, segments and control points. }
         property NodesAspect: TLineNodesAspect read FNodesAspect write SetNodesAspect default lnaAxes;
         {: Size for the various node aspects. }
         property NodeSize : Single read FNodeSize write SetNodeSize stored StoreNodeSize;
   end;

   // TLinesOptions
   //
   TLinesOption = (loUseNodeColorForLines);
   TLinesOptions = set of TLinesOption;

   // TGLLines
   //
   {: Set of 3D line segments.<p>
      You define a 3D Line by adding its nodes in the "Nodes" property. The line
      may be rendered as a set of segment or as a curve (nodes then act as spline
      control points).<p>
      Alternatively, you can also use it to render a set of spacial nodes (points
      in space), just make the lines transparent and the nodes visible by picking
      the node aspect that suits you. }
   TGLLines = class(TGLNodedLines)
      private
			{ Private Declarations }
         FDivision : Integer;
         FSplineMode : TLineSplineMode;
         FOptions : TLinesOptions;
         FNURBSOrder : Integer;
         FNURBSTolerance : Single;
         FNURBSKnots : TSingleList;

		protected
			{ Protected Declarations }
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetDivision(const value: Integer);
         procedure SetOptions(const val : TLinesOptions);
         procedure SetNURBSOrder(const val : Integer);
         procedure SetNURBSTolerance(const val : Single);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;

         property NURBSKnots : TSingleList read FNURBSKnots;
         property NURBSOrder : Integer read FNURBSOrder write SetNURBSOrder;
         property NURBSTolerance : Single read FNURBSTolerance write SetNURBSTolerance;

      published
			{ Published Declarations }
         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p> }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

         {: Rendering options for the line.<p>
            <ul>
            <li>loUseNodeColorForLines: if set lines will be drawn using node
               colors (and color interpolation between nodes), if not, LineColor
               will be used (single color).
            </ul> }
         property Options : TLinesOptions read FOptions write SetOptions;
   end;

	TCubePart  = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
	TCubeParts = set of TCubePart;

   // TGLCube
   //
   {: A simple cube object.<p>
      This cube use the same material for each of its faces, ie. all faces look
      the same. If you want a multi-material cube, use a mesh in conjunction
      with a TGLFreeForm and a material library. }
   TGLCube = class (TGLSceneObject)
		private
			{ Private Declarations }
         FCubeSize : TAffineVector;
         FParts : TCubeParts;
         FNormalDirection : TNormalDirection;
         procedure SetCubeWidth(const aValue : Single);
         procedure SetCubeHeight(const aValue : Single);
         procedure SetCubeDepth(const aValue : Single);
         procedure SetParts(aValue: TCubeParts);
         procedure SetNormalDirection(aValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property CubeWidth: TGLFloat read FCubeSize[0] write SetCubeWidth stored False;
         property CubeHeight: TGLFloat read FCubeSize[1] write SetCubeHeight stored False;
         property CubeDepth: TGLFloat read FCubeSize[2] write SetCubeDepth stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
   end;

   // TNormalSmoothing
   //
   {: Determines how and if normals are smoothed.<p>
      - nsFlat : facetted look<br>
      - nsSmooth : smooth look<br>
      - nsNone : unlighted rendering, usefull for decla texturing }
   TNormalSmoothing = (nsFlat, nsSmooth, nsNone);

   // TGLQuadricObject
   //
   {: Base class for quadric objects.<p>
      Introduces some basic Quadric interaction functions (the actual quadric
      math is part of the GLU library). }
   TGLQuadricObject = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormals : TNormalSmoothing;
         FNormalDirection : TNormalDirection;

      protected
         { Protected Declarations }
         procedure SetNormals(aValue : TNormalSmoothing);
         procedure SetNormalDirection(aValue : TNormalDirection);
         procedure SetupQuadricParams(quadric : PGLUquadricObj);
         procedure SetNormalQuadricOrientation(quadric : PGLUquadricObj);
         procedure SetInvertedQuadricOrientation(quadric : PGLUquadricObj);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations }
         property Normals : TNormalSmoothing read FNormals write SetNormals default nsSmooth;
         property NormalDirection : TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
   end;

   TAngleLimit1 = -90..90;
   TAngleLimit2 = 0..360;
   TCapType = (ctNone, ctCenter, ctFlat);

   // TGLSphere
   //
   {: A sphere object.<p>
      The sphere can have to and bottom caps, as well as being just a slice
      of sphere. }
   TGLSphere = class (TGLQuadricObject)
      private
         { Private Declarations }
         FRadius  : TGLFloat;
         FSlices, FStacks  : TGLInt;
         FTop     : TAngleLimit1;
         FBottom  : TAngleLimit1;
         FStart   : TAngleLimit2;
         FStop    : TAngleLimit2;
         FTopCap, FBottomCap : TCapType;
         procedure SetBottom(aValue: TAngleLimit1);
         procedure SetBottomCap(aValue: TCapType);
         procedure SetRadius(const aValue : TGLFloat);
         procedure SetSlices(aValue: TGLInt);
         procedure SetStart(aValue: TAngleLimit2);
         procedure SetStop(aValue: TAngleLimit2);
         procedure SetStacks(aValue : TGLInt);
         procedure SetTop(aValue: TAngleLimit1);
         procedure SetTopCap(aValue: TCapType);

      public
         { Public Declarations }
         constructor Create(AOwner:TComponent); override;
         procedure Assign(Source:TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

          function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;
      published
         { Published Declarations }
         property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
         property BottomCap: TCapType read FBottomCap write SetBottomCap default ctNone;
         property Radius: TGLFloat read FRadius write SetRadius;
         property Slices: TGLInt read FSlices write SetSlices default 16;
         property Stacks: TGLInt read FStacks write SetStacks default 16;
         property Start: TAngleLimit2 read FStart write SetStart default 0;
         property Stop: TAngleLimit2 read FStop write SetStop default 360;
         property Top: TAngleLimit1 read FTop write SetTop default 90;
         property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
   end;

   // TGLPolygonBase
   //
   {: Base class for objects based on a polygon. }
   TGLPolygonBase = class(TGLSceneObject)
      private
			{ Private Declarations }
         FDivision : Integer;
         FSplineMode : TLineSplineMode;

		protected
			{ Protected Declarations }
         FNodes : TGLNodes;
         procedure CreateNodes; dynamic;
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetDivision(const value: Integer);
         procedure SetNodes(const aNodes : TGLNodes);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure NotifyChange(Sender : TObject); override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

      published
			{ Published Declarations }
         {: The nodes list.<p> }
         property Nodes : TGLNodes read FNodes write SetNodes;
         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p>
            This mode is used only for the curve, not for the rotation path. }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

   end;

{: Issues OpenGL for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildList(var rci : TRenderContextInfo;
                                 size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);
{: Issues OpenGL for a unit-size dodecahedron. }
procedure DodecahedronBuildList;
{: Issues OpenGL for a unit-size icosahedron. }
procedure IcosahedronBuildList;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses GLStrings, Spline, XOpenGL, GLState;

const
   cDefaultPointSize : Single = 1.0;

// CubeWireframeBuildList
//
procedure CubeWireframeBuildList(var rci : TRenderContextInfo;
                                 size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);
var
	mi, ma : Single;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   glEnable(GL_LINE_SMOOTH);
   if stipple then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, $CCCC);
   end;
   glLineWidth(1);
   ma:=0.5*size;
   mi:=-ma;
   rci.GLStates.ResetGLMaterialColors;
   glColorMaterial(GL_FRONT, GL_EMISSION);
   glEnable(GL_COLOR_MATERIAL);
   glColor4fv(@color);
   glBegin(GL_LINE_STRIP);
      // front face
      glVertex3f(ma, mi, mi); glVertex3f(ma, ma, mi);
      glVertex3f(ma, ma, ma); glVertex3f(ma, mi, ma);
      glVertex3f(ma, mi, mi);
      // partial up back face
      glVertex3f(mi, mi, mi); glVertex3f(mi, mi, ma);
      glVertex3f(mi, ma, ma); glVertex3f(mi, ma, mi);
      // right side low
      glVertex3f(ma, ma, mi);
   glEnd;
   glBegin(GL_LINES);
      // right high
      glVertex3f(ma, ma, ma);	glVertex3f(mi, ma, ma);
      // back low
      glVertex3f(mi, mi, mi); glVertex3f(mi, ma, mi);
      // left high
      glVertex3f(ma, mi, ma); glVertex3f(mi, mi, ma);
   glEnd;
   glPopAttrib;
end;

// DodecahedronBuildList
//
procedure DodecahedronBuildList;
const
   A = 1.61803398875*0.3; // (Sqrt(5)+1)/2 
   B = 0.61803398875*0.3; // (Sqrt(5)-1)/2
   C = 1*0.3;
const
   vertices : packed array [0..19] of TAffineVector =
      ((-A,  0,  B), (-A,  0, -B), ( A,  0, -B), (A,  0,  B),
       ( B, -A,  0), (-B, -A,  0), (-B,  A,  0), (B,  A,  0),
       ( 0,  B, -A), ( 0, -B, -A), ( 0, -B,  A), (0,  B,  A),
       (-C, -C,  C), (-C, -C, -C), ( C, -C, -C), (C, -C,  C),
       (-C,  C,  C), (-C,  C, -C), ( C,  C, -C), (C,  C,  C));

   polygons : packed array [0..11] of packed array [0..4] of Byte =
      (( 0, 12, 10, 11, 16),  ( 1, 17,  8,  9, 13),
       ( 2, 14,  9,  8, 18),  ( 3, 19, 11, 10, 15),
       ( 4, 14,  2,  3, 15),  ( 5, 12,  0,  1, 13),
       ( 6, 17,  1,  0, 16),  ( 7, 19,  3,  2, 18),
       ( 8, 17,  6,  7, 18),  ( 9, 14,  4,  5, 13),
       (10, 12,  5,  4, 15),  (11, 19,  7,  6, 16));

var
   i, j : Integer;
   n : TAffineVector;
   faceIndices : PByteArray;
begin
   for i:=0 to 11 do begin
      faceIndices:=@polygons[i, 0];

      n:=CalcPlaneNormal(vertices[faceIndices[0]],
                         vertices[faceIndices[1]],
                         vertices[faceIndices[2]]);
      glNormal3fv(@N);

      glBegin(GL_TRIANGLE_FAN);
      for j:=0 to 4 do
         glVertex3fv(@vertices[faceIndices[j]]);
      glEnd;
   end;
end;

// IcosahedronBuildList
//
procedure IcosahedronBuildList;
const
   A = 0.5;
   B = 0.30901699437; // 1/(1+Sqrt(5))
const
   vertices : packed array [0..11] of TAffineVector =
      (( 0,-A,-A), ( 0,-B, A), ( 0, B,-A), ( 0, B, A),
       (-A, 0,-B), (-A, 0, B), ( A, 0,-B), ( A, 0, B),
       (-B,-A, 0), (-B, A, 0), ( B,-A, 0), ( B, A, 0));


   triangles : packed array [0..19] of packed array [0..2] of Byte =
      (( 2, 9,11), ( 3,11, 9), ( 3, 5, 1), ( 3, 1, 7),
       ( 2, 6, 0), ( 2, 0, 4), ( 1, 8,10), ( 0,10, 8),
       ( 9, 4, 5), ( 8, 5, 4), (11, 7, 6), (10, 6, 7),
       ( 3, 9, 5), ( 3, 7,11), ( 2, 4, 9), ( 2,11, 6),
       ( 0, 8, 4), ( 0, 6,10), ( 1, 5, 8), ( 1,10, 7));

var
   i, j : Integer;
   n : TAffineVector;
   faceIndices : PByteArray;
begin
   for i:=0 to 19 do begin
      faceIndices:=@triangles[i, 0];

      n:=CalcPlaneNormal(vertices[faceIndices[0]],
                         vertices[faceIndices[1]],
                         vertices[faceIndices[2]]);
      glNormal3fv(@N);

      glBegin(GL_TRIANGLES);
      for j:=0 to 2 do
         glVertex3fv(@vertices[faceIndices[j]]);
      glEnd;
   end;
end;


// ------------------
// ------------------ TGLDummyCube ------------------
// ------------------

// Create
//
constructor TGLDummyCube.Create(AOwner : TComponent);
begin
	inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
	FCubeSize:=1;
	FEdgeColor:=TGLColor.Create(Self);
	FEdgeColor.Initialize(clrWhite);
   FGroupList:=TGLListHandle.Create;
   CamInvarianceMode:=cimNone;
end;

// Destroy
//
destructor TGLDummyCube.Destroy;
begin
   FGroupList.Free;
   FEdgeColor.Free;
   inherited;
end;

// Assign
//
procedure TGLDummyCube.Assign(Source: TPersistent);
begin
	if Source is TGLDummyCube then begin
		FCubeSize:=TGLDummyCube(Source).FCubeSize;
		FEdgeColor.Color:=TGLDummyCube(Source).FEdgeColor.Color;
		FVisibleAtRunTime:=TGLDummyCube(Source).FVisibleAtRunTime;
		NotifyChange(Self);
	end;
	inherited Assign(Source);
end;

// AxisAlignedDimensionsUnscaled
//
function TGLDummyCube.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=0.5*Abs(FCubeSize);
   Result[1]:=Result[0];
   Result[2]:=Result[0];
   Result[3]:=0;
end;

// RayCastIntersect
//
function TGLDummyCube.RayCastIntersect(const rayStart, rayVector : TVector;
                                     intersectPoint : PVector = nil;
                                     intersectNormal : PVector = nil) : Boolean;
begin
   Result:=False;
end;

// BuildList
//
procedure TGLDummyCube.BuildList(var rci : TRenderContextInfo);
begin
 	if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
      CubeWireframeBuildList(rci, FCubeSize, True, EdgeColor.Color);
end;

// DoRender
//
procedure TGLDummyCube.DoRender(var rci : TRenderContextInfo;
                                renderSelf, renderChildren : Boolean);
begin
   if Assigned(FOnVisibilityDetermination) then
      if not FOnVisibilityDetermination(Self, rci) then
         Exit;
   if FAmalgamate and (not rci.amalgamating) then begin
      if FGroupList.Handle=0 then begin
         FGroupList.AllocateHandle;
         Assert(FGroupList.Handle<>0, 'Handle=0 for '+ClassName);
         glNewList(FGroupList.Handle, GL_COMPILE);
         rci.amalgamating:=True;
         try
            inherited;
         finally
            rci.amalgamating:=False;
            glEndList;
         end;
      end;
      glCallList(FGroupList.Handle);
   end else begin
      // proceed as usual
      inherited;
   end;
end;

// StructureChanged
//
procedure TGLDummyCube.StructureChanged;
begin
   if FAmalgamate then
      FGroupList.DestroyHandle;
   inherited;
end;

// BarycenterAbsolutePosition
//
function TGLDummyCube.BarycenterAbsolutePosition : TVector;
var
	i : Integer;
begin
	if Count>0 then begin
		Result:=Children[0].BarycenterAbsolutePosition;
		for i:=1 to Count-1 do
			Result:=VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
		ScaleVector(Result, 1/Count);
	end else Result:=AbsolutePosition;
end;

// SetCubeSize
//
procedure TGLDummyCube.SetCubeSize(const val : TGLFloat);
begin
	if val<>FCubeSize then begin
		FCubeSize:=val;
		StructureChanged;
	end;
end;

// SetEdgeColor
//
procedure TGLDummyCube.SetEdgeColor(const val : TGLColor);
begin
	if val<>FEdgeColor then begin
		FEdgeColor.Assign(val);
		StructureChanged;
	end;
end;

// SetVisibleAtRunTime
//
procedure TGLDummyCube.SetVisibleAtRunTime(const val : Boolean);
begin
	if val<>FVisibleAtRunTime then begin
		FVisibleAtRunTime:=val;
		StructureChanged;
	end;
end;

// SetAmalgamate
//
procedure TGLDummyCube.SetAmalgamate(const val : Boolean);
begin
   if val<>FAmalgamate then begin
      FAmalgamate:=val;
      if val then
         ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding]
      else begin
         FGroupList.DestroyHandle;
         ObjectStyle:=ObjectStyle-[osDoesTemperWithColorsOrFaceWinding]
      end;
      inherited StructureChanged;
   end;
end;

// ------------------
// ------------------ TGLPlane ------------------
// ------------------

// Create
//
constructor TGLPlane.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FXTiles:=1;
   FYTiles:=1;
   FXScope:=1;
   FYScope:=1;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FStyle:=[psSingleQuad, psTileTexture];
end;

// Assign
//
procedure TGLPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLPlane) then begin
      FWidth:=TGLPlane(Source).FWidth;
      FHeight:=TGLPlane(Source).FHeight;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLPlane.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=0.5*Abs(FWidth);
   Result[1]:=0.5*Abs(FHeight);
   Result[2]:=0;
end;

// RayCastIntersect
//
function TGLPlane.RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean;
var
   locRayStart, locRayVector, ip : TVector;
   t : Single;
begin
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);
   if locRayStart[2]>=0 then begin
      // ray start over plane
      if locRayVector[2]<0 then begin
         t:=locRayStart[2]/locRayVector[2];
         ip[0]:=locRayStart[0]-t*locRayVector[0];
         ip[1]:=locRayStart[1]-t*locRayVector[1];
         if (Abs(ip[0])<=0.5*Width) and (Abs(ip[1])<=0.5*Height) then begin
            Result:=True;
            if Assigned(intersectNormal) then
               intersectNormal^:=AbsoluteDirection;
         end else Result:=False;
      end else Result:=False;
   end else begin
      // ray start below plane
      if locRayVector[2]>0 then begin
         t:=locRayStart[2]/locRayVector[2];
         ip[0]:=locRayStart[0]-t*locRayVector[0];
         ip[1]:=locRayStart[1]-t*locRayVector[1];
         if (Abs(ip[0])<=0.5*Width) and (Abs(ip[1])<=0.5*Height) then begin
            Result:=True;
            if Assigned(intersectNormal) then
               intersectNormal^:=VectorNegate(AbsoluteDirection);
         end else Result:=False;
      end else Result:=False;
   end;
   if Result and Assigned(intersectPoint) then begin
      ip[2]:=0;
      ip[3]:=1;
      intersectPoint^:=LocalToAbsolute(ip);
   end;
end;

// GenerateSilhouette
//
function TGLPlane.GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette;
var
   hw, hh : single;
begin
   Result := TGLSilhouette.Create;

   hw:=FWidth*0.5;
   hh:=FHeight*0.5;

   with Result.Vertices do begin
      AddPoint( hw,  hh);
      AddPoint( hw, -hh);
      AddPoint(-hw, -hh);
      AddPoint(-hw,  hh);
   end;

   with Result.Indices do begin
      Add(0, 1);
      Add(1, 2);
      Add(2, 3);
      Add(3, 0);
   end;

   if silhouetteParameters.CappingRequired then with Result.CapIndices do begin
      Add(0, 1, 2);
      Add(2, 3, 0);
   end;
end;

// BuildList
//
procedure TGLPlane.BuildList(var rci : TRenderContextInfo);
var
   hw, hh, posXFact, posYFact, pX, pY0, pY1 : TGLFloat;
   tx0, tx1, ty0, ty1, texSFact, texTFact : TGLFloat;
   texS, texT0, texT1 : TGLFloat;
   x, y : Integer;
begin
   hw:=FWidth*0.5;
   hh:=FHeight*0.5;
   glNormal3fv(@ZVector);
   // determine tex coords extents
   if psTileTexture in FStyle then begin
      tx0:=FXOffset;
      tx1:=FXTiles*FXScope+FXOffset;
      ty0:=FYOffset;
      ty1:=FYTiles*FYScope+FYOffset;
   end else begin
      tx0:=0;
      ty0:=tx0;
      tx1:=FXScope;
      ty1:=FYScope;
   end;

   if NoZWrite then
      glDepthMask(False);

   if psSingleQuad in FStyle then begin
      // single quad plane
      glBegin(GL_QUADS);
         xglTexCoord2f(tx1, ty1);
         glVertex2f( hw, hh);
         xglTexCoord2f(tx0, ty1);
         glVertex2f(-hw, hh);
         xglTexCoord2f(tx0, ty0);
         glVertex2f(-hw, -hh);
         xglTexCoord2f(tx1, ty0);
         glVertex2f( hw, -hh);
      glEnd;
   end else begin
      // multi-quad plane (actually built from tri-strips)
      texSFact:=(tx1-tx0)/FXTiles;
      texTFact:=(ty1-ty0)/FYTiles;
      posXFact:=FWidth/FXTiles;
      posYFact:=FHeight/FYTiles;
      texT0:=0;
      pY0:=-hh;
      for y:=0 to FYTiles-1 do begin
         texT1:=(y+1)*texTFact;
         pY1:=(y+1)*posYFact-hh;
         glBegin(GL_TRIANGLE_STRIP);
         for x:=0 to FXTiles do begin
            texS:=tx0+x*texSFact;
            pX:=x*posXFact-hw;
            xglTexCoord2f(texS, texT1);
            glVertex2f(pX, pY1);
            xglTexCoord2f(texS, texT0);
            glVertex2f(pX, pY0);
         end;
         glEnd;
         texT0:=texT1;
         pY0:=pY1;
      end;
   end;

   if NoZWrite then
      glDepthMask(True);

end;

// SetWidth
//
procedure TGLPlane.SetWidth(const aValue : Single);
begin
   if aValue<>FWidth then begin
      FWidth:=aValue;
	   StructureChanged;
   end;
end;

// ScreenRect
//
function TGLPlane.ScreenRect : TGLRect;
var
   v : array [0..3] of TVector;
   buf : TGLSceneBuffer;
   hw, hh : TGLFloat;
begin
   buf:=Scene.CurrentBuffer;
   if Assigned(buf) then begin
      hw:=FWidth*0.5;
      hh:=FHeight*0.5;
      v[0]:=LocalToAbsolute(PointMake(-hw, -hh, 0));
      v[1]:=LocalToAbsolute(PointMake( hw, -hh, 0));
      v[2]:=LocalToAbsolute(PointMake( hw,  hh, 0));
      v[3]:=LocalToAbsolute(PointMake(-hw,  hh, 0));
      buf.WorldToScreen(@v[0], 4);
      Result.Left  :=Round(MinFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
      Result.Right :=Round(MaxFloat([v[0][0], v[1][0], v[2][0], v[3][0]]));
      Result.Top   :=Round(MinFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
      Result.Bottom:=Round(MaxFloat([v[0][1], v[1][1], v[2][1], v[3][1]]));
   end else FillChar(Result, SizeOf(TGLRect), 0);
end;

// PointDistance
//
function TGLPlane.PointDistance(const aPoint : TVector) : Single;
begin
   Result:=VectorDotProduct(VectorSubtract(aPoint, AbsolutePosition),
                            AbsoluteDirection);
end;

// SetHeight
//
procedure TGLPlane.SetHeight(const aValue : Single);
begin
   if aValue<>FHeight then begin
      FHeight:=aValue;
      StructureChanged;
   end;
end;

// SetXOffset
//
procedure TGLPlane.SetXOffset(const Value: TGLFloat);
begin
   if Value<>FXOffset then begin
      FXOffset:=Value;
      StructureChanged;
   end;
end;

// SetXScope
//
procedure TGLPlane.SetXScope(const Value: TGLFloat);
begin
   if Value<>FXScope then begin
      FXScope:=Value;
      if FXScope>1 then
         FXScope:=1;
      StructureChanged;
   end;
end;

// StoreXScope
//
function TGLPlane.StoreXScope : Boolean;
begin
   Result:=(FXScope<>1);
end;

// SetXTiles
//
procedure TGLPlane.SetXTiles(const Value: Cardinal);
begin
   if Value<>FXTiles then begin
      FXTiles:=Value;
      StructureChanged;
   end;
end;

// SetYOffset
//
procedure TGLPlane.SetYOffset(const Value: TGLFloat);
begin
   if Value<>FYOffset then begin
      FYOffset:=Value;
      StructureChanged;
   end;
end;

// SetYScope
//
procedure TGLPlane.SetYScope(const Value: TGLFloat);
begin
   if Value<>FYScope then begin
      FYScope:=Value;
      if FYScope>1 then
         FYScope:=1;
      StructureChanged;
   end;
end;

// StoreYScope
//
function TGLPlane.StoreYScope : Boolean;
begin
   Result:=(FYScope<>1);
end;

// SetYTiles
//
procedure TGLPlane.SetYTiles(const Value: Cardinal);
begin
   if Value<>FYTiles then begin
      FYTiles:=Value;
      StructureChanged;
   end;
end;

// SetStyle
//
procedure TGLPlane.SetStyle(const val : TPlaneStyles);
begin
   if val<>FStyle then begin
      FStyle:=val;
      StructureChanged;
   end;
end;

// ------------------
// ------------------ TGLSprite ------------------
// ------------------

// Create
//
constructor TGLSprite.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FAlphaChannel:=1;
	FWidth:=1;
	FHeight:=1;
  FUVTop := 0;
  FUVLeft := 0;
  FUVBottom := 1;
  FUVRight := 1;
end;

// Assign
//
procedure TGLSprite.Assign(Source : TPersistent);
begin
	if Source is TGLSprite then begin
		FWidth:=TGLSprite(Source).FWidth;
		FHeight:=TGLSprite(Source).FHeight;
		FRotation:=TGLSprite(Source).FRotation;
      FAlphaChannel:=TGLSprite(Source).FAlphaChannel;
      FNoZWrite:=TGLSprite(Source).FNoZWrite;
	end;
	inherited Assign(Source);
end;

function TGLSprite.AxisAlignedDimensionsUnscaled: TVector;
begin
   Result[0]:=0.5*Abs(FWidth);
   Result[1]:=0.5*Abs(FHeight);
   // Sprites turn with the camera and can be considered to have the same depth
   // as width
   Result[2]:=0.5*Abs(FWidth);
end;

// BuildList
//
procedure TGLSprite.BuildList(var rci : TRenderContextInfo);
var
   vx, vy : TAffineVector;
   w, h : Single;
   mat : TMatrix;
   u0, v0, u1, v1 : Single;
begin
   if FAlphaChannel<>1 then
      rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, FAlphaChannel);
   if NoZWrite then
      glDepthMask(False);
   glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
   // extraction of the "vecteurs directeurs de la matrice"
   // (dunno how they are named in english)
   w:=FWidth*0.5;
   h:=FHeight*0.5;
   vx[0]:=mat[0][0];  vy[0]:=mat[0][1];
   vx[1]:=mat[1][0];  vy[1]:=mat[1][1];
   vx[2]:=mat[2][0];  vy[2]:=mat[2][1];
   ScaleVector(vx, w/VectorLength(vx));
   ScaleVector(vy, h/VectorLength(vy));
   if FMirrorU then begin
      u0:=FUVRight;
      u1:=FUVLeft;
   end else begin
      u0:=FUVLeft;
      u1:=FUVRight;
   end;
   if FMirrorV then begin
      v0:=FUVBottom;
      v1:=FUVTop;
   end else begin
      v0:=FUVTop;
      v1:=FUVBottom;
   end;

   if FRotation <> 0 then begin
     glPushMatrix;
     glRotatef(FRotation,mat[0][2],mat[1][2],mat[2][2]);
   end;
   glBegin(GL_QUADS);
      xglTexCoord2f(u1, v1);  glVertex3f( vx[0]+vy[0], vx[1]+vy[1], vx[2]+vy[2]);
      xglTexCoord2f(u0, v1);  glVertex3f(-vx[0]+vy[0],-vx[1]+vy[1],-vx[2]+vy[2]);
      xglTexCoord2f(u0, v0);  glVertex3f(-vx[0]-vy[0],-vx[1]-vy[1],-vx[2]-vy[2]);
      xglTexCoord2f(u1, v0);  glVertex3f( vx[0]-vy[0], vx[1]-vy[1], vx[2]-vy[2]);
   glEnd;
   if FRotation <> 0 then
     glPopMatrix;

   if NoZWrite then
      glDepthMask(True);
end;

// SetWidth
//
procedure TGLSprite.SetWidth(const val : TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
		NotifyChange(Self);
	end;
end;

// SetHeight
//
procedure TGLSprite.SetHeight(const val : TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
		NotifyChange(Self);
	end;
end;

// SetRotation
//
procedure TGLSprite.SetRotation(const val : TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		NotifyChange(Self);
	end;
end;

// SetAlphaChannel
//
procedure TGLSprite.SetAlphaChannel(const val : Single);
begin
   if val<>FAlphaChannel then begin
      if val<0 then
         FAlphaChannel:=0
      else if val>1 then
         FAlphaChannel:=1
      else FAlphaChannel:=val;
		NotifyChange(Self);
   end;
end;

// StoreAlphaChannel
//
function TGLSprite.StoreAlphaChannel : Boolean;
begin
	Result:=(FAlphaChannel<>1);
end;

// SetNoZWrite
//
procedure TGLSprite.SetNoZWrite(const val : Boolean);
begin
   FNoZWrite:=val;
   NotifyChange(Self);
end;

// SetMirrorU
//
procedure TGLSprite.SetMirrorU(const val : Boolean);
begin
   FMirrorU:=val;
   NotifyChange(Self);
end;

// SetMirrorV
//
procedure TGLSprite.SetMirrorV(const val : Boolean);
begin
   FMirrorV:=val;
   NotifyChange(Self);
end;

// SetSize
//
procedure TGLSprite.SetSize(const width, height : TGLFloat);
begin
	FWidth:=width;
	FHeight:=height;
   NotifyChange(Self);
end;

// SetSquareSize
//
procedure TGLSprite.SetSquareSize(const size : TGLFloat);
begin
	FWidth:=size;
	FHeight:=size;
   NotifyChange(Self);
end;

// ------------------
// ------------------ TGLPointParameters ------------------
// ------------------

// Create
//
constructor TGLPointParameters.Create(AOwner : TPersistent);
begin
	inherited Create(AOwner);
   FMinSize:=0;
   FMaxSize:=128;
   FFadeTresholdSize:=1;
   FDistanceAttenuation:=TGLCoordinates.CreateInitialized(Self, XHmgVector, csVector);
end;

// Destroy
//
destructor TGLPointParameters.Destroy;
begin
   FDistanceAttenuation.Free;
   inherited;
end;

// Assign
//
procedure TGLPointParameters.Assign(Source : TPersistent);
begin
	if Source is TGLPointParameters then begin
      FMinSize:=TGLPointParameters(Source).FMinSize;
      FMaxSize:=TGLPointParameters(Source).FMaxSize;
      FFadeTresholdSize:=TGLPointParameters(Source).FFadeTresholdSize;
      FDistanceAttenuation.Assign(TGLPointParameters(Source).DistanceAttenuation);
	end;
end;

// DefineProperties
//
procedure TGLPointParameters.DefineProperties(Filer: TFiler);
var
   defaultParams : Boolean;
begin
   inherited;
   defaultParams:=(FMaxSize=128) and (FMinSize=0) and (FFadeTresholdSize=1);
   Filer.DefineBinaryProperty('PointParams', ReadData, WriteData,
                              not defaultParams);
end;

// ReadData
//
procedure TGLPointParameters.ReadData(Stream: TStream);
begin
   with Stream do begin
      Read(FMinSize, SizeOf(Single));
      Read(FMaxSize, SizeOf(Single));
      Read(FFadeTresholdSize, SizeOf(Single));
   end;
end;

// WriteData
//
procedure TGLPointParameters.WriteData(Stream: TStream);
begin
   with Stream do begin
      Write(FMinSize, SizeOf(Single));
      Write(FMaxSize, SizeOf(Single));
      Write(FFadeTresholdSize, SizeOf(Single));
   end;
end;

// Apply
//
procedure TGLPointParameters.Apply;
begin
   if Enabled and GL_ARB_point_parameters then begin
      glPointParameterfARB(GL_POINT_SIZE_MIN_ARB, FMinSize);
      glPointParameterfARB(GL_POINT_SIZE_MAX_ARB, FMaxSize);
      glPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, FFadeTresholdSize);
      glPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB, FDistanceAttenuation.AsAddress);
   end;
end;

// UnApply
//
procedure TGLPointParameters.UnApply;
begin
   if Enabled and GL_ARB_point_parameters then begin
      glPointParameterfARB(GL_POINT_SIZE_MIN_ARB, 0);
      glPointParameterfARB(GL_POINT_SIZE_MAX_ARB, 128);
      glPointParameterfARB(GL_POINT_FADE_THRESHOLD_SIZE_ARB, 1);
      glPointParameterfvARB(GL_DISTANCE_ATTENUATION_ARB, @XVector);
   end;
end;

// SetEnabled
//
procedure TGLPointParameters.SetEnabled(const val : Boolean);
begin
   if val<>FEnabled then begin
      FEnabled:=val;
      NotifyChange(Self);
   end;
end;

// SetMinSize
//
procedure TGLPointParameters.SetMinSize(const val : Single);
begin
   if val<>FMinSize then begin
      if val<0 then
         FMinSize:=0
      else FMinSize:=val;
      NotifyChange(Self);
   end;
end;

// SetMaxSize
//
procedure TGLPointParameters.SetMaxSize(const val : Single);
begin
   if val<>FMaxSize then begin
      if val<0 then
         FMaxSize:=0
      else FMaxSize:=val;
      NotifyChange(Self);
   end;
end;

// SetFadeTresholdSize
//
procedure TGLPointParameters.SetFadeTresholdSize(const val : Single);
begin
   if val<>FFadeTresholdSize then begin
      if val<0 then
         FFadeTresholdSize:=0
      else FFadeTresholdSize:=val;
      NotifyChange(Self);
   end;
end;

// SetDistanceAttenuation
//
procedure TGLPointParameters.SetDistanceAttenuation(const val : TGLCoordinates);
begin
   FDistanceAttenuation.Assign(val);
end;

// ------------------
// ------------------ TGLPoints ------------------
// ------------------

// Create
//
constructor TGLPoints.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FStyle:=psSquare;
   FSize:=cDefaultPointSize;
   FPositions:=TAffineVectorList.Create;
   FColors:=TVectorList.Create;
   FPointParameters:=TGLPointParameters.Create(Self);
end;

// Destroy
//
destructor TGLPoints.Destroy;
begin
   FPointParameters.Free;
   FColors.Free;
   FPositions.Free;
   inherited;
end;

// Assign
//
procedure TGLPoints.Assign(Source : TPersistent);
begin
	if Source is TGLPoints then begin
      FSize:=TGLPoints(Source).FSize;
      FStyle:=TGLPoints(Source).FStyle;
      FPositions.Assign(TGLPoints(Source).FPositions);
      FColors.Assign(TGLPoints(Source).FColors);
      StructureChanged
	end;
	inherited Assign(Source);
end;

// BuildList
//
procedure TGLPoints.BuildList(var rci : TRenderContextInfo);
var
   n : Integer;
   v : TVector;
begin
   n:=FPositions.Count;
   case FColors.Count of
      0 : glColor4f(1, 1, 1, 1);
      1 : glColor4fv(PGLFloat(FColors.List));
   else
      if FColors.Count<n then
         n:=FColors.Count;
      glColorPointer(4, GL_FLOAT, 0, FColors.List);
      glEnableClientState(GL_COLOR_ARRAY);
   end;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   if n=0 then begin
      v:=NullHmgPoint;
      glVertexPointer(3, GL_FLOAT, 0, @v);
      n:=1;
   end else glVertexPointer(3, GL_FLOAT, 0, FPositions.List);
   glEnableClientState(GL_VERTEX_ARRAY);
   if NoZWrite then
      glDepthMask(False);
   glPointSize(FSize);
   PointParameters.Apply;
   if GL_EXT_compiled_vertex_array and (n>64) then
      glLockArraysEXT(0, n);
   case FStyle of
      psSquare : begin
         // square point (simplest method, fastest)
         glDisable(GL_BLEND);
      end;
      psRound : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_GREATER, 0.5);
         glDisable(GL_BLEND);
      end;
      psSmooth : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_NOTEQUAL, 0.0);
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      psSmoothAdditive : begin
         glEnable(GL_POINT_SMOOTH);
         glEnable(GL_ALPHA_TEST);
         glAlphaFunc(GL_NOTEQUAL, 0.0);
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
      psSquareAdditive : begin
         glEnable(GL_BLEND);
         glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      end;
   else
      Assert(False);
   end;
   glDrawArrays(GL_POINTS, 0, n);
   if GL_EXT_compiled_vertex_array and (n>64) then
      glUnlockArraysEXT;
   PointParameters.UnApply;
   if NoZWrite then
      glDepthMask(True);
   glDisableClientState(GL_VERTEX_ARRAY);
   if FColors.Count>1 then
      glDisableClientState(GL_COLOR_ARRAY);
   glPopAttrib;
   // restore default GLScene AlphaFunc
   glAlphaFunc(GL_GREATER, 0);
end;

// StoreSize
//
function TGLPoints.StoreSize : Boolean;
begin
   Result:=(FSize<>cDefaultPointSize);
end;

// SetNoZWrite
//
procedure TGLPoints.SetNoZWrite(const val : Boolean);
begin
   if FNoZWrite<>val then begin
      FNoZWrite:=val;
      StructureChanged;
   end;
end;

// SetStatic
//
procedure TGLPoints.SetStatic(const val : Boolean);
begin
   if FStatic<>val then begin
      FStatic:=val;
      if val then
         ObjectStyle:=ObjectStyle-[osDirectDraw]
      else ObjectStyle:=ObjectStyle+[osDirectDraw];
      StructureChanged;
   end;
end;

// SetSize
//
procedure TGLPoints.SetSize(const val : Single);
begin
   if FSize<>val then begin
      FSize:=val;
      StructureChanged;
   end;
end;

// SetPositions
//
procedure TGLPoints.SetPositions(const val : TAffineVectorList);
begin
   FPositions.Assign(val);
   StructureChanged;
end;

// SetColors
//
procedure TGLPoints.SetColors(const val : TVectorList);
begin
   FColors.Assign(val);
   StructureChanged;
end;

// SetStyle
//
procedure TGLPoints.SetStyle(const val : TGLPointStyle);
begin
   if FStyle<>val then begin
      FStyle:=val;
      StructureChanged;
   end;
end;

// SetPointParameters
//
procedure TGLPoints.SetPointParameters(const val : TGLPointParameters);
begin
   FPointParameters.Assign(val);
end;

// ------------------
// ------------------ TGLLineBase ------------------
// ------------------

// Create
//
constructor TGLLineBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FLineColor:=TGLColor.Create(Self);
   FLineColor.Initialize(clrWhite);
   FLinePattern:=$FFFF;
   FAntiAliased:=False;
   FLineWidth:=1.0;
end;

// Destroy
//
destructor TGLLineBase.Destroy;
begin
   FLineColor.Free;
   inherited Destroy;
end;

// SetLineColor
//
procedure TGLLineBase.SetLineColor(const value: TGLColor);
begin
   FLineColor.Color:=Value.Color;
   StructureChanged;
end;

// SetLinePattern
//
procedure TGLLineBase.SetLinePattern(const value: TGLushort);
begin
   if FLinePattern<>value then begin
      FLinePattern:=Value;
      StructureChanged;
   end;
end;

// SetLineWidth
//
procedure TGLLineBase.SetLineWidth(const val : Single);
begin
   if FLineWidth<>val then begin
      FLineWidth:=val;
      StructureChanged;
   end;
end;

// StoreLineWidth
//
function TGLLineBase.StoreLineWidth : Boolean;
begin
   Result:=(FLineWidth<>1.0);
end;

// SetAntiAliased
//
procedure TGLLineBase.SetAntiAliased(const val : Boolean);
begin
   if FAntiAliased<>val then begin
      FAntiAliased:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLLineBase.Assign(Source: TPersistent);
begin
   if Source is TGLLineBase then begin
      LineColor:=TGLLineBase(Source).FLineColor;
      LinePattern:=TGLLineBase(Source).FLinePattern;
      LineWidth:=TGLLineBase(Source).FLineWidth;
      AntiAliased:=TGLLineBase(Source).FAntiAliased;
   end else inherited Assign(Source);
end;

// SetupLineStyle
//
procedure TGLLineBase.SetupLineStyle;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   if FLinePattern<>$FFFF then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, FLinePattern);
   end;
   if FAntiAliased then begin
      glEnable(GL_LINE_SMOOTH);
      glEnable(GL_BLEND);
   	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   end else glDisable(GL_LINE_SMOOTH);
   glLineWidth(FLineWidth);
   if FLineColor.Alpha<>1 then begin
      if not FAntiAliased then begin
         glEnable(GL_BLEND);
      	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      end;
      glColor4fv(FLineColor.AsAddress);
   end else glColor3fv(FLineColor.AsAddress);
end;

// RestoreLineStyle
//
procedure TGLLineBase.RestoreLineStyle;
begin
   glPopAttrib;
end;

// ------------------
// ------------------ TGLLinesNode ------------------
// ------------------

// Create
//
constructor TGLLinesNode.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FColor:=TGLColor.Create(Self);
   FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TGLLines).NodeColor.Color);
   FColor.OnNotifyChange:=OnColorChange;
end;

// Destroy
//
destructor TGLLinesNode.Destroy;
begin
   FColor.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLLinesNode.Assign(Source: TPersistent);
begin
	if Source is TGLLinesNode then 
      FColor.Assign(TGLLinesNode(Source).FColor);
	inherited;
end;

// SetColor
//
procedure TGLLinesNode.SetColor(const val : TGLColor);
begin
   FColor.Assign(val);
end;

// OnColorChange
//
procedure TGLLinesNode.OnColorChange(sender : TObject);
begin
   (Collection as TGLNodes).NotifyChange;
end;

// StoreColor
//
function TGLLinesNode.StoreColor : Boolean;
begin
   Result:=not VectorEquals((TGLLinesNodes(Collection).GetOwner as TGLLines).NodeColor.Color,
                            FColor.Color);
end;

// ------------------
// ------------------ TGLLinesNodes ------------------
// ------------------

// Create
//
constructor TGLLinesNodes.Create(AOwner : TComponent);
begin
   inherited Create(AOwner, TGLLinesNode);
end;

// NotifyChange
//
procedure TGLLinesNodes.NotifyChange;
begin
   if (GetOwner<>nil) then
      (GetOwner as TGLBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TGLNodedLines ------------------
// ------------------

// Create
//
constructor TGLNodedLines.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FNodes:=TGLLinesNodes.Create(Self);
   FNodeColor:=TGLColor.Create(Self);
   FNodeColor.Initialize(clrBlue);
   FNodeColor.OnNotifyChange:=OnNodeColorChanged;
   FOldNodeColor:=clrBlue;
   FNodesAspect:=lnaAxes;
   FNodeSize:=1;
end;

// Destroy
//
destructor TGLNodedLines.Destroy;
begin
   FNodes.Free;
   FNodeColor.Free;
   inherited Destroy;
end;

// SetNodesAspect
//
procedure TGLNodedLines.SetNodesAspect(const value : TLineNodesAspect);
begin
   if Value<>FNodesAspect then begin
      FNodesAspect:=value;
      StructureChanged;
   end;
end;

// SetNodeColor
//
procedure TGLNodedLines.SetNodeColor(const value: TGLColor);
begin
   FNodeColor.Color:=Value.Color;
   StructureChanged;
end;

// OnNodeColorChanged
//
procedure TGLNodedLines.OnNodeColorChanged(sender : TObject);
var
   i : Integer;
begin
   // update color for nodes...
   for i:=0 to Nodes.Count-1 do
      if VectorEquals(TGLLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
         TGLLinesNode(Nodes[i]).Color.Assign(FNodeColor);
   SetVector(FOldNodeColor, FNodeColor.Color);
end;

// SetNodes
//
procedure TGLNodedLines.SetNodes(const aNodes : TGLLinesNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetNodeSize
//
procedure TGLNodedLines.SetNodeSize(const val : Single);
begin
   if val<=0 then
      FNodeSize:=1
   else FNodeSize:=val;
   StructureChanged;
end;

// StoreNodeSize
//
function TGLNodedLines.StoreNodeSize : Boolean;
begin
   Result:=FNodeSize<>1;
end;

// Assign
//
procedure TGLNodedLines.Assign(Source: TPersistent);
begin
   if Source is TGLNodedLines then begin
      SetNodes(TGLNodedLines(Source).FNodes);
      FNodesAspect:=TGLNodedLines(Source).FNodesAspect;
      FNodeColor.Color:=TGLNodedLines(Source).FNodeColor.Color;
      FNodeSize:= TGLNodedLines(source).FNodeSize;
   end;
   inherited Assign(Source);
end;

// DrawNode
//
procedure TGLNodedLines.DrawNode(var rci : TRenderContextInfo; X, Y, Z: Single; Color: TGLColor);
begin
   glPushMatrix;
   glTranslatef(x, y, z);
   case NodesAspect of
      lnaAxes :
         AxesBuildList(rci, $CCCC, FNodeSize*0.5);
      lnaCube :
         CubeWireframeBuildList(rci, FNodeSize, False, Color.Color);
      lnaDodecahedron : begin
         if FNodeSize<>1 then begin
            glPushMatrix;
            glScalef(FNodeSize, FNodeSize, FNodeSize);
            rci.GLStates.SetGLMaterialColors(GL_FRONT, clrBlack, clrGray20, Color.Color, clrBlack, 0);
            DodecahedronBuildList;
            glPopMatrix;
         end else begin
            rci.GLStates.SetGLMaterialColors(GL_FRONT, clrBlack, clrGray20, Color.Color, clrBlack, 0);
            DodecahedronBuildList;
         end;
      end;
   else
      Assert(False)
   end;
   glPopMatrix;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLNodedLines.AxisAlignedDimensionsUnscaled : TVector;
var
   i : Integer;
begin
   RstVector(Result);
   for i:=0 to Nodes.Count-1 do
      MaxVector(Result, VectorAbs(Nodes[i].AsVector));
   // EG: commented out, line below looks suspicious, since scale isn't taken
   //     into account in previous loop, must have been hiding another bug... somewhere...
   //DivideVector(Result, Scale.AsVector);     //DanB ?
end;

// AddNode (coords)
//
procedure TGLNodedLines.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLNodedLines.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TGLNodedLines.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLNodedLines.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

// ------------------
// ------------------ TGLLines ------------------
// ------------------

// Create
//
constructor TGLLines.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FDivision:=10;
   FSplineMode:=lsmLines;
   FNURBSKnots:=TSingleList.Create;
   FNURBSOrder:=0;
   FNURBSTolerance:=50;
end;

// Destroy
//
destructor TGLLines.Destroy;
begin
   FNURBSKnots.Free;
   inherited Destroy;
end;

// SetDivision
//
procedure TGLLines.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetOptions
//
procedure TGLLines.SetOptions(const val : TLinesOptions);
begin
   FOptions:=val;
   StructureChanged;
end;

// SetSplineMode
//
procedure TGLLines.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// SetNURBSOrder
//
procedure TGLLines.SetNURBSOrder(const val : Integer);
begin
   if val<>FNURBSOrder then begin
      FNURBSOrder:=val;
      StructureChanged;
   end;
end;

// SetNURBSTolerance
//
procedure TGLLines.SetNURBSTolerance(const val : Single);
begin
   if val<>FNURBSTolerance then begin
      FNURBSTolerance:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLLines.Assign(Source: TPersistent);
begin
   if Source is TGLLines then begin
      FDivision:=TGLLines(Source).FDivision;
      FSplineMode:=TGLLines(Source).FSplineMode;
      FOptions:= TGLLines(source).FOptions;
   end;
   inherited Assign(Source);
end;

// BuildList
//
procedure TGLLines.BuildList(var rci : TRenderContextInfo);
var
   i, n : Integer;
   a, b, c : TGLFloat;
   f : Single;
   spline : TCubicSpline;
   vertexColor : TVector;
   nodeBuffer : array of TAffineVector;
   colorBuffer : array of TVector;
   nurbsRenderer : PGLUNurbs;
begin
   if Nodes.Count>1 then begin
      // first, we setup the line color & stippling styles
      SetupLineStyle;

      // Set up the control point buffer for Bezier splines and NURBS curves.
      // If required this could be optimized by storing a cached node buffer.
      if (FSplineMode=lsmBezierSpline) or (FSplineMode=lsmNURBSCurve) then begin
         SetLength(nodeBuffer, Nodes.Count);
         SetLength(colorBuffer, Nodes.Count);
         for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do begin
            nodeBuffer[i]:=AsAffineVector;
            colorBuffer[i]:=Color.Color;
         end;
      end;

      if FSplineMode=lsmBezierSpline then begin
         // map evaluator
         glPushAttrib(GL_EVAL_BIT);
         glEnable(GL_MAP1_VERTEX_3);
         glEnable(GL_MAP1_COLOR_4);

         glMap1f(GL_MAP1_VERTEX_3, 0, 1, 3, Nodes.Count, @nodeBuffer[0]);
         glMap1f(GL_MAP1_COLOR_4, 0, 1, 4, Nodes.Count, @colorBuffer[0]);
      end;

      // start drawing the line
      if (FSplineMode=lsmNURBSCurve) and (FDivision>=2) then begin
         if (FNURBSOrder>0) and (FNURBSKnots.Count>0) then begin
            nurbsRenderer:=gluNewNurbsRenderer;
            try
               gluNurbsProperty(nurbsRenderer, GLU_SAMPLING_TOLERANCE, FNURBSTolerance);
               gluNurbsProperty(nurbsRenderer, GLU_DISPLAY_MODE, GLU_FILL);
               gluBeginCurve(nurbsRenderer);
                  gluNurbsCurve(nurbsRenderer,
                                FNURBSKnots.Count, @FNURBSKnots.List[0],
                                3, @nodeBuffer[0],
                                FNURBSOrder,
                                GL_MAP1_VERTEX_3);
               gluEndCurve(nurbsRenderer);
            finally
               gluDeleteNurbsRenderer(nurbsRenderer);
            end;
         end;
      end else begin
         // lines, cubic splines or bezier
         if FSplineMode=lsmSegments then
            glBegin(GL_LINES)
         else glBegin(GL_LINE_STRIP);
         if (FDivision<2) or (FSplineMode in [lsmLines, lsmSegments]) then begin
            // standard line(s), draw directly
            if loUseNodeColorForLines in Options then begin
               // node color interpolation
               for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do begin
                  glColor4fv(Color.AsAddress);
                  glVertex3f(X, Y, Z);
               end;
            end else begin
               // single color
               for i:=0 to Nodes.Count-1 do with Nodes[i] do
                  glVertex3f(X, Y, Z);
            end;
         end else if FSplineMode=lsmCubicSpline then begin
            // cubic spline
            spline:=Nodes.CreateNewCubicSpline;
            try
               f:=1/FDivision;
               for i:=0 to (Nodes.Count-1)*FDivision  do begin
                  Spline.SplineXYZ(i*f, a, b, c);
                  if loUseNodeColorForLines in Options then begin
                     n:=(i div FDivision);
                     if n<Nodes.Count-1 then
                        VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                                   TGLLinesNode(Nodes[n+1]).Color.Color,
                                   (i mod FDivision)*f, vertexColor)
                     else SetVector(vertexColor, TGLLinesNode(Nodes[Nodes.Count-1]).Color.Color);
                     glColor4fv(@vertexColor);
                  end;
                  glVertex3f(a, b, c);
               end;
            finally
               spline.Free;
            end;
         end else if FSplineMode=lsmBezierSpline then begin
            f:=1/FDivision;
            for i:=0 to FDivision do
               glEvalCoord1f(i*f);
         end;
         glEnd;
      end;

      if FSplineMode=lsmBezierSpline then
         glPopAttrib;
      if Length(nodeBuffer)>0 then begin
        SetLength(nodeBuffer, 0);
        SetLength(colorBuffer, 0);
      end;

      RestoreLineStyle;

      if FNodesAspect<>lnaInvisible then begin
         glPushAttrib(GL_ENABLE_BIT);
         if not rci.ignoreBlendingRequests then begin
            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
         end;
         glDisable(GL_TEXTURE_2D);
         if GL_ARB_texture_cube_map then
            glDisable(GL_TEXTURE_CUBE_MAP_ARB);
         for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do
            DrawNode(rci, X, Y, Z, Color);
         glPopAttrib;
      end;
   end;
end;

// ------------------
// ------------------ TGLCube ------------------
// ------------------

// Create
//
constructor TGLCube.Create(AOwner:Tcomponent);
begin
  inherited Create(AOwner);
  FCubeSize:=XYZVector;
  FParts:=[cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection:=ndOutside;
  ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// BuildList
//
procedure TGLCube.BuildList(var rci : TRenderContextInfo);
var
	hw, hh, hd, nd  : TGLFloat;
begin
   if FNormalDirection=ndInside then
      nd:=-1
   else nd:=1;
   hw:=FCubeSize[0]*0.5;
   hh:=FCubeSize[1]*0.5;
   hd:=FCubeSize[2]*0.5;

   glBegin(GL_QUADS);
   if cpFront in FParts then begin
      glNormal3f(  0,  0, nd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw,  hh, hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw,  hh, hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, hd);
   end;
   if cpBack in FParts then begin
      glNormal3f(  0,  0, -nd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f( hw,  hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw,  hh, -hd);
   end;
   if cpLeft in FParts then begin
      glNormal3f(-nd,  0,  0);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw,  hh,  hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw,  hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh,  hd);
   end;
   if cpRight in FParts then begin
      glNormal3f(nd,  0,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(hw,  hh,  hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(hw, -hh,  hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(hw,  hh, -hd);
   end;
   if cpTop in FParts then begin
      glNormal3f(  0, nd,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, hh,  hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, hh,  hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, hh, -hd);
   end;
   if cpBottom in FParts then begin
      glNormal3f(  0, -nd,  0);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, -hh,  hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, -hh,  hd);
   end;
   glEnd;
end;

// GenerateSilhouette
//
function TGLCube.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
	hw, hh, hd : TGLFloat;
   connectivity : TConnectivity;
   sil : TGLSilhouette;
begin
   Connectivity := TConnectivity.Create(true);

   hw:=FCubeSize[0]*0.5;
   hh:=FCubeSize[1]*0.5;
   hd:=FCubeSize[2]*0.5;

   if cpFront in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake( hw,  hh, hd),
        AffineVectorMake(-hw,  hh, hd),
        AffineVectorMake(-hw, -hh, hd),
        AffineVectorMake( hw, -hh, hd));
   end;
   if cpBack in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(hw,  hh, -hd),
        AffineVectorMake( hw, -hh, -hd),
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake(-hw,  hh, -hd));
   end;
   if cpLeft in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw,  hh,  hd),
        AffineVectorMake(-hw,  hh, -hd),
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake(-hw, -hh,  hd));
   end;
   if cpRight in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(hw,  hh,  hd),
        AffineVectorMake(hw, -hh,  hd),
        AffineVectorMake(hw, -hh, -hd),
        AffineVectorMake(hw,  hh, -hd));
   end;
   if cpTop in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw, hh, -hd),
        AffineVectorMake(-hw, hh,  hd),
        AffineVectorMake( hw, hh,  hd),
        AffineVectorMake( hw, hh, -hd));
   end;
   if cpBottom in FParts then begin
      Connectivity.AddQuad(
        AffineVectorMake(-hw, -hh, -hd),
        AffineVectorMake( hw, -hh, -hd),
        AffineVectorMake( hw, -hh,  hd),
        AffineVectorMake(-hw, -hh,  hd));
   end;

   sil := nil;
   Connectivity.CreateSilhouette(
      silhouetteParameters, sil, false);

   result := sil;

   Connectivity.Free;
end;

// SetCubeWidth
//
procedure TGLCube.SetCubeWidth(const aValue : Single);
begin
   if aValue<>FCubeSize[0] then begin
      FCubeSize[0]:=aValue;
      StructureChanged;
   end;
end;

// SetCubeHeight
//
procedure TGLCube.SetCubeHeight(const aValue : Single);
begin
   if aValue<>FCubeSize[1] then begin
      FCubeSize[1]:=aValue;
      StructureChanged;
   end;
end;

// SetCubeDepth
//
procedure TGLCube.SetCubeDepth(const aValue : Single);
begin
   if aValue<>FCubeSize[2] then begin
      FCubeSize[2]:=aValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TGLCube.SetParts(aValue:TCubeParts);
begin
   if aValue<>FParts then begin
      FParts:=aValue;
      StructureChanged;
   end;
end;

// SetNormalDirection
//
procedure TGLCube.SetNormalDirection(aValue: TNormalDirection);
begin
   if aValue<>FNormalDirection then begin
      FNormalDirection:=aValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLCube.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLCube) then begin
      FCubeSize:=TGLCube(Source).FCubeSize;
      FParts:=TGLCube(Source).FParts;
      FNormalDirection:=TGLCube(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLCube.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=FCubeSize[0]*0.5;
   Result[1]:=FCubeSize[1]*0.5;
   Result[2]:=FCubeSize[2]*0.5;
   Result[3]:=0;
end;

// RayCastIntersect
//
function TGLCube.RayCastIntersect(const rayStart, rayVector : TVector;
                                  intersectPoint : PVector = nil;
                                  intersectNormal : PVector = nil) : Boolean;
var
   p     : array [0..5] of TVector;
   rv    : TVector;
   rs, r : TVector;
   i     : Integer;
   t, e  : Single;
   eSize : TAffineVector;
begin
   rs:=AbsoluteToLocal(rayStart);
   SetVector(rv, VectorNormalize(AbsoluteToLocal(rayVector)));
   e:=0.5+0.0001; //Small value for floating point imprecisions
   eSize[0]:=FCubeSize[0]*e;
   eSize[1]:=FCubeSize[1]*e;
   eSize[2]:=FCubeSize[2]*e;
   p[0]:=XHmgVector;
   p[1]:=YHmgVector;
   p[2]:=ZHmgVector;
   SetVector(p[3], -1,  0,  0);
   SetVector(p[4],  0, -1,  0);
   SetVector(p[5],  0,  0, -1);
   for i:=0 to 5 do begin
      if VectorDotProduct(p[i], rv)>0 then begin
         t:=- (p[i][0]*rs[0]+p[i][1]*rs[1]+p[i][2]*rs[2]+0.5*FCubeSize[i mod 3])
             /(p[i][0]*rv[0]+p[i][1]*rv[1]+p[i][2]*rv[2]);
         MakePoint(r, rs[0]+t*rv[0], rs[1]+t*rv[1], rs[2]+t*rv[2]);
         if  (Abs(r[0])<=eSize[0]) 
         and (Abs(r[1])<=eSize[1]) 
         and (Abs(r[2])<=eSize[2])
         and (VectorDotProduct(VectorSubtract(r,rs),rv)>0) then begin
            if Assigned(intersectPoint) then
               MakePoint(intersectPoint^, LocalToAbsolute(r));
            if Assigned(intersectNormal) then
               MakeVector(intersectNormal^, LocalToAbsolute(VectorNegate(p[i])));
            Result:=True;
            Exit;
         end;
      end;
   end;
   Result:=False;
end;

// DefineProperties
//
procedure TGLCube.DefineProperties(Filer: TFiler);
begin
   inherited;
   Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
                              (FCubeSize[0]<>1) or (FCubeSize[1]<>1) or (FCubeSize[2]<>1));
end;

// ReadData
//
procedure TGLCube.ReadData(Stream: TStream);
begin
   with Stream do begin
      Read(FCubeSize, SizeOf(TAffineVector));
   end;
end;

// WriteData
//
procedure TGLCube.WriteData(Stream: TStream);
begin
   with Stream do begin
      Write(FCubeSize, SizeOf(TAffineVector));
   end;
end;

// ------------------
// ------------------ TGLQuadricObject ------------------
// ------------------

// Create
//
constructor TGLQuadricObject.Create(AOwner : TComponent);
begin
   inherited;
   FNormals:=nsSmooth;
   FNormalDirection:=ndOutside;
end;

// SetNormals
//
procedure TGLQuadricObject.SetNormals(aValue : TNormalSmoothing);
begin
	if aValue<>FNormals then begin
		FNormals:=aValue;
		StructureChanged;
	end;
end;

// SetNormalDirection
//
procedure TGLQuadricObject.SetNormalDirection(aValue : TNormalDirection);
begin
	if aValue<>FNormalDirection then begin
		FNormalDirection:=aValue;
		StructureChanged;
  	end;
end;

// SetupQuadricParams
//
procedure TGLQuadricObject.SetupQuadricParams(quadric : PGLUquadricObj);
const
   cNormalSmoothinToEnum : array [nsFlat..nsNone] of TGLEnum = (
         GLU_FLAT, GLU_SMOOTH, GLU_NONE );
begin
	gluQuadricDrawStyle(Quadric, GLU_FILL);
	gluQuadricNormals(Quadric, cNormalSmoothinToEnum[FNormals]);
   SetNormalQuadricOrientation(Quadric);
	gluQuadricTexture(Quadric, True);
end;

// SetNormalQuadricOrientation
//
procedure TGLQuadricObject.SetNormalQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_INSIDE, GLU_OUTSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//
procedure TGLQuadricObject.SetInvertedQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_OUTSIDE, GLU_INSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//
procedure TGLQuadricObject.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TGLQuadricObject) then begin
      FNormals:=TGLQuadricObject(Source).FNormals;
      FNormalDirection:=TGLQuadricObject(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

// ------------------
// ------------------ TGLSphere ------------------
// ------------------

// Create
//
constructor TGLSphere.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FRadius:=0.5;
   FSlices:=16;
   FStacks:=16;
   FTop:=90;
   FBottom:=-90;
   FStart:=0;
   FStop:=360;
end;

// BuildList
//
procedure TGLSphere.BuildList(var rci : TRenderContextInfo);
var
   V1, V2, N1 : TAffineVector;
   AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
   SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
   uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
   I, J: Integer;
   DoReverse: Boolean;
begin         
   DoReverse:=(FNormalDirection=ndInside);
   glPushAttrib(GL_POLYGON_BIT);
   if DoReverse then
      rci.GLStates.InvertGLFrontFace;

   // common settings
   AngTop:=DegToRad(FTop);
   AngBottom:=DegToRad(FBottom);
   AngStart:=DegToRad(FStart);
   AngStop:=DegToRad(FStop);
   StepH:=(AngStop - AngStart) / FSlices;
   StepV:=(AngTop - AngBottom) / FStacks;
   glPushMatrix;
   glScalef(Radius, Radius, Radius);

   // top cap
   if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngTop, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, -1, 0)
      else glNormal3f(0, 1, 0);
      if FTopCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         N1:=YVector;
         if DoReverse then
            N1[1]:=-N1[1];
      end;
      V1[1]:=SinP;
      Theta:=AngStart;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP*SinT;
         V1[2]:=CosP*CosT;
         if FTopCap=ctCenter then begin
            N1:=VectorPerpendicular(YVector, V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT*0.5+0.5, CosT*0.5+0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta + StepH;
      end;
      glEnd;
   end;

   // main body
   Phi:=AngTop;
   Phi2:=Phi-StepV;
   uTexFactor:=1/FSlices;
   vTexFactor:=1/FStacks;

   for J:=0 to FStacks-1 do begin
      Theta:=AngStart;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      V1[1]:=SinP;
      V2[1]:=SinP2;
      vTexCoord0:=1-j*vTexFactor;
      vTexCoord1:=1-(j+1)*vTexFactor;

      glBegin(GL_TRIANGLE_STRIP);
      for i:=0 to FSlices do begin

         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V2[0]:=CosP2 * SinT;
         V1[2]:=CosP * CosT;
         V2[2]:=CosP2 * CosT;

         uTexCoord:=i*uTexFactor;
         xglTexCoord2f(uTexCoord, vTexCoord0);
         if DoReverse then begin
            N1:=VectorNegate(V1);
            glNormal3fv(@N1);
         end else glNormal3fv(@V1);
         glVertex3fv(@V1);

         xglTexCoord2f(uTexCoord, vTexCoord1);
         if DoReverse then begin
            N1:=VectorNegate(V2);
            glNormal3fv(@N1);
         end else glNormal3fv(@V2);
         glVertex3fv(@V2);

         Theta:=Theta+StepH;
      end;
      glEnd;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;

   // bottom cap
   if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngBottom, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, 1, 0)
      else glNormal3f(0, -1, 0);
      if FBottomCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1[1]:=SinP;
      Theta:=AngStop;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V1[2]:=CosP * CosT;
         if FTopCap = ctCenter then begin
            N1:=VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT*0.5+0.5, CosT*0.5+0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta - StepH;
      end;
      glEnd;
   end;
   if DoReverse then
      rci.GLStates.InvertGLFrontFace;
   glPopMatrix;
   glPopAttrib;
end;

// RayCastIntersect
//
function TGLSphere.RayCastIntersect(const rayStart, rayVector : TVector;
                                 intersectPoint : PVector = nil;
                                 intersectNormal : PVector = nil) : Boolean;
var
   i1, i2 : TVector;
   localStart, localVector : TVector;
begin
   // compute coefficients of quartic polynomial
   SetVector(localStart,  AbsoluteToLocal(rayStart));
   SetVector(localVector, AbsoluteToLocal(rayVector));
   NormalizeVector(localVector);
   if RayCastSphereIntersect(localStart, localVector, NullHmgVector, Radius, i1, i2)>0 then begin
      Result:=True;
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^,  LocalToAbsolute(i1));
      if Assigned(intersectNormal) then begin
         i1[3]:=0; // vector transform
         SetVector(intersectNormal^, LocalToAbsolute(i1));
      end;
   end else Result:=False;
end;

// GenerateSilhouette
//
function TGLSphere.GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette;
var
   i, j : Integer;
   s, c, angleFactor : Single;
   sVec, tVec : TAffineVector;
   Segments : integer;
begin
   Segments := MaxInteger(FStacks, FSlices);

   // determine a local orthonormal matrix, viewer-oriented
   sVec:=VectorCrossProduct(silhouetteParameters.SeenFrom, XVector);
   if VectorLength(sVec)<1e-3 then
      sVec:=VectorCrossProduct(silhouetteParameters.SeenFrom, YVector);
   tVec:=VectorCrossProduct(silhouetteParameters.SeenFrom, sVec);
   NormalizeVector(sVec);
   NormalizeVector(tVec);
   // generate the silhouette (outline and capping)
   Result:=TGLSilhouette.Create;
   angleFactor:=(2*PI)/Segments;
   for i:=0 to Segments-1 do begin
      SinCos(i*angleFactor, FRadius, s, c);
      Result.Vertices.AddPoint(VectorCombine(sVec, tVec, s, c));
      j:=(i+1) mod Segments;
      Result.Indices.Add(i, j);
      if silhouetteParameters.CappingRequired then
         Result.CapIndices.Add(Segments, i, j)
   end;
   if silhouetteParameters.CappingRequired then
      Result.Vertices.Add(NullHmgPoint);
end;

// SetBottom
//
procedure TGLSphere.SetBottom(aValue : TAngleLimit1);
begin
   if FBottom<>aValue then begin
      FBottom:=aValue;
      StructureChanged;
   end;
end;

// SetBottomCap
//
procedure TGLSphere.SetBottomCap(aValue : TCapType);
begin
   if FBottomCap<>aValue then begin
      FBottomCap:=aValue;
      StructureChanged;
   end;
end;

// SetRadius
//
procedure TGLSphere.SetRadius(const aValue : TGLFloat);
begin
   if aValue<>FRadius then begin
      FRadius:=aValue;
      StructureChanged;
   end;
end;

// SetSlices
//
procedure TGLSphere.SetSlices(aValue : Integer);
begin
   if aValue<>FSlices then begin
      if aValue<=0 then
         FSlices:=1
      else FSlices:=aValue;
      StructureChanged;
  end;
end;

// SetStacks
//
procedure TGLSphere.SetStacks(aValue : TGLInt);
begin
   if aValue<>FStacks then begin
      if aValue<=0 then
         FStacks:=1
      else FStacks:=aValue;
      StructureChanged;
   end;
end;

// SetStart
//
procedure TGLSphere.SetStart(aValue : TAngleLimit2);
begin
   if FStart<>aValue then begin
      Assert(aValue<=FStop);
      FStart:=aValue;
      StructureChanged;
   end;
end;

// SetStop
//
procedure TGLSphere.SetStop(aValue : TAngleLimit2);
begin
   if FStop<>aValue then begin
      Assert(aValue>=FStart);
      FStop:=aValue;
      StructureChanged;
   end;
end;

// SetTop
//
procedure TGLSphere.SetTop(aValue : TAngleLimit1);
begin
   if FTop<>aValue then begin
      FTop:=aValue;
      StructureChanged;
   end;
end;

// SetTopCap
//
procedure TGLSphere.SetTopCap(aValue : TCapType);
begin
   if FTopCap<>aValue then begin
      FTopCap:=aValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLSphere.Assign(Source : TPersistent);
begin
   if Assigned(Source) and (Source is TGLSphere) then begin
      FRadius:=TGLSphere(Source).FRadius;
      FSlices:=TGLSphere(Source).FSlices;
      FStacks:=TGLSphere(Source).FStacks;
      FBottom:=TGLSphere(Source).FBottom;
      FTop:=TGLSphere(Source).FTop;
      FStart:=TGLSphere(Source).FStart;
      FStop:=TGLSphere(Source).FStop;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLSphere.AxisAlignedDimensionsUnscaled : TVector;
begin
   Result[0]:=Abs(FRadius);
   Result[1]:=Result[0];
   Result[2]:=Result[0];
   Result[3]:=0;
end;

// ------------------
// ------------------ TGLPolygonBase ------------------
// ------------------

// Create
//
constructor TGLPolygonBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   CreateNodes;
   FDivision:=10;
   FSplineMode:=lsmLines;
end;

// CreateNodes
//
procedure TGLPolygonBase.CreateNodes;
begin
   FNodes:=TGLNodes.Create(Self);
end;

// Destroy
//
destructor TGLPolygonBase.Destroy;
begin
   FNodes.Free;
   inherited Destroy;
end;

// Assign
//
procedure TGLPolygonBase.Assign(Source: TPersistent);
begin
   if Source is TGLPolygonBase then begin
      SetNodes(TGLPolygonBase(Source).FNodes);
      FDivision:=TGLPolygonBase(Source).FDivision;
      FSplineMode:=TGLPolygonBase(Source).FSplineMode;
   end;
   inherited Assign(Source);
end;

// NotifyChange
//
procedure TGLPolygonBase.NotifyChange(Sender : TObject);
begin
   if Sender=Nodes then StructureChanged;
   inherited;
end;

// SetDivision
//
procedure TGLPolygonBase.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetNodes
//
procedure TGLPolygonBase.SetNodes(const aNodes : TGLNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetSplineMode
//
procedure TGLPolygonBase.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// AddNode (coords)
//
procedure TGLPolygonBase.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TGLPolygonBase.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TGLPolygonBase.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TGLPolygonBase.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLSphere, TGLCube, TGLPlane, TGLSprite, TGLPoints,
                    TGLDummyCube, TGLLines]);

end.
