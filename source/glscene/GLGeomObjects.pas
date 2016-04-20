// GLGeomObjects
{: Geometric objects.<p>

	<b>History : </b><font size=-1><ul>

      <li>25/09/04 - Eric Pascual - Added AxisAlignedBoundingBox,AxisAlignedBoundingBoxUnscaled,AxisAlignedDimensionsUnscaled
      <li>29/11/03 - MF - Added shadow silhouette code for TGLCylinderBase et al.
        Added GetTopRadius to facilitate silhouette.
      <li>24/10/03 - NelC - Fixed TGLTorus texture coord. bug
      <li>21/07/03 - EG - Creation from GLObjects split
   </ul></font>
}
unit GLGeomObjects;

interface

uses Classes, GLScene, GLTexture, VectorGeometry, OpenGL1x, GLMisc, GLObjects,
  GLSilhouette, VectorTypes,GeometryBB;

type

   // TGLDisk
   //
   {: A Disk object.<p>
      The disk may not be complete, it can have a hole (controled by the
      InnerRadius property) and can only be a slice (controled by the StartAngle
      and SweepAngle properties). }
   TGLDisk = class(TGLQuadricObject)
      private
         { Private Declarations }
         FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius : TGLFloat;
         FSlices, FLoops : TGLInt;
         procedure SetOuterRadius(const aValue : Single);
         procedure SetInnerRadius(const aValue : Single);
         procedure SetSlices(aValue: TGLInt);
         procedure SetLoops(aValue: TGLInt);
         procedure SetStartAngle(const aValue : Single);
         procedure SetSweepAngle(const aValue : Single);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
         { Published Declarations }
         {: Allows defining a "hole" in the disk. }
         property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
         {: Number of radial mesh subdivisions. }
         property Loops : TGLInt read FLoops write SetLoops default 2;
         {: Outer radius for the disk.<p>
            If you leave InnerRadius at 0, this is the disk radius. }
         property OuterRadius : TGLFloat read FOuterRadius write SetOuterRadius;
         {: Number of mesh slices.<p>
            For instance, if Slices=6, your disk will look like an hexagon. }
         property Slices : TGLInt read FSlices write SetSlices default 16;
         property StartAngle : TGLFloat read FStartAngle write SetStartAngle;
         property SweepAngle : TGLFloat read FSweepAngle write SetSweepAngle;
   end;

	// TGLCylinderBase
   //
   {: Base class to cylinder-like objects.<p>
      Introduces the basic cylinder description properties.<p>
      Be aware teh default slices and stacks make up for a high-poly cylinder,
      unless you're after high-quality lighting it is recommended to reduce the
      Stacks property to 1. }
	TGLCylinderBase = class (TGLQuadricObject)
		private
			{ Private Declarations }
			FBottomRadius : TGLFloat;
			FSlices,	FStacks, FLoops  : TGLInt;
			FHeight  : TGLFloat;

		protected
			{ Protected Declarations }
			procedure SetBottomRadius(const aValue : Single);
			procedure SetHeight(const aValue : Single);
			procedure SetSlices(aValue: TGLInt);
			procedure SetStacks(aValue: TGLInt);
			procedure SetLoops(aValue: TGLInt);
      function GetTopRadius : single; virtual;
		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

			procedure Assign(Source: TPersistent); override;

      function GenerateSilhouette(const silhouetteParameters : TGLSilhouetteParameters) : TGLSilhouette; override;
		published
			{ Published Declarations }
			property BottomRadius: TGLFloat read FBottomRadius write SetBottomRadius;
			property Height: TGLFloat read FHeight write SetHeight;
			property Slices: TGLInt read FSlices write SetSlices default 16;
			property Stacks: TGLInt read FStacks write SetStacks default 4;
			{: Number of concentric rings for top/bottom disk(s). }
			property Loops: TGLInt read FLoops write SetLoops default 1;
	end;

   // TConePart
   //
	TConePart  = (coSides, coBottom);
	TConeParts = set of TConePart;

	// TGLCone
	//
   {: A cone object. }
	TGLCone = class (TGLCylinderBase)
		private
			{ Private Declarations }
			FParts : TConeParts;

		protected
			{ Protected Declarations }
			procedure SetParts(aValue: TConeParts);
      function GetTopRadius : single; override;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;

		published
			{ Published Declarations }
			property Parts : TConeParts read FParts Write SetParts default [coSides, coBottom];
	end;

	// TCylinderPart
	//
	TCylinderPart = (cySides, cyBottom, cyTop);
	TCylinderParts = set of TCylinderPart;

   // TCylinderAlignment
   //
   TCylinderAlignment = (caCenter, caTop, caBottom);

	// TGLCylinder
	//
   {: Cylinder object, can also be used to make truncated cones }
	TGLCylinder = class(TGLCylinderBase)
		private
			{ Private Declarations }
			FParts     : TCylinderparts;
			FTopRadius : TGLFloat;
         FAlignment : TCylinderAlignment;

		protected
			{ Protected Declarations }
			procedure SetTopRadius(const aValue : Single);
			procedure SetParts(aValue : TCylinderParts);
         procedure SetAlignment(val : TCylinderAlignment);
      function GetTopRadius : single; override;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

         procedure Align(const startPoint, endPoint : TVector); overload;
         procedure Align(const startObj, endObj : TGLBaseSceneObject); overload;
         procedure Align(const startPoint, endPoint : TAffineVector); overload;

		published
			{ Published Declarations }
			property TopRadius : TGLFloat read FTopRadius write SetTopRadius;
			property Parts : TCylinderParts read FParts write SetParts default [cySides, cyBottom, cyTop];
         property Alignment : TCylinderAlignment read FAlignment write SetAlignment default caCenter;
	end;

   // TAnnulusPart
   //
   TAnnulusPart = (anInnerSides, anOuterSides, anBottom, anTop);
	TAnnulusParts = set of TAnnulusPart;

   // TGLAnnulus
   //
   {: An annulus is a cylinder that can be made hollow (pipe-like). }
   TGLAnnulus = class(TGLCylinderBase)
      private
			{ Private Declarations }
         FParts : TAnnulusParts;
         FBottomInnerRadius : TGLFloat;
         FTopInnerRadius : TGLFloat;
         FTopRadius : TGLFloat;

      protected
			{ Protected Declarations }
         procedure SetTopRadius(const aValue : Single);
         procedure SetTopInnerRadius(const aValue : Single);
         procedure SetBottomInnerRadius(const aValue : Single);
         procedure SetParts(aValue:TAnnulusParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property BottomInnerRadius: TGLFLoat read FBottomInnerRadius write SetBottomInnerRadius;
         property TopInnerRadius: TGLFloat read FTopInnerRadius write SetTopInnerRadius;
         property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
         property Parts: TAnnulusParts read FParts Write SetParts default [anInnerSides, anOuterSides, anBottom, anTop];
   end;

   // TGLTorus
   //
   {: A Torus object. }
   TGLTorus = class(TGLSceneObject)
      private
			{ Private Declarations }
         FRings, FSides : Cardinal;
         FMinorRadius, FMajorRadius  : Single;

      protected
			{ Protected Declarations }
         procedure SetMajorRadius(const aValue: Single);
         procedure SetMinorRadius(const aValue: Single);
         procedure SetRings(aValue: Cardinal);
         procedure SetSides(aValue : Cardinal);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensionsUnscaled : TVector; override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

      published
			{ Published Declarations }
         property MajorRadius: Single read FMajorRadius write SetMajorRadius;
         property MinorRadius: Single read FMinorRadius write SetMinorRadius;
         property Rings: Cardinal read FRings write SetRings default 25;
         property Sides: Cardinal read FSides write SetSides default 15;
   end;

   // TArrowLinePart
   //
   TArrowLinePart = (alLine, alTopArrow, alBottomArrow);
   TArrowLineParts = set of TArrowLinePart;

   // TArrowHeadStackingStyle
   //
   TArrowHeadStackingStyle = (ahssStacked, ahssCentered, ahssIncluded);

   // TGLArrowLine
   //
   {: Draws an arrowhead (cylinder + cone).<p>
      The arrow head is a cone that shares the attributes of the cylinder
      (ie stacks/slices, materials etc). Seems to work ok.<br>
      This is useful for displaying a vector based field (eg velocity) or
      other arrows that might be required.<br>
      By default the bottom arrow is off }
   TGLArrowLine = class(TGLCylinderBase)
      private
         { Private Declarations}
         fParts: TArrowLineParts;
         fTopRadius: Single;
         fTopArrowHeadHeight: Single;
         fTopArrowHeadRadius: Single;
         fBottomArrowHeadHeight: Single;
         fBottomArrowHeadRadius: Single;
         FHeadStackingStyle : TArrowHeadStackingStyle;

      protected
         { Protected Declarations}
         procedure SetTopRadius(const aValue : Single);
         procedure SetTopArrowHeadHeight(const aValue : Single);
         procedure SetTopArrowHeadRadius(const aValue : Single);
         procedure SetBottomArrowHeadHeight(const aValue : Single);
         procedure SetBottomArrowHeadRadius(const aValue : Single);
         procedure SetParts(aValue:TArrowLineParts);
         procedure SetHeadStackingStyle(const val : TArrowHeadStackingStyle);

      public
         { Public Declarations}
         constructor Create(AOwner:TComponent);override;
         procedure BuildList(var rci : TRenderContextInfo);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations}
         property TopRadius : TGLFloat read fTopRadius write SetTopRadius;
         property HeadStackingStyle : TArrowHeadStackingStyle read FHeadStackingStyle write SetHeadStackingStyle default ahssStacked;
         property Parts : TArrowLineParts read fParts write SetParts default [alLine, alTopArrow];
         property TopArrowHeadHeight : TGLFloat read fTopArrowHeadHeight write SetTopArrowHeadHeight;
         property TopArrowHeadRadius : TGLFloat read fTopArrowHeadRadius write SetTopArrowHeadRadius;
         property BottomArrowHeadHeight : TGLFloat read fBottomArrowHeadHeight write SetBottomArrowHeadHeight;
         property BottomArrowHeadRadius : TGLFloat read fBottomArrowHeadRadius write SetBottomArrowHeadRadius;
   end;

   // TPolygonParts
   //
   TPolygonPart = (ppTop, ppBottom);
   TPolygonParts = set of TPolygonPart;

   // TGLPolygon
   //
   {: A basic polygon object.<p>
      The curve is described by the Nodes and SplineMode properties, should be
      planar and is automatically tessellated.<p>
      Texture coordinates are deduced from X and Y coordinates only.<p>
      This object allows only for polygons described by a single curve, if you
      need "complex polygons" with holes, patches and cutouts, see GLMultiPolygon. }
   TGLPolygon = class(TGLPolygonBase)
      private
			{ Private Declarations }
         FParts : TPolygonParts;

		protected
			{ Protected Declarations }
         procedure SetParts(const val : TPolygonParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

      published
			{ Published Declarations }
         {: Parts of polygon.<p>
            The 'top' of the polygon is the position were the curve describing
            the polygon spin counter-clockwise (i.e. right handed convention). }
         property Parts : TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
   end;

   // TFrustrumParts
   //
	TFrustrumPart = (fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight);
   TFrustrumParts = set of TFrustrumPart;

const
	cAllFrustrumParts = [fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight];

type
   // TGLFrustrum
   //
   { A frustrum is a pyramid with the top chopped off.<p>
      The height of the imaginary pyramid is ApexHeight, the height of the
      frustrum is Height. If ApexHeight and Height are the same, the frustrum
      degenerates into a pyramid.<br>
      Height cannot be greater than ApexHeight. }
   TGLFrustrum = class(TGLSceneObject)
      private
			{ Private Declarations }
         FApexHeight, FBaseDepth, FBaseWidth, FHeight: TGLFloat;
         FParts: TFrustrumParts;
         FNormalDirection: TNormalDirection;
         procedure SetApexHeight(const aValue : Single);
         procedure SetBaseDepth(const aValue : Single);
         procedure SetBaseWidth(const aValue : Single);
         procedure SetHeight(const aValue : Single);
         procedure SetParts(aValue: TFrustrumParts);
         procedure SetNormalDirection(aValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci: TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;
         function TopDepth: TGLFloat;
         function TopWidth: TGLFloat;
         function AxisAlignedBoundingBox: TAABB;
         function AxisAlignedBoundingBoxUnscaled: TAABB;
         function AxisAlignedDimensionsUnscaled: TVector; override;
      published
			{ Published Declarations }
         property ApexHeight: TGLFloat read FApexHeight write SetApexHeight stored False;
         property BaseDepth: TGLFloat read FBaseDepth write SetBaseDepth stored False;
         property BaseWidth: TGLFloat read FBaseWidth write SetBaseWidth stored False;
         property Height: TGLFloat read FHeight write SetHeight stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TFrustrumParts read FParts write SetParts default cAllFrustrumParts;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses Polynomials, XOpenGL;

// ------------------
// ------------------ TGLDisk ------------------
// ------------------

// Create
//
constructor TGLDisk.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius:=0.5;
  FInnerRadius:=0;
  FSlices:=16;
  FLoops:=2;
  FStartAngle:=0;
  FSweepAngle:=360;
end;

// BuildList
//
procedure TGLDisk.BuildList(var rci : TRenderContextInfo);
var
   quadric : PGLUquadricObj;
begin
   quadric:=gluNewQuadric();
   SetupQuadricParams(Quadric);
   gluPartialDisk(Quadric, FInnerRadius, FOuterRadius, FSlices, FLoops, FStartAngle, FSweepAngle);
   gluDeleteQuadric(Quadric);
end;

// SetOuterRadius
//
procedure TGLDisk.SetOuterRadius(const aValue : Single);
begin
   if aValue<>FOuterRadius then begin
      FOuterRadius:=aValue;
      StructureChanged;
   end;
end;

// SetInnerRadius
//
procedure TGLDisk.SetInnerRadius(const aValue : Single);
begin
   if aValue<>FInnerRadius then begin
      FInnerRadius:=aValue;
      StructureChanged;
   end;
end;

// SetSlices
//
procedure TGLDisk.SetSlices(aValue : Integer);
begin
   if aValue<>FSlices then begin
      FSlices:=aValue;
      StructureChanged;
   end;
end;

// SetLoops
//
procedure TGLDisk.SetLoops(aValue : Integer);
begin
   if aValue<>FLoops then begin
      FLoops:=aValue;
      StructureChanged;
   end;
end;

// SetStartAngle
//
procedure TGLDisk.SetStartAngle(const aValue : Single);
begin
   if aValue<>FStartAngle then begin
      FStartAngle:=aValue;
      StructureChanged;
   end;
end;

// SetSweepAngle
//
procedure TGLDisk.SetSweepAngle(const aValue : Single);
begin
   if aValue<>FSweepAngle then begin
      FSweepAngle:=aValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLDisk.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TGLDisk) then begin
      FOuterRadius:=TGLDisk(Source).FOuterRadius;
      FInnerRadius:=TGLDisk(Source).FInnerRadius;
      FSlices:=TGLDisk(Source).FSlices;
      FLoops:=TGLDisk(Source).FLoops;
      FStartAngle:=TGLDisk(Source).FStartAngle;
      FSweepAngle:=TGLDisk(Source).FSweepAngle;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLDisk.AxisAlignedDimensionsUnscaled : TVector;
var
   r : TGLFloat;
begin
   r:=Abs(FOuterRadius);
   Result:=VectorMake(r, r, 0);
end;

// RayCastIntersect
//
function TGLDisk.RayCastIntersect(const rayStart, rayVector : TVector;
                                intersectPoint : PVector = nil;
                                intersectNormal : PVector = nil) : Boolean;
var
   ip : TVector;
   d : Single;
begin
   // start and sweep angle aren't honoured yet!
   if RayCastPlaneIntersect(rayStart, rayVector, AbsolutePosition, AbsoluteDirection, @ip) then begin
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^, ip);
      d:=VectorNorm(AbsoluteToLocal(ip));
      if (d>=Sqr(InnerRadius)) and (d<=Sqr(OuterRadius)) then begin
         if Assigned(intersectNormal) then
            SetVector(intersectNormal^, AbsoluteUp);
         Result:=True;
      end else Result:=False;
   end else Result:=False;
end;

// ------------------
// ------------------ TGLCylinderBase ------------------
// ------------------

// Create
//
constructor TGLCylinderBase.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);
   FBottomRadius:=0.5;
   FHeight:=1;
   FSlices:=16;
   FStacks:=4;
   FLoops:=1;
end;

// SetBottomRadius
//
procedure TGLCylinderBase.SetBottomRadius(const aValue : Single);
begin
	if aValue<>FBottomRadius then begin
		FBottomRadius:=aValue;
		StructureChanged;
	end;
end;

// GetTopRadius
//
function TGLCylinderBase.GetTopRadius : single;
begin
  result := FBottomRadius;
end;

// SetHeight
//
procedure TGLCylinderBase.SetHeight(const aValue : Single);
begin
	if aValue<>FHeight then begin
		FHeight:=aValue;
		StructureChanged;
	end;
end;

// SetSlices
//
procedure TGLCylinderBase.SetSlices(aValue : TGLInt);
begin
	if aValue<>FSlices then begin
		FSlices:=aValue;
		StructureChanged;
  	end;
end;

// SetStack
//
procedure TGLCylinderBase.SetStacks(aValue : TGLInt);
begin
	if aValue<>FStacks then begin
		FStacks:=aValue;
		StructureChanged;
	end;
end;

// SetLoops
//
procedure TGLCylinderBase.SetLoops(aValue : TGLInt);
begin
	if (aValue>=1) and (aValue<>FLoops) then begin
		FLoops:=aValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TGLCylinderBase.Assign(Source : TPersistent);
begin
	if assigned(Source) and (Source is TGLCylinderBase) then begin
		FBottomRadius:=TGLCylinderBase(Source).FBottomRadius;
		FSlices:=TGLCylinderBase(Source).FSlices;
		FStacks:=TGLCylinderBase(Source).FStacks;
		FLoops :=TGLCylinderBase(Source).FLoops;
		FHeight:=TGLCylinderBase(Source).FHeight;
	end;
	inherited Assign(Source);
end;

// GenerateSilhouette
//
function TGLCylinderBase.GenerateSilhouette(
  const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  connectivity : TConnectivity;
  sil : TGLSilhouette;
  ShadowSlices : integer;

  i : integer;
  p : array[0..3] of TVector3f;
  PiDivSlices : single;
  a1, a2 : single;
  c1, c2 : TVector3f;
  cosa1, cosa2, sina1, sina2 : single;
  HalfHeight : single;
  ShadowTopRadius : single;
begin
  Connectivity := TConnectivity.Create(true);

  ShadowSlices:= FSlices div 1;

  if FSlices<5 then
    FSlices := 5;

  PiDivSlices := 2*Pi/ShadowSlices;

  a1 := 0;

  // Is this a speed improvement or just a waste of code?
  HalfHeight := FHeight/2;

  MakeVector(c1, 0, -HalfHeight, 0);
  MakeVector(c2, 0,  HalfHeight, 0);

  ShadowTopRadius  := GetTopRadius;

  for i:=0 to ShadowSlices-1 do
  begin
    a2 := a1 + PiDivSlices;

    // Is this a speed improvement or just a waste of code?
    cosa1 := cos(a1); cosa2 := cos(a2);
    sina1 := sin(a1); sina2 := sin(a2);

    // Generate the four "corners";
    // Bottom corners
    MakeVector(p[0], FBottomRadius*sina2,-HalfHeight, FBottomRadius*cosa2);
    MakeVector(p[1], FBottomRadius*sina1,-HalfHeight, FBottomRadius*cosa1);

    // Top corners
    MakeVector(p[2], ShadowTopRadius*sina1, HalfHeight, ShadowTopRadius*cosa1);
    MakeVector(p[3], ShadowTopRadius*sina2, HalfHeight, ShadowTopRadius*cosa2);//}

    // This should be optimized to use AddIndexedFace, because this method
    // searches for each of the vertices and adds them or re-uses them.

    // Skin
    connectivity.AddFace(p[2], p[1], p[0]);
    connectivity.AddFace(p[3], p[2], p[0]);

    // Sides / caps
    connectivity.AddFace(c1, p[0], p[1]);
    connectivity.AddFace(p[2], p[3], c2);

    a1 := a1 + PiDivSlices;
  end;

  sil := nil;
  Connectivity.CreateSilhouette(
    silhouetteParameters, sil, false);

  result := sil;

  Connectivity.Free;
end;

// ------------------
// ------------------ TGLCone ------------------
// ------------------

// Create
//
constructor TGLCone.Create(AOwner:TComponent);
begin
	inherited Create(AOwner);
	FParts:=[coSides, coBottom];
end;

// BuildList
//
procedure TGLCone.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric();
	SetupQuadricParams(Quadric);
	glRotated(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if coSides in FParts then
		gluCylinder(quadric, BottomRadius, 0, Height, Slices, Stacks);
	if coBottom in FParts then begin
		// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, BottomRadius, Slices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetParts
//
procedure TGLCone.SetParts(aValue : TConeParts);
begin
	if aValue<>FParts then begin
		FParts:=aValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TGLCone.Assign(Source: TPersistent);
begin
	if Assigned(Source) and (Source is TGLCone) then begin
		FParts:=TGLCone(Source).FParts;
	end;
  	inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLCone.AxisAlignedDimensionsUnscaled : TVector;
var
   r : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   Result:=VectorMake(r{*Scale.DirectX}, 0.5*FHeight{*Scale.DirectY}, r{*Scale.DirectZ});
end;

// GetTopRadius
//
function TGLCone.GetTopRadius: single;
begin
  result := 0;
end;

// ------------------
// ------------------ TGLCylinder ------------------
// ------------------

// Create
//
constructor TGLCylinder.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTopRadius:=0.5;
   FParts:=[cySides, cyBottom, cyTop];
   FAlignment:=caCenter;
end;

// BuildList
//
procedure TGLCylinder.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
   case Alignment of
      caTop : glTranslatef(0, 0, -FHeight);
      caBottom : ;
   else // caCenter
   	glTranslatef(0, 0, -FHeight*0.5);
   end;
	if cySides in FParts then
		gluCylinder(Quadric, FBottomRadius, FTopRadius, FHeight, FSlices, FStacks);
	if cyTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
	if cyBottom in FParts then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetTopRadius
//
procedure TGLCylinder.SetTopRadius(const aValue : Single);
begin
   if aValue<>FTopRadius then begin
      FTopRadius:=aValue;
      StructureChanged;
   end;
end;

// GetTopRadius
//
function TGLCylinder.GetTopRadius: single;
begin
  result := FTopRadius;
end;

// SetParts
//
procedure TGLCylinder.SetParts(aValue: TCylinderParts);
begin
   if aValue<>FParts then begin
      FParts:=aValue;
      StructureChanged;
   end;
end;

// SetAlignment
//
procedure TGLCylinder.SetAlignment(val : TCylinderAlignment);
begin
   if val<>FAlignment then begin
      FAlignment:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLCylinder.Assign(Source: TPersistent);
begin
   if Assigned(SOurce) and (Source is TGLCylinder) then begin
      FParts:=TGLCylinder(Source).FParts;
      FTopRadius:=TGLCylinder(Source).FTopRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLCylinder.AxisAlignedDimensionsUnscaled: TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FBottomRadius);
  r1:=Abs(FTopRadius);
  if r1>r then r:=r1;
  Result:=VectorMake(r, 0.5*FHeight, r);
//  ScaleVector(Result, Scale.AsVector);
end;

// RayCastIntersect
//
function TGLCylinder.RayCastIntersect(const rayStart, rayVector : TVector;
                                    intersectPoint : PVector = nil;
                                    intersectNormal : PVector = nil) : Boolean;
const
   cOne : Single = 1;
var
   locRayStart, locRayVector, ip : TVector;
   poly : array [0..2] of Double;
   roots : TDoubleArray;
   minRoot : Double;
   t, tr2, invRayVector1, hTop, hBottom : Single;
   tPlaneMin, tPlaneMax : Single;
begin
   Result:=False;
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);

   case Alignment of
      caTop : begin
         hTop:=0;
         hBottom:=-Height;
      end;
      caBottom : begin
         hTop:=Height;
         hBottom:=0;
      end;
   else
      // caCenter
      hTop:=Height*0.5;
      hBottom:=-hTop;
   end;

   if locRayVector[1]=0 then begin
      // intersect if ray shot through the top/bottom planes
      if (locRayStart[0]>hTop) or (locRayStart[0]<hBottom) then
         Exit;
      tPlaneMin:=-1e99;
      tPlaneMax:=1e99;
   end else begin
      invRayVector1:=cOne/locRayVector[1];
      tr2:=Sqr(TopRadius);

      // compute intersection with topPlane
      t:=(hTop-locRayStart[1])*invRayVector1;
      if (t>0) and (cyTop in Parts) then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         if Sqr(ip[0])+Sqr(ip[2])<=tr2 then begin
            // intersect with top plane
            if Assigned(intersectPoint) then
               intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], hTop, ip[2], 1));
            if Assigned(intersectNormal) then
               intersectNormal^:=LocalToAbsolute(YHmgVector);
            Result:=True;
         end;
      end;
      tPlaneMin:=t;
      tPlaneMax:=t;
      // compute intersection with bottomPlane
      t:=(hBottom-locRayStart[1])*invRayVector1;
      if (t>0) and (cyBottom in Parts) then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         if (t<tPlaneMin) or (not (cyTop in Parts)) then begin
            if Sqr(ip[0])+Sqr(ip[2])<=tr2 then begin
               // intersect with top plane
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], hBottom, ip[2], 1));
               if Assigned(intersectNormal) then
                  intersectNormal^:=LocalToAbsolute(VectorNegate(YHmgVector));
               Result:=True;
            end;
         end;
      end;
      if t<tPlaneMin then
         tPlaneMin:=t;
      if t>tPlaneMax then
         tPlaneMax:=t;
   end;
   if cySides in Parts then begin
      // intersect against cylinder infinite cylinder
      poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopRadius);
      poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
      poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
      roots:=SolveQuadric(@poly);
      if MinPositiveCoef(roots, minRoot) then begin
         t:=minRoot;
         if (t>=tPlaneMin) and (t<tPlaneMax) then begin
            if Assigned(intersectPoint) or Assigned(intersectNormal) then begin
               ip:=VectorCombine(locRayStart, locRayVector, 1, t);
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(ip);
               if Assigned(intersectNormal) then begin
                  ip[1]:=0;
                  ip[3]:=0;
                  intersectNormal^:=LocalToAbsolute(ip);
               end;
            end;
            Result:=True;
         end;
      end;
   end else SetLength(roots, 0);
end;

// Align
//
procedure TGLCylinder.Align(const startPoint, endPoint : TVector);
var
   dir : TAffineVector;
begin
   AbsolutePosition:=startPoint;
   VectorSubtract(endPoint, startPoint, dir);
   if Parent<>nil then
      dir:=Parent.AbsoluteToLocal(dir);
   Up.AsAffineVector:=dir;
   Height:=VectorLength(dir);
   Lift(Height*0.5);
   Alignment:=caCenter;
end;

// Align
//
procedure TGLCylinder.Align(const startObj, endObj : TGLBaseSceneObject);
begin
   Align(startObj.AbsolutePosition, endObj.AbsolutePosition);
end;

// Align
//
procedure TGLCylinder.Align(const startPoint, endPoint : TAffineVector);
begin
   Align(PointMake(startPoint), PointMake(endPoint));
end;

// ------------------
// ------------------ TGLAnnulus ------------------
// ------------------

// Create
//
constructor TGLAnnulus.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fBottomInnerRadius:=0.3;
   fTopInnerRadius:=0.3;
   fTopRadius:=0.5;
   fParts:=[anInnerSides, anOuterSides, anBottom, anTop];
end;

// SetBottomInnerRadius
//
procedure TGLAnnulus.SetBottomInnerRadius(const aValue : Single);
begin
	if aValue<>FBottomInnerRadius then begin
		FBottomInnerRadius:=aValue;
		StructureChanged;
	end;
end;

// SetTopRadius
//
procedure TGLAnnulus.SetTopRadius(const aValue : Single);
begin
	if aValue<>FTopRadius then begin
		FTopRadius:=aValue;
		StructureChanged;
	end;
end;

// SetTopInnerRadius
//
procedure TGLAnnulus.SetTopInnerRadius(const aValue : Single);
begin
	if aValue<>FTopInnerRadius then begin
		FTopInnerRadius:=aValue;
		StructureChanged;
	end;
end;

// SetParts
//
procedure TGLAnnulus.SetParts(aValue: TAnnulusParts);
begin
   if aValue<>FParts then begin
      FParts:=aValue;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLAnnulus.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if anOuterSides in FParts then
		gluCylinder(Quadric, fBottomRadius, fTopRadius, fHeight, fSlices, fStacks);
	if anTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric,fTopInnerRadius, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
   if [anBottom, anInnerSides]*FParts<>[] then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
   	if anBottom in FParts then
	   	gluDisk(quadric,fBottominnerRadius,FBottomRadius, FSlices, FLoops);
      if anInnerSides in fParts then
         gluCylinder(Quadric, fBottomInnerRadius, fTopInnerRadius, fHeight, fSlices, fStacks);
   end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// Assign
//
procedure TGLAnnulus.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TGLAnnulus) then begin
      FParts:=TGLAnnulus(Source).FParts;
      FTopRadius:=TGLAnnulus(Source).FTopRadius;
      FTopInnerRadius:=TGLAnnulus(Source).fTopInnerRadius;
      FBottomRadius:=TGLAnnulus(Source).fBottomRadius;
      FBottomInnerRadius:=TGLAnnulus(Source).fbottomInnerRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TGLAnnulus.AxisAlignedDimensionsUnscaled : TVector;
var
   r, r1 : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   r1:=Abs(FTopRadius);
   if r1>r then r:=r1;
   Result:=VectorMake(r, 0.5*FHeight, r);
end;

// RayCastIntersect
//
function TGLAnnulus.RayCastIntersect(const rayStart, rayVector : TVector;
                  intersectPoint, intersectNormal : PVector) : Boolean;
const
   cOne : Single = 1;
var
   locRayStart, locRayVector, ip : TVector;
   poly : array [0..2] of Double;
   t, tr2, invRayVector1 : Single;
   tPlaneMin, tPlaneMax : Single;
   tir2,d2		:single;
   Root 			:Double;
   Roots,tmpRoots	:TDoubleArray;
   FirstIntersected	:boolean;
   h1,h2,hTop,hBot	:single;
   Draw1,Draw2	:boolean;
begin
   Result:=False;
   FirstIntersected:=False;
   SetLength(tmpRoots, 0);
   locRayStart:=AbsoluteToLocal(rayStart);
   locRayVector:=AbsoluteToLocal(rayVector);

   hTop:=Height*0.5;
   hBot:=-hTop;
   if locRayVector[1]<0 then begin // Sort the planes according to the direction of view
      h1:=hTop;   // Height of the 1st plane
      h2:=hBot;   // Height of the 2nd plane
      Draw1:=(anTop in Parts);    // 1st "cap" Must be drawn?
      Draw2:=(anBottom in Parts);
   end else begin
      h1:=hBot;
      h2:=hTop;
      Draw1:=(anBottom in Parts);
      Draw2:=(anTop in Parts);
   end;//if

   if locRayVector[1]=0 then begin
      // intersect if ray shot through the top/bottom planes
      if (locRayStart[0]>hTop) or (locRayStart[0]<hBot) then
         Exit;
      tPlaneMin:=-1e99;
      tPlaneMax:=1e99;
   end else begin
      invRayVector1:=cOne/locRayVector[1];
      tr2:=Sqr(TopRadius);
      tir2:=Sqr(TopInnerRadius);
      FirstIntersected:=False;

      // compute intersection with first plane
      t:=(h1-locRayStart[1])*invRayVector1;
      if (t>0) and Draw1 then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         d2:=Sqr(ip[0])+Sqr(ip[2]);
         if (d2<=tr2)and(d2>=tir2) then begin
            // intersect with top plane
            FirstIntersected:=true;
            if Assigned(intersectPoint) then
               intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], h1, ip[2], 1));
            if Assigned(intersectNormal) then
               intersectNormal^:=LocalToAbsolute(YHmgVector);
            Result:=True;
         end;
      end;
      tPlaneMin:=t;
      tPlaneMax:=t;

      // compute intersection with second plane
      t:=(h2-locRayStart[1])*invRayVector1;
      if (t>0) and Draw2 then begin
         ip[0]:=locRayStart[0]+t*locRayVector[0];
         ip[2]:=locRayStart[2]+t*locRayVector[2];
         d2:=Sqr(ip[0])+Sqr(ip[2]);
         if (t<tPlaneMin) or (not FirstIntersected) then begin
            if (d2<=tr2)and(d2>=tir2) then begin
               // intersect with top plane
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(VectorMake(ip[0], h2, ip[2], 1));
               if Assigned(intersectNormal) then
                  intersectNormal^:=LocalToAbsolute(VectorNegate(YHmgVector));
               Result:=True;
            end;
         end;
      end;
      if t<tPlaneMin then begin
         tPlaneMin:=t;
      end;//if
      if t>tPlaneMax then
         tPlaneMax:=t;
   end;

   try
      SetLength(Roots,4);
      Roots[0]:=-1; Roots[1]:=-1; Roots[2]:=-1; Roots[3]:=-1; // By default, side is behind rayStart

      {Compute roots for outer cylinder}
      if anOuterSides in Parts then begin
         // intersect against infinite cylinder, will be cut by tPlaneMine and tPlaneMax
         poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopRadius);
         poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
         poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
         tmpRoots:=SolveQuadric(@poly);  // Intersect coordinates on rayVector (rayStart=0)
         if (High(tmproots)>=0)and  // Does root exist?
            ((tmpRoots[0]>tPlaneMin) and not FirstIntersected) and // In the annulus and not masked by first cap
            ((tmpRoots[0]<tPlaneMax) )  // In the annulus
         then Roots[0]:=tmpRoots[0];
         if (High(tmproots)>=1)and
            ((tmpRoots[1]>tPlaneMin) and not FirstIntersected) and
            ((tmpRoots[1]<tPlaneMax) )
         then Roots[1]:=tmpRoots[1];
      end;//if

      {Compute roots for inner cylinder}
      if anInnerSides in Parts then begin
         // intersect against infinite cylinder
         poly[0]:=Sqr(locRayStart[0])+Sqr(locRayStart[2])-Sqr(TopInnerRadius);
         poly[1]:=2*(locRayStart[0]*locRayVector[0]+locRayStart[2]*locRayVector[2]);
         poly[2]:=Sqr(locRayVector[0])+Sqr(locRayVector[2]);
         tmproots:=SolveQuadric(@poly);
         if (High(tmproots)>=0)and
            ((tmpRoots[0]>tPlaneMin) and not FirstIntersected) and
            ((tmpRoots[0]<tPlaneMax) )
         then Roots[2]:=tmpRoots[0];
         if (High(tmproots)>=1)and
            ((tmpRoots[1]>tPlaneMin) and not FirstIntersected) and
            ((tmpRoots[1]<tPlaneMax) )
         then Roots[3]:=tmpRoots[1];
      end;//if

      {Find the first intersection point and compute its coordinates and normal}
      if MinPositiveCoef(Roots, Root) then begin
         t:=Root;
         if (t>=tPlaneMin) and (t<tPlaneMax) then begin
            if Assigned(intersectPoint) or Assigned(intersectNormal) then begin
               ip:=VectorCombine(locRayStart, locRayVector, 1, t);
               if Assigned(intersectPoint) then
                  intersectPoint^:=LocalToAbsolute(ip);
               if Assigned(intersectNormal) then begin
                  ip[1]:=0;
                  ip[3]:=0;
                  intersectNormal^:=LocalToAbsolute(ip);
               end;
            end;
            Result:=True;
         end;
      end;

   finally
      Roots:=nil;
      tmpRoots:=nil;
   end;//finally
end;

// ------------------
// ------------------ TGLTorus ------------------
// ------------------

// Create
//
constructor TGLTorus.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FRings:=25;
   FSides:=15;
   FMinorRadius:=0.1;
   FMajorRadius:=0.4;
end;

// BuildList
//
procedure TGLTorus.BuildList(var rci : TRenderContextInfo);
var
   I, J : Integer;
   Theta, Phi, Theta1, cosPhi, sinPhi, dist : TGLFloat;
   cosTheta, sinTheta: TGLFloat;
   cosTheta1, sinTheta1: TGLFloat;
   ringDelta, sideDelta: TGLFloat;
   iFact, jFact : Single;
begin
   // handle texture generation
   ringDelta:=c2PI/FRings;
   sideDelta:=c2PI/FSides;
   theta:=0;
   cosTheta:=1;
   sinTheta:=0;
   iFact:=1/FRings;
   jFact:=1/FSides;
   for I:=FRings-1 downto 0 do  begin
      theta1:=theta+ringDelta;
      SinCos(theta1, sinTheta1, cosTheta1);
      glBegin(GL_QUAD_STRIP);
      phi:=0;
      for J:=FSides downto 0 do begin
         phi:=phi+sideDelta;
         SinCos(phi, sinPhi, cosPhi);
         dist:=FMajorRadius+FMinorRadius*cosPhi;

         xglTexCoord2f(i*iFact, j*jFact);
         glNormal3f(cosTheta1*cosPhi, -sinTheta1*cosPhi, sinPhi);
         glVertex3f(cosTheta1*dist, -sinTheta1*dist, FMinorRadius*sinPhi);

         xglTexCoord2f((i+1)*iFact, j*jFact);
         glNormal3f(cosTheta*cosPhi, -sinTheta*cosPhi, sinPhi);
         glVertex3f(cosTheta*dist, -sinTheta*dist, FMinorRadius*sinPhi);
      end;
      glEnd;
      theta:=theta1;
      cosTheta:=cosTheta1;
      sinTheta:=sinTheta1;
   end;
end;

// SetMajorRadius
//
procedure TGLTorus.SetMajorRadius(const aValue: Single);
begin
   if FMajorRadius<>aValue then begin
      FMajorRadius:=aValue;
      StructureChanged;
   end;
end;

// SetMinorRadius
//
procedure TGLTorus.SetMinorRadius(const aValue : Single);
begin
   if FMinorRadius<>aValue then begin
      FMinorRadius:=aValue;
      StructureChanged;
   end;
end;

// SetRings
//
procedure TGLTorus.SetRings(aValue: Cardinal);
begin
   if FRings<>aValue then begin
      FRings:=aValue;
      if FRings<2 then FRings:=2;
      StructureChanged;
   end;
end;

// SetSides
//
procedure TGLTorus.SetSides(aValue : Cardinal);
begin
   if FSides<>aValue then begin
      FSides:=aValue;
      if FSides<3 then FSides:=3;
      StructureChanged;
   end;
end;

// AxisAlignedDimensionsUnscaled
//
function TGLTorus.AxisAlignedDimensionsUnscaled : TVector;
var
   r, r1 : TGLFloat;
begin
   r:=Abs(FMajorRadius);
   r1:=Abs(FMinorRadius);
   Result:=VectorMake(r+r1, r+r1, r1); //Danb
end;

// RayCastIntersect
//
function TGLTorus.RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean;
var
   i : Integer;
   fRo2, fRi2, fDE, fVal, r, nearest : Double;
   polynom : array [0..4] of Double;
   polyRoots : TDoubleArray;
   localStart, localVector : TVector;
   vi, vc : TVector;
begin
   // compute coefficients of quartic polynomial
   fRo2:=Sqr(MajorRadius);
   fRi2:=Sqr(MinorRadius);
   localStart :=AbsoluteToLocal(rayStart);
   localVector:=AbsoluteToLocal(rayVector);
   NormalizeVector(localVector);
   fDE :=VectorDotProduct(localStart, localVector);
   fVal:=VectorNorm(localStart)-(fRo2+fRi2);

   polynom[0] := Sqr(fVal) - 4.0*fRo2*(fRi2 - Sqr(localStart[2]));
   polynom[1] := 4.0*fDE*fVal + 8.0*fRo2*localVector[2]*localStart[2];
   polynom[2] := 2.0*fVal + 4.0*Sqr(fDE) + 4.0*fRo2*Sqr(localVector[2]);
   polynom[3] := 4.0*fDE;
   polynom[4] := 1;

   // solve the quartic
   polyRoots:=SolveQuartic(@polynom[0]);

   // search for closest point
   Result:=(Length(polyRoots)>0);
   if Result then begin
      nearest:=1e20;
      for i:=0 to High(polyRoots) do begin
         r:=polyRoots[i];
         if (r>0) and (r<nearest) then begin
            nearest:=r;
            Result:=True;
         end;
      end;
      vi:=VectorCombine(localStart, localVector, 1, nearest);
      if Assigned(intersectPoint) then
         SetVector(intersectPoint^, LocalToAbsolute(vi));
      if Assigned(intersectNormal) then begin
         // project vi on local torus plane
         vc[0]:=vi[0];
         vc[1]:=vi[1];
         vc[2]:=0;
         // project vc on MajorRadius circle
         ScaleVector(vc, MajorRadius/(VectorLength(vc)+0.000001));
         // calculate circle to intersect vector (gives normal);
         SubtractVector(vi, vc);
         // return to absolute coordinates and normalize
         vi[3]:=0;
         SetVector(intersectNormal^, LocalToAbsolute(vi));
      end;
   end;
end;

// ------------------
// ------------------ TGLArrowLine ------------------
// ------------------

// Create
//
constructor TGLArrowLine.Create(AOwner:TComponent);
begin
   inherited;
   fTopRadius:=0.1;
   BottomRadius:=0.1;
   fTopArrowHeadRadius:=0.2;
   fTopArrowHeadHeight:=0.5;
   fBottomArrowHeadRadius:=0.2;
   fBottomArrowHeadHeight:=0.5;
   FHeadStackingStyle:=ahssStacked;
   { by default there is not much point having the top of the line (cylinder)
     showing as it is coincidental with the Toparrowhead bottom.
     Note I've defaulted to "vector" type arrows (arrow head on top only}
   fParts:=[alLine, alTopArrow];
end;

// SetTopRadius
//
procedure TGLArrowLine.SetTopRadius(const aValue : Single);
begin
   if aValue<>fTopRadius then begin
      fTopRadius:=aValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadHeight
//
procedure TGLArrowLine.SetTopArrowHeadHeight(const aValue : Single);
begin
   if aValue<>fTopArrowHeadHeight then begin
      fTopArrowHeadHeight:=aValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadRadius
//
procedure TGLArrowLine.SetTopArrowHeadRadius(const aValue : Single);
begin
   if aValue<>fTopArrowHeadRadius then begin
      fTopArrowHeadRadius:=aValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadHeight
//
procedure TGLArrowLine.SetBottomArrowHeadHeight(const aValue : Single);
begin
   if aValue<>fBottomArrowHeadHeight then begin
      fBottomArrowHeadHeight:=aValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadRadius
//
procedure TGLArrowLine.SetBottomArrowHeadRadius(const aValue : Single);
begin
   if aValue<>fBottomArrowHeadRadius then begin
      fBottomArrowHeadRadius:=aValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TGLArrowLine.SetParts(aValue: TArrowLineParts);
begin
   if aValue<>FParts then begin
      FParts:=aValue;
      StructureChanged;
   end;
end;

// SetHeadStackingStyle
//
procedure TGLArrowLine.SetHeadStackingStyle(const val : TArrowHeadStackingStyle);
begin
   if val<>FHeadStackingStyle then begin
      FHeadStackingStyle:=val;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TGLArrowLine.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
   cylHeight, cylOffset, headInfluence : Single;
begin
   case HeadStackingStyle of
      ahssCentered : headInfluence:=0.5;
      ahssIncluded : headInfluence:=1;
   else // ahssStacked
      headInfluence:=0;
   end;
   cylHeight:=Height;
   cylOffset:=-FHeight*0.5;
   // create a new quadric
 	quadric:=gluNewQuadric;
   SetupQuadricParams(Quadric);
   // does the top arrow part - the cone
   if alTopArrow in Parts then begin
      cylHeight:=cylHeight-TopArrowHeadHeight*headInfluence;
      glPushMatrix;
      glTranslatef(0, 0, Height*0.5-TopArrowHeadHeight*headInfluence);
      gluCylinder(quadric, fTopArrowHeadRadius, 0, fTopArrowHeadHeight, Slices, Stacks);
     	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fTopRadius, fTopArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fTopArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the bottom arrow part - another cone
   if alBottomArrow in Parts then begin
      cylHeight:=cylHeight-BottomArrowHeadHeight*headInfluence;
      cylOffset:=cylOffset+BottomArrowHeadHeight*headInfluence;
      glPushMatrix;
      // make the bottom arrow point in the other direction
	   glRotatef(180, 1, 0, 0);
      glTranslatef(0, 0, Height*0.5-BottomArrowHeadHeight*headInfluence);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(quadric, fBottomArrowHeadRadius, 0, fBottomArrowHeadHeight, Slices, Stacks);
   	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fBottomRadius, fBottomArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fBottomArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the cylinder that makes the line
   if (cylHeight>0) and (alLine in Parts) then begin
      glPushMatrix;
      glTranslatef(0, 0, cylOffset);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(Quadric, FBottomRadius, FTopRadius, cylHeight, FSlices, FStacks);
      if not (alTopArrow in Parts) then begin
         glPushMatrix;
         glTranslatef(0, 0, cylHeight);
         gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
         glPopMatrix;
      end;
      if not (alBottomArrow in Parts) then begin
         // swap quadric orientation because top of a disk is defined as outside
         SetInvertedQuadricOrientation(quadric);
         gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
      end;
      glPopMatrix;
   end;
   gluDeleteQuadric(Quadric);
end;

// Assign
//
procedure TGLArrowLine.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TGLArrowLine) then begin
      FParts:=TGLArrowLine(Source).FParts;
      FTopRadius:=TGLArrowLine(Source).FTopRadius;
      fTopArrowHeadHeight:=TGLArrowLine(Source).fTopArrowHeadHeight;
      fTopArrowHeadRadius:=TGLArrowLine(Source).fTopArrowHeadRadius;
      fBottomArrowHeadHeight:=TGLArrowLine(Source).fBottomArrowHeadHeight;
      fBottomArrowHeadRadius:=TGLArrowLine(Source).fBottomArrowHeadRadius;
      FHeadStackingStyle:=TGLArrowLine(Source).FHeadStackingStyle;
   end;
   inherited Assign(Source);
end;

// ------------------
// ------------------ TGLFrustrum ------------------
// ------------------

constructor TGLFrustrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApexHeight := 1;
  FBaseWidth := 1;
  FBaseDepth := 1;
  FHeight := 0.5;
  FParts := cAllFrustrumParts;
  FNormalDirection := ndOutside;
end;

procedure TGLFrustrum.BuildList(var rci: TRenderContextInfo);
var
  HBW, HBD: TGLFloat; // half of width, half of depth at base
  HTW, HTD: TGLFloat; // half of width, half of depth at top of frustrum
  Sign: TGLFloat;     // +1 or -1
  Angle: TGLFloat;    // in radians
  ASin, ACos: TGLFloat;
begin
  if FNormalDirection = ndInside then
    Sign := -1
  else
    Sign := 1;
  HBW := FBaseWidth * 0.5;
  HBD := FBaseDepth * 0.5;
  HTW := HBW * (FApexHeight - FHeight) / FApexHeight;
  HTD := HBD * (FApexHeight - FHeight) / FApexHeight;

  glBegin(GL_QUADS);

  if [fpFront, fpBack] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBD); // angle of front plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpFront in FParts then
    begin
      glNormal3f(0, Sign * ACos, Sign * ASin);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, HBD);
    end;
    if fpBack in FParts then
    begin
      glNormal3f(0, Sign * ACos, -Sign * ASin);
      xglTexCoord2fv(@YTexPoint);     glVertex3f( HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f( HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight, -HTD);
    end;
  end;

  if [fpLeft, fpRight] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBW); // angle of side plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpLeft in FParts then
    begin
      glNormal3f(-Sign * ASin, Sign * ACos, 0);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight,  HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0,  HBD);
    end;
    if fpRight in FParts then
    begin
      glNormal3f(Sign * ASin, Sign * ACos, 0);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(HBW, 0,  HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(HTW, FHeight, -HTD);
    end;
  end;

  if (fpTop in FParts) and (FHeight < FApexHeight) then
  begin
    glNormal3f(0, Sign, 0);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HTW, FHeight,  HTD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HTW, FHeight,  HTD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, -HTD);
  end;
  if fpBottom in FParts then
  begin
    glNormal3f(0, -Sign, 0);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, -HBD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HBW, 0,  HBD);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HBW, 0,  HBD);
  end;

  glEnd;
end;

procedure TGLFrustrum.SetApexHeight(const aValue : Single);
begin
  if (aValue <> FApexHeight) and (aValue >= 0) then
  begin
    FApexHeight := aValue;
    if FHeight > aValue then
      FHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseDepth(const aValue : Single);
begin
  if (aValue <> FBaseDepth) and (aValue >= 0) then
  begin
    FBaseDepth := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetBaseWidth(const aValue : Single);
begin
  if (aValue <> FBaseWidth) and (aValue >= 0) then
  begin
    FBaseWidth := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetHeight(const aValue : Single);
begin
  if (aValue <> FHeight) and (aValue >= 0) then
  begin
    FHeight := aValue;
    if FApexHeight < aValue then
      FApexHeight := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetParts(aValue: TFrustrumParts);
begin
  if aValue <> FParts then
  begin
    FParts := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.SetNormalDirection(aValue: TNormalDirection);
begin
  if aValue <> FNormalDirection then
  begin
    FNormalDirection := aValue;
    StructureChanged;
  end;
end;

procedure TGLFrustrum.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLFrustrum) then
  begin
    FApexHeight := TGLFrustrum(Source).FApexHeight;
    FBaseDepth := TGLFrustrum(Source).FBaseDepth;
    FBaseWidth := TGLFrustrum(Source).FBaseWidth;
    FHeight := TGLFrustrum(Source).FHeight;
    FParts := TGLFrustrum(Source).FParts;
    FNormalDirection := TGLFrustrum(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TGLFrustrum.TopDepth: TGLFloat;
begin
  Result := FBaseDepth * (FApexHeight - FHeight) / FApexHeight;
end;

function TGLFrustrum.TopWidth: TGLFloat;
begin
  Result := FBaseWidth * (FApexHeight - FHeight) / FApexHeight;
end;

procedure TGLFrustrum.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('FrustrumSize', ReadData, WriteData,
    (FApexHeight <> 1) or (FBaseDepth <> 1) or (FBaseWidth <> 1) or
    (FHeight <> 0.5));
end;

procedure TGLFrustrum.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FApexHeight, SizeOf(FApexHeight));
    Read(FBaseDepth, SizeOf(FBaseDepth));
    Read(FBaseWidth, SizeOf(FBaseWidth));
    Read(FHeight, SizeOf(FHeight));
  end;
end;

procedure TGLFrustrum.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FApexHeight, SizeOf(FApexHeight));
    Write(FBaseDepth, SizeOf(FBaseDepth));
    Write(FBaseWidth, SizeOf(FBaseWidth));
    Write(FHeight, SizeOf(FHeight));
  end;
end;


function TGLFrustrum.AxisAlignedBoundingBoxUnscaled : TAABB ;
var
   aabb              : TAABB ;
   child             : TGLBaseSceneObject ;
   i                 : Integer ;
begin
   SetAABB(Result, AxisAlignedDimensionsUnscaled) ;
   OffsetAABB(Result, VectorMake(0, FHeight * 0.5, 0)) ;

   // not tested for child objects
   for i := 0 to Count - 1 do begin
     child := TGLBaseSceneObject(Children[i]) ;
     aabb := child.AxisAlignedBoundingBoxUnscaled ;
     AABBTransform(aabb, child.Matrix) ;
     AddAABB(Result, aabb) ;
   end ;
end ;

function TGLFrustrum.AxisAlignedBoundingBox : TAABB ;
begin
   Result := AxisAlignedBoundingBoxUnscaled ;
   AABBScale(Result, Scale.AsAffineVector) ;
end ;

function TGLFrustrum.AxisAlignedDimensionsUnscaled : TVector ;
begin
   Result[0] := FBaseWidth * 0.5 ;
   Result[1] := FHeight * 0.5 ;
   Result[2] := FBaseDepth * 0.5 ;
   Result[3] := 0 ;
end ;


// ------------------
// ------------------ TGLPolygon ------------------
// ------------------

// Create
//
constructor TGLPolygon.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FParts:=[ppTop, ppBottom];
end;

// Destroy
//
destructor TGLPolygon.Destroy;
begin
   inherited Destroy;
end;

// SetParts
//
procedure TGLPolygon.SetParts(const val : TPolygonParts);
begin
   if FParts<>val then begin
      FParts:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TGLPolygon.Assign(Source: TPersistent);
begin
   if Source is TGLPolygon then begin
      FParts:=TGLPolygon(Source).FParts;
   end;
   inherited Assign(Source);
end;

// BuildList
//
procedure TGLPolygon.BuildList(var rci : TRenderContextInfo);
var
   normal : TAffineVector;
   pNorm : PAffineVector;
begin
   if (Nodes.Count>1) then begin
      normal:=Nodes.Normal;
      if VectorIsNull(normal) then
         pNorm:=nil
      else pNorm:=@normal;
      if ppTop in FParts then begin
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, pNorm, 1)
         else Nodes.RenderTesselatedPolygon(True, pNorm, Division);
      end;
      // tessellate bottom polygon
      if ppBottom in FParts then begin
         if Assigned(pNorm) then
            NegateVector(normal);
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, pNorm, 1, True)
         else Nodes.RenderTesselatedPolygon(True, pNorm, Division, True);
      end;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------


initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLCylinder, TGLCone, TGLTorus, TGLDisk, TGLArrowLine,
                    TGLAnnulus, TGLFrustrum, TGLPolygon]);

end.

