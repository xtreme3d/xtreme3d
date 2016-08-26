//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTerrainRenderer<p>

   GLScene's brute-force terrain renderer.<p>

   <b>History : </b><font size=-1><ul>
      <li>01/09/04 - SG - Fix for RayCastIntersect (Alan Rose)
      <li>25/04/04 - EG - Occlusion testing support
      <li>13/01/04 - EG - Leak fix (Phil Scadden)
      <li>05/11/03 - SG - Fixed minuscule bug in RayCastIntersect (thanks Michael)
      <li>06/02/03 - EG - Fixed speculative range computation, better hashkey
      <li>14/01/03 - EG - RayCastIntersect normals fix (Stuart Gooding)
      <li>24/09/02 - EG - Added RayCastIntersect (Stuart Gooding)
      <li>28/08/02 - EG - Now longer wrongly requests hdtByte (Phil Scadden),
                          Terrain bounds limiting event (Erazem Polutnik)
      <li>10/07/02 - EG - Added support for "holes" in the elevation data
      <li>16/06/02 - EG - Added support for multi-material terrains
      <li>24/02/02 - EG - Hybrid ROAM-stripifier engine
      <li>18/12/01 - EG - Vertex-cache aware stripifier (+10% on GeForce)
      <li>12/08/01 - EG - Completely rewritten handles management
      <li>21/07/01 - EG - Added Notication registration in SetHeightDataSource
      <li>04/03/01 - EG - Completed for first release
	    <li>12/02/01 - EG - Creation
	</ul></font><p>

   NOTA : multi-materials terrain support is not yet optimized to minimize
          texture switches (in case of resued tile textures).
}
unit GLTerrainRenderer;

interface

uses Classes, GLScene, GLHeightData, GLTexture, VectorGeometry, GLContext,
   GLROAMPatch, VectorLists;

const
   cTilesHashSize = 511;

type

   TGetTerrainBoundsEvent = procedure(var l, t, r, b : Single) of object;
   TPatchPostRenderEvent = procedure (var rci : TRenderContextInfo; const patches : TList) of object;
   THeightDataPostRenderEvent = procedure (var rci : TRenderContextInfo; const heightDatas : TList) of object;
   TTerrainHighResStyle = (hrsFullGeometry, hrsTesselated);
   TTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);

	// TGLTerrainRenderer
	//
   {: Basic terrain renderer.<p>
      This renderer uses no sophisticated meshing, it just builds and maintains
      a set of terrain tiles, performs basic visibility culling and renders its
      stuff. You can use it has a base class/sample for more specialized
      terrain renderers.<p>
      The Terrain heightdata is retrieved directly from a THeightDataSource, and
      expressed as z=f(x, y) data. }
	TGLTerrainRenderer = class (TGLSceneObject)
	   private
	      { Private Declarations }
         FHeightDataSource : THeightDataSource;
         FTileSize : Integer;
         FQualityDistance, FinvTileSize : Single;
         FLastTriangleCount : Integer;
         FTilesPerTexture : Single;
         FMaxCLODTriangles, FCLODPrecision : Integer;
         FBufferVertices : TAffineVectorList;
         FBufferTexPoints : TTexPointList;
         FBufferVertexIndices : TIntegerList;
         FMaterialLibrary : TGLMaterialLibrary;
         FOnGetTerrainBounds : TGetTerrainBoundsEvent;
         FOnPatchPostRender : TPatchPostRenderEvent;
         FOnHeightDataPostRender : THeightDataPostRenderEvent;
         FQualityStyle : TTerrainHighResStyle;
         FOcclusionFrameSkip : Integer;
         FOcclusionTesselate : TTerrainOcclusionTesselate;

	   protected
	      { Protected Declarations }
         FTilesHash : packed array [0..cTilesHashSize] of TList;

         procedure MarkAllTilesAsUnused;
         procedure ReleaseAllUnusedTiles;
         procedure MarkHashedTileAsUsed(const tilePos : TAffineVector);
         function HashedTile(const tilePos : TAffineVector; canAllocate : Boolean = True) : THeightData; overload;
         function HashedTile(const xLeft, yTop : Integer; canAllocate : Boolean = True) : THeightData; overload;

         procedure SetHeightDataSource(const val : THeightDataSource);
         procedure SetTileSize(const val : Integer);
         procedure SetTilesPerTexture(const val : Single);
         procedure SetCLODPrecision(const val : Integer);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetQualityStyle(const val : TTerrainHighResStyle);
         procedure SetOcclusionFrameSkip(val : Integer);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure DestroyHandle; override;

         procedure ReleaseAllTiles; dynamic;
         procedure OnTileDestroyed(sender : TObject); virtual;

         function GetPreparedPatch(const tilePos, eyePos : TAffineVector;
                                   texFactor : Single;
                                   hdList : TList) : TGLROAMPatch;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function RayCastIntersect(const rayStart, rayVector : TVector;
                                   intersectPoint : PVector = nil;
                                   intersectNormal : PVector = nil) : Boolean; override;

         {: Interpolates height for the given point.<p>
            Expects a point expressed in absolute coordinates. }
         function InterpolatedHeight(const p : TVector) : Single; overload; virtual; 
         function InterpolatedHeight(const p : TAffineVector) : Single; overload;
         {: Triangle count for the last render. }
         property LastTriangleCount : Integer read FLastTriangleCount;

	   published
	      { Published Declarations }
         {: Specifies the HeightData provider component. }
         property HeightDataSource : THeightDataSource read FHeightDataSource write SetHeightDataSource;
         {: Size of the terrain tiles.<p>
            Must be a power of two. }
         property TileSize : Integer read FTileSize write SetTileSize default 16;
         {: Number of tiles required for a full texture map. }
         property TilesPerTexture : Single read FTilesPerTexture write SetTilesPerTexture;
         {: Link to the material library holding terrain materials.<p>
            If unspecified, and for all terrain tiles with unspecified material,
            the terrain renderer's material is used. }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

         {: Quality distance hint.<p>
            This parameter gives an hint to the terrain renderer at which distance
            the terrain quality can be degraded to favor speed. The distance is
            expressed in absolute coordinates units.<p>
            All tiles closer than this distance are rendered according to
            QualityStyle and with a static resolution. }
         property QualityDistance : Single read FQualityDistance write FQualityDistance;
         {: Determines how high-res tiles (closer than QualityDistance) are rendered.<p>
            hrsFullGeometry (default value) means that the high-res tiles are rendered
            with full-geometry, and no LOD of any kind, while hrsTesselated means
            the tiles will be tesselated once, with the best output for the
            CLODPrecision, and the result of that tesselation will be reused
            in further frames without any adpative tesselation. }
         property QualityStyle : TTerrainHighResStyle read FQualityStyle write SetQualityStyle default hrsFullGeometry;
         {: Maximum number of CLOD triangles per scene.<p>
            Triangles in high-resolution tiles (closer than QualityDistance) do
            not count toward this limit. }
         property MaxCLODTriangles : Integer read FMaxCLODTriangles write FMaxCLODTriangles default 65536;
         {: Precision of CLOD tiles.<p>
            The lower the value, the higher the precision and triangle count.
            Large values will result in coarse terrain.<br>
            high-resolution tiles (closer than QualityDistance) ignore this setting. }
         property CLODPrecision : Integer read FCLODPrecision write SetCLODPrecision default 100;
         {: Numbers of frames to skip for a tile when occlusion testing found it invisible.<p>
            Occlusion testing can help reduce CPU, T&L and fillrate requirements
            when tiles are occluded, either by the terrain itself (tiles behind
            a mountain or a cliff) or by geometry that was rendered before the
            terrain (large buildings). If there is little occlusion in your scene
            (such as in top down or high-altitude view), turning occlusion on
            may have a slightly negative effect on framerate.<br>
            It works by turning off rendering of tiles for the specified number
            of frames if it has been found invisible, after FrameSkip number
            of frames have been skipped, it will be rendered again, and a new
            occlusion testing made. This makes occlusion-testing a frame-to-frame
            coherency optimization, and as such, shouldn't be used for static
            rendering (ie. leave value to its default of zero).<br>
            This optimization requires the hardware to support GL_NV_occlusion_query. }
         property OcclusionFrameSkip : Integer read FOcclusionFrameSkip write SetOcclusionFrameSkip default 0;
         {: Determines if and how occlusion testing affects tesselation.<p>
            Turning off tesselation of tiles determined invisible can improve
            performance, however, it may result in glitches since the tesselation
            of an ivisible tile can have a slight effect on the tesselation
            of its adjacent tiles (by forcing higher resolution at the border
            for instance). This negative effect can be lessened by increasing
            the QualityDistance, so that glitches will apear farther away
            (this will mean increasing your triangle count though, so you'll
            trade CPU power against T&L power). }
         property OcclusionTesselate : TTerrainOcclusionTesselate read FOcclusionTesselate write FOcclusionTesselate default totTesselateIfVisible;

         {: Allows to specify terrain bounds.<p>
            Default rendering bounds will reach depth of view in all direction,
            with this event you can chose to specify a smaller rendered
            terrain area. }
         property OnGetTerrainBounds : TGetTerrainBoundsEvent read FOnGetTerrainBounds write FOnGetTerrainBounds;
         {: Invoked for each rendered patch after terrain render has completed.<p>
            The list holds TGLROAMPatch objects and allows per-patch
            post-processings, like waters, trees... It is invoked *before*
            OnHeightDataPostRender. }
         property OnPatchPostRender : TPatchPostRenderEvent read FOnPatchPostRender write FOnPatchPostRender;
         {: Invoked for each heightData not culled out by the terrain renderer.<p>
            The list holds THeightData objects and allows per-patch
            post-processings, like waters, trees... It is invoked *after*
            OnPatchPostRender. }
         property OnHeightDataPostRender : THeightDataPostRenderEvent read FOnHeightDataPostRender write FOnHeightDataPostRender;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLMisc, XOpenGL, GLUtils;

// HashKey
//
function HashKey(const xLeft, yTop : Integer) : Integer;
begin
   Result:=( xLeft+(xLeft shr 8)+(xLeft shr 16)
            +(yTop shl 1)+(yTop shr 9)+(yTop shr 17)) and cTilesHashSize;
end;


// ------------------
// ------------------ TGLTerrainRenderer ------------------
// ------------------

// Create
//
constructor TGLTerrainRenderer.Create(AOwner: TComponent);
var
   i : Integer;
begin
	inherited Create(AOwner);
   for i:=0 to cTilesHashSize do
      FTilesHash[i]:=TList.Create;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FTileSize:=16;
   FinvTileSize:=1/16;
   FTilesPerTexture:=1;
   FMaxCLODTriangles:=65536;
   FCLODPrecision:=100;
   FOcclusionTesselate:=totTesselateIfVisible;
   FBufferVertices:=TAffineVectorList.Create;
   FBufferTexPoints:=TTexPointList.Create;
   FBufferVertexIndices:=TIntegerList.Create;
end;

// Destroy
//
destructor TGLTerrainRenderer.Destroy;
var
   i : Integer;
begin
   FBufferVertices.Free;
   FBufferTexPoints.Free;
   FBufferVertexIndices.Free;
   ReleaseAllTiles;
   for i:=0 to cTilesHashSize do begin
      FTilesHash[i].Free;
      FTilesHash[i]:=nil;
   end;
	inherited Destroy;
end;

// Notification
//
procedure TGLTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FHeightDataSource then
         HeightDataSource:=nil
      else if AComponent=FMaterialLibrary then
         MaterialLibrary:=nil;
   end;
   inherited;
end;

// DestroyHandle
//
procedure TGLTerrainRenderer.DestroyHandle;
begin
   inherited;
   ReleaseAllTiles;
   if Assigned(HeightDataSource) then
      HeightDataSource.Clear;
end;

// RayCastIntersect
//
function TGLTerrainRenderer.RayCastIntersect(const rayStart, rayVector : TVector;
                                           intersectPoint : PVector = nil;
                                           intersectNormal : PVector = nil) : Boolean;
var
   p1, d, p2, p3 : TVector;
   step, i, h, minH, maxH, p1height : Single;
   startedAbove : Boolean;
   failSafe : Integer;
   AbsX, AbsY, AbsZ : TVector;
begin
   Result:=False;
   if Assigned(HeightDataSource) then begin
      step:=(Scale.X+Scale.Y); //Initial step size guess
      i:=step;
      d:=VectorNormalize(rayVector);
      AbsZ:=VectorNormalize(LocalToAbsolute(ZHMGVector));
      startedAbove:=((InterpolatedHeight(rayStart)-VectorDotProduct(rayStart, AbsZ))<0);
      maxH:=Scale.Z*256;
      minH:=-Scale.Z*256;
      failSafe:=0;
      while True do begin
         p1:=VectorCombine(rayStart, d, 1, i);
         h:=InterpolatedHeight(p1);
         p1height:=VectorDotProduct(AbsZ, p1);
         if Abs(h-p1height)<0.1 then begin //Need a tolerance variable here (how close is good enough?)
            Result:=True;
            Break;
         end else begin
            if startedAbove then begin
               if h<p1height then
                  i:=i+step;
               if (h-p1height)>0 then begin
                  step:=step*0.5;
                  i:=i-step;
               end;
            end else begin
               if h>p1height then
                  i:=i+step;
            end;
         end;
         Inc(failSafe);
         if failSafe>1024 then Break;
         if VectorDotProduct(AbsZ, d)<0 then begin
            if h<minH then Exit
         end else if h>maxH then Exit;
      end;

      if Result then begin
         p1:=VectorAdd(p1, VectorScale(AbsZ, InterpolatedHeight(p1)-VectorDotProduct(p1,AbsZ)));
         if Assigned(intersectPoint) then
            intersectPoint^:=p1;

         // Calc Normal
         if Assigned(intersectNormal) then begin
            // Get 2 nearby points for cross-product
            AbsX:=VectorNormalize(LocalToAbsolute(XHMGVector));
            AbsY:=VectorNormalize(LocalToAbsolute(YHMGVector));
            p2:=VectorAdd(p1, VectorScale(AbsX, 0.1));
            p2:=VectorAdd(p2, VectorScale(AbsZ, InterpolatedHeight(p2)-VectorDotProduct(p2,AbsZ)));
            p3:=VectorAdd(p1, VectorScale(AbsY, 0.1));
            p3:=VectorAdd(p3, VectorScale(AbsZ, InterpolatedHeight(p3)-VectorDotProduct(p3,AbsZ)));

            intersectNormal^:=VectorNormalize(VectorCrossProduct(VectorSubtract(p1, p2),
                                                                 VectorSubtract(p3, p1)));
         end;
      end;
   end;
end;

// ReleaseAllTiles
//
procedure TGLTerrainRenderer.ReleaseAllTiles;
var
   i, k : Integer;
   hd : THeightData;
begin
   for i:=0 to cTilesHashSize do with FTilesHash[i] do begin
      for k:=Count-1 downto 0 do begin
         hd:=THeightData(List[k]);
         OnTileDestroyed(hd);
         hd.OnDestroy:=nil;
         hd.Release;
      end;
      Clear;
   end;
end;

// OnTileDestroyed
//
procedure TGLTerrainRenderer.OnTileDestroyed(sender : TObject);
var
   list : TList;
begin
   with sender as THeightData do begin
      if ObjectTag<>nil then begin
         ObjectTag.Free;
         ObjectTag:=nil;
      end;
      list:=FTilesHash[HashKey(XLeft, YTop)];
      Assert(Assigned(list));
      list.Remove(Sender);
   end;
end;

// InterpolatedHeight (hmg)
//
function TGLTerrainRenderer.InterpolatedHeight(const p : TVector) : Single;
var
   pLocal : TVector;
begin
   if Assigned(HeightDataSource) then begin
      pLocal:=AbsoluteToLocal(p);
      Result:=HeightDataSource.InterpolatedHeight(pLocal[0], pLocal[1], TileSize+1)*Scale.Z*(1/128);
   end else Result:=0;
end;

// InterpolatedHeight (affine)
//
function TGLTerrainRenderer.InterpolatedHeight(const p : TAffineVector) : Single;
begin
   Result:=InterpolatedHeight(PointMake(p));
end;

// BuildList
//
procedure TGLTerrainRenderer.BuildList(var rci : TRenderContextInfo);
var
   vEye, vEyeDirection : TVector;
   tilePos, absTilePos, observer : TAffineVector;
   deltaX, nbX, iX : Integer;
   deltaY, nbY, iY : Integer;
   n, rpIdxDelta, accumCount : Integer;
   f, tileRadius, tileGroundRadius, texFactor, tileDist, qDist : Single;
   patch, prevPatch : TGLROAMPatch;
   patchList, rowList, prevRow, buf : TList;
   postRenderPatchList, postRenderHeightDataList : TList;
   rcci : TRenderContextClippingInfo;
   currentMaterialName : String;
   maxTilePosX, maxTilePosY, minTilePosX, minTilePosY : Single;
   t_l, t_t, t_r, t_b : Single;
   hd : THeightData;

   procedure ApplyMaterial(const materialName : String);
   begin
      if (MaterialLibrary<>nil) and (currentMaterialName<>materialName) then begin
         // flush whatever is in progress
         TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);
         // unapply current
         if currentMaterialName='' then begin
            repeat
               // ... proper multipass support will be implemented later
            until not Material.UnApply(rci)
         end else begin
            repeat
               // ... proper multipass support will be implemented later
            until not MaterialLibrary.UnApplyMaterial(rci);
         end;
         // apply new
         if materialName='' then
            Material.Apply(rci)
         else MaterialLibrary.ApplyMaterial(materialName, rci);
         currentMaterialName:=materialName;
      end;
   end;

begin
   if csDesigning in ComponentState then Exit;
   if HeightDataSource=nil then Exit;
   currentMaterialName:='';
   // first project eye position into heightdata coordinates
   vEye:=VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
   vEyeDirection:=VectorTransform(rci.cameraDirection, InvAbsoluteMatrix);
   SetVector(observer, vEye);
   vEye[0]:=Round(vEye[0]*FinvTileSize-0.5)*TileSize+TileSize*0.5;
   vEye[1]:=Round(vEye[1]*FinvTileSize-0.5)*TileSize+TileSize*0.5;
   tileGroundRadius:=Sqr(TileSize*0.5*Scale.X)+Sqr(TileSize*0.5*Scale.Y);
   tileRadius:=Sqrt(tileGroundRadius+Sqr(256*Scale.Z));
   tileGroundRadius:=Sqrt(tileGroundRadius);
   // now, we render a quad grid centered on eye position
   SetVector(tilePos, vEye);
   tilePos[2]:=0;
   f:=(rci.rcci.farClippingDistance+tileGroundRadius)/Scale.X;
   f:=Round(f*FinvTileSize+1.0)*TileSize;
   maxTilePosX:=vEye[0]+f;
   maxTilePosY:=vEye[1]+f;
   minTilePosX:=vEye[0]-f;
   minTilePosY:=vEye[1]-f;

   texFactor:=1/(TilesPerTexture*TileSize);
   rcci:=rci.rcci;
   if QualityDistance>0 then
      qDist:=QualityDistance+tileRadius*0.5
   else qDist:=-1;

   SetROAMTrianglesCapacity(MaxCLODTriangles);
   n:=MaxInteger(MaxCLODTriangles*2, Sqr(TileSize+1)*2);
   FBufferVertices.Capacity:=n;
   FBufferTexPoints.Capacity:=n;

   xglPushState;
   try
      if GL_ARB_multitexture then
         xglMapTexCoordToDual
      else xglMapTexCoordToMain;

      glPushMatrix;
      glScalef(1, 1, 1/128);
      glTranslatef(-0.5*TileSize, -0.5*TileSize, 0);
      glEnableClientState(GL_VERTEX_ARRAY);
      xglEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glDisableClientState(GL_COLOR_ARRAY);
      glDisableClientState(GL_NORMAL_ARRAY);

      glVertexPointer(3, GL_FLOAT, 0, FBufferVertices.List);
      xglTexCoordPointer(2, GL_FLOAT, 0, FBufferTexPoints.List);
   finally
      xglPopState;
   end;

   if Assigned(FOnGetTerrainBounds) then begin
      // User-specified terrain bounds, may override ours
      t_l:=minTilePosX;
      t_t:=maxTilePosY;
      t_r:=maxTilePosX;
      t_b:=minTilePosY;

      FOnGetTerrainBounds(t_l, t_t, t_r, t_b);

      t_l:=Round(t_l/TileSize-0.5)*TileSize+TileSize*0.5;
      t_t:=Round(t_t/TileSize-0.5)*TileSize-TileSize*0.5;
      t_r:=Round(t_r/TileSize-0.5)*TileSize-TileSize*0.5;
      t_b:=Round(t_b/TileSize-0.5)*TileSize+TileSize*0.5;

      if maxTilePosX>t_r then maxTilePosX:=t_r;
      if maxTilePosY>t_t then maxTilePosY:=t_t;
      if minTilePosX<t_l then minTilePosX:=t_l;
      if minTilePosY<t_b then minTilePosY:=t_b;
   end;
   nbX:=Round((maxTilePosX-minTilePosX)/TileSize);
   nbY:=Round((maxTilePosY-minTilePosY)/TileSize);

   FLastTriangleCount:=0;

   patchList:=TList.Create;
   patchList.Capacity:=(nbX+1)*(nbY+1);
   rowList:=TList.Create;
   prevRow:=TList.Create;
   if Assigned(FOnPatchPostRender) then
      postRenderPatchList:=TList.Create
   else postRenderPatchList:=nil;
   if Assigned(FOnHeightDataPostRender) then
      postRenderHeightDataList:=TList.Create
   else postRenderHeightDataList:=nil;

   MarkAllTilesAsUnused;
   AbsoluteMatrix; // makes sure it is available

   // determine orientation (to render front-to-back)
   if vEyeDirection[0]>=0 then
      deltaX:=TileSize
   else begin
      deltaX:=-TileSize;
      minTilePosX:=maxTilePosX;
   end;
   if vEyeDirection[1]>=0 then
      deltaY:=TileSize
   else begin
      deltaY:=-TileSize;
      minTilePosY:=maxTilePosY;
   end;

   tileRadius:=tileRadius;

   tilePos[1]:=minTilePosY;
   for iY:=0 to nbY-1 do begin
      tilePos[0]:=minTilePosX;
      prevPatch:=nil;
      n:=0;
      for iX:=0 to nbX do begin
         absTilePos:=VectorTransform(tilePos, DirectAbsoluteMatrix^);
         if not IsVolumeClipped(absTilePos, tileRadius, rcci) then begin
            patch:=GetPreparedPatch(tilePos, observer, texFactor,
                                    postRenderHeightDataList);

            if patch<>nil then begin

               tileDist:=VectorDistance(PAffineVector(@rcci.origin)^, absTilePos);
               patch.HighRes:=(tileDist<qDist);

               if not patch.HighRes then
                  patch.ResetTessellation;
               if Assigned(prevPatch) then begin
                  if deltaX>0 then
                     patch.ConnectToTheWest(prevPatch)
                  else prevPatch.ConnectToTheWest(patch);
               end;
               if (prevRow.Count>n) and (prevRow.List[n]<>nil) then begin
                  if deltaY>0 then
                     patch.ConnectToTheNorth(TGLROAMPatch(prevRow.List[n]))
                  else TGLROAMPatch(prevRow.List[n]).ConnectToTheNorth(patch);
               end;

               if patch.HighRes then begin
                  // high-res patches are issued immediately
                  ApplyMaterial(patch.HeightData.MaterialName);
                  patch.RenderHighRes(FBufferVertices, FBufferVertexIndices, FBufferTexPoints,
                                      (QualityStyle=hrsTesselated));
                  FLastTriangleCount:=FLastTriangleCount+patch.TriangleCount;
               end else begin
                  // CLOD patches are issued after tesselation
                  patchList.Add(patch);
               end;

               prevPatch:=patch;
               rowList.Add(patch);

               if Assigned(postRenderPatchList) then
                  postRenderPatchList.Add(patch);
            end else begin
               prevPatch:=nil;
               rowList.Add(nil);
            end;
         end else begin
            MarkHashedTileAsUsed(tilePos);
            prevPatch:=nil;
            rowList.Add(nil);
         end;
         tilePos[0]:=tilePos[0]+deltaX;
         Inc(n);
      end;
      tilePos[1]:=tilePos[1]+deltaY;
      buf:=prevRow;
      prevRow:=rowList;
      rowList:=buf;
      rowList.Count:=0;
   end;

   accumCount:=FBufferVertexIndices.Capacity shr 3;

   // Interleave Tesselate and Render so we can send some work to the hardware
   // while the CPU keeps working
   rpIdxDelta:=Round(2*f/TileSize)+2;
   for n:=0 to patchList.Count-1+rpIdxDelta do begin
      if n<patchList.Count then begin
         patch:=TGLROAMPatch(patchList[n]);
         if Assigned(patch) then begin
            if    (patch.LastOcclusionTestPassed)
               or (patch.OcclusionCounter<=0)
               or (OcclusionTesselate=totTesselateAlways) then
               patch.Tesselate;
         end;
      end;
      if n>=rpIdxDelta then begin
         patch:=TGLROAMPatch(patchList[n-rpIdxDelta]);
         if Assigned(patch) then begin
            ApplyMaterial(patch.HeightData.MaterialName);
            patch.RenderAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints,
                              accumCount);
            Inc(FLastTriangleCount, patch.TriangleCount);
         end;
      end;
   end;
   TGLROAMPatch.FlushAccum(FBufferVertices, FBufferVertexIndices, FBufferTexPoints);

   xglPushState;
   try
      if GL_ARB_multitexture then
         xglMapTexCoordToDual
      else xglMapTexCoordToMain;

      glDisableClientState(GL_VERTEX_ARRAY);
      xglDisableClientState(GL_TEXTURE_COORD_ARRAY);
   finally
      xglPopState;
   end;

   ApplyMaterial('');
   if Assigned(postRenderPatchList) then begin
      FOnPatchPostRender(rci, postRenderPatchList);
      postRenderPatchList.Free;
   end;
   if Assigned(postRenderHeightDataList) then begin
      FOnHeightDataPostRender(rci, postRenderHeightDataList);
      postRenderHeightDataList.Free;
   end;

   glPopMatrix;

   ReleaseAllUnusedTiles;
   HeightDataSource.CleanUp;

   rowList.Free;
   prevRow.Free;
   patchList.Free;
end;

// MarkAllTilesAsUnused
//
procedure TGLTerrainRenderer.MarkAllTilesAsUnused;
var
   i, j, zero : Integer;
   pList : PPointerList;
begin
   for i:=0 to cTilesHashSize do with FTilesHash[i] do begin
      pList:=List;
      zero:=0;
      for j:=Count-1 downto 0 do
         THeightData(pList[j]).Tag:=zero;
   end;
end;

// ReleaseAllUnusedTiles
//
procedure TGLTerrainRenderer.ReleaseAllUnusedTiles;
var
   i, j : Integer;
   hashList : TList;
   hd : THeightData;
begin
   for i:=0 to cTilesHashSize do begin
      hashList:=FTilesHash[i];
      for j:=hashList.Count-1 downto 0 do begin
         hd:=THeightData(hashList.List[j]);
         if hd.Tag=0 then begin
            hashList.Delete(j);
            OnTileDestroyed(hd);
            hd.OnDestroy:=nil;
            hd.Release;
         end;
      end;
   end;
end;

// MarkHashedTileAsUsed
//
procedure TGLTerrainRenderer.MarkHashedTileAsUsed(const tilePos : TAffineVector);
var
   hd : THeightData;
begin
   hd:=HashedTile(tilePos);
   if Assigned(hd) then hd.Tag:=1;
end;

// HashedTile
//
function TGLTerrainRenderer.HashedTile(const tilePos : TAffineVector; canAllocate : Boolean = True) : THeightData;
var
   xLeft, yTop : Integer;
begin
   xLeft:=Round(tilePos[0]*FinvTileSize-0.5)*(TileSize);
   yTop:=Round(tilePos[1]*FinvTileSize-0.5)*(TileSize);
   Result:=HashedTile(xLeft, yTop, canAllocate);
end;

// HashedTile
//
function TGLTerrainRenderer.HashedTile(const xLeft, yTop : Integer; canAllocate : Boolean = True) : THeightData;
var
   i : Integer;
   hd : THeightData;
   hashList : TList;
   pList : PPointerList;
begin
   // is the tile already in our list?
   hashList:=FTilesHash[HashKey(xLeft, yTop)];
   pList:=hashList.List;
   for i:=hashList.Count-1 downto 0 do begin
      hd:=THeightData(pList[i]);
      if (hd.XLeft=xLeft) and (hd.YTop=yTop) then begin
         Result:=hd;
         Exit;
      end;
   end;
   // if not, request it
   if canAllocate then begin
      Result:=HeightDataSource.GetData(xLeft, yTop, TileSize+1, hdtSmallInt);
      Result.RegisterUse;
      Result.OnDestroy:=OnTileDestroyed;
      if Result.DataState<>hdsNone then
         Result.DataType:=hdtSmallInt;
      FTilesHash[HashKey(xLeft, yTop)].Add(Result);
   end else Result:=nil;
end;

// GetPreparedPatch
//
function TGLTerrainRenderer.GetPreparedPatch(const tilePos, eyePos : TAffineVector;
                                             texFactor : Single; hdList : TList) : TGLROAMPatch;
var
   tile : THeightData;
   patch : TGLROAMPatch;
   xLeft, yTop : Integer;
begin
   xLeft:=Round(tilePos[0]*FinvTileSize-0.5)*TileSize;
   yTop:=Round(tilePos[1]*FinvTileSize-0.5)*TileSize;
   tile:=HashedTile(xLeft, yTop);
   if Assigned(hdList) then
      hdList.Add(tile);
   if tile.DataState=hdsNone then begin
      tile.Tag:=1;
      Result:=nil;
   end else begin
      tile.Tag:=1;
      patch:=TGLROAMPatch(tile.ObjectTag);
      if not Assigned(patch) then begin
         // spawn ROAM patch
         patch:=TGLROAMPatch.Create;
         tile.ObjectTag:=patch;
         patch.HeightData:=tile;
         patch.VertexScale:=XYZVector;
         patch.VertexOffset:=tilePos;
         patch.OcclusionSkip:=OcclusionFrameSkip;
         case tile.TextureCoordinatesMode of
            tcmWorld : begin
               patch.TextureScale:=AffineVectorMake(texFactor, -texFactor, texFactor);
               patch.TextureOffset:=AffineVectorMake(xLeft*texFactor, 1-yTop*texFactor, 0);
            end;
            tcmLocal : begin
               with tile.TextureCoordinatesScale do
               patch.TextureScale:=AffineVectorMake(texFactor*S, -texFactor*T, texFactor);
               with tile.TextureCoordinatesOffset do
                  patch.TextureOffset:=AffineVectorMake(0+S, 1+T, 0);
            end;
         else
            Assert(False);
         end;
         patch.ComputeVariance(FCLODPrecision);
      end;
      patch.ObserverPosition:=VectorSubtract(eyePos, tilePos);
      Result:=patch;
   end;
end;

// SetHeightDataSource
//
procedure TGLTerrainRenderer.SetHeightDataSource(const val : THeightDataSource);
begin
   if FHeightDataSource<>val then begin
      if Assigned(FHeightDataSource) then begin
         FHeightDataSource.RemoveFreeNotification(Self);
         ReleaseAllTiles;
         FHeightDataSource.Clear;
      end;
      FHeightDataSource:=val;
      if Assigned(FHeightDataSource) then
         FHeightDataSource.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetTileSize
//
procedure TGLTerrainRenderer.SetTileSize(const val : Integer);
begin
   if val<>FTileSize then begin
      if val<8 then
         FTileSize:=8
      else FTileSize:=RoundUpToPowerOf2(val);
      FinvTileSize:=1/FTileSize;
      ReleaseAllTiles;
      StructureChanged;
   end;
end;

// SetTilesPerTexture
//
procedure TGLTerrainRenderer.SetTilesPerTexture(const val : Single);
begin
   if val<>FTilesPerTexture then begin
      FTilesPerTexture:=val;
      StructureChanged;
   end;
end;

// SetCLODPrecision
//
procedure TGLTerrainRenderer.SetCLODPrecision(const val : Integer);
var
   i, k : Integer;
   hd : THeightData;
begin
   if val<>FCLODPrecision then begin
      FCLODPrecision:=val;
      if FCLODPrecision<1 then FCLODPrecision:=1;
      // drop all ROAM data (CLOD has changed, rebuild required)
      for i:=0 to cTilesHashSize do with FTilesHash[i] do begin
         for k:=Count-1 downto 0 do begin
            hd:=THeightData(List[k]);
            if Assigned(hd.ObjectTag) then begin
               (hd.ObjectTag as TGLROAMPatch).Free;
               hd.ObjectTag:=nil;
            end;
         end;
         Clear;
      end;
   end;
end;

// SetMaterialLibrary
//
procedure TGLTerrainRenderer.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   if val<>FMaterialLibrary then begin
      FMaterialLibrary:=val;
      StructureChanged;
   end;
end;

// SetQualityStyle
//
procedure TGLTerrainRenderer.SetQualityStyle(const val : TTerrainHighResStyle);
begin
   if val<>FQualityStyle then begin
      FQualityStyle:=val;
      StructureChanged;
   end;
end;

// SetOcclusionFrameSkip
//
procedure TGLTerrainRenderer.SetOcclusionFrameSkip(val : Integer);
var
   i, k : Integer;
   hd : THeightData;
begin
   if val<0 then val:=0;
   if FOcclusionFrameSkip<>val then begin
      FOcclusionFrameSkip:=val;
      for i:=0 to cTilesHashSize do with FTilesHash[i] do begin
         for k:=Count-1 downto 0 do begin
            hd:=THeightData(List[k]);
            if hd.ObjectTag<>nil then
               TGLROAMPatch(hd.ObjectTag).OcclusionSkip:=OcclusionFrameSkip;
         end;
      end;
      NotifyChange(Self);
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TGLTerrainRenderer);

end.
