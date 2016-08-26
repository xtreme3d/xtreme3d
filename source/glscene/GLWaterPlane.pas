// GLWaterPlane

{ Modified by skinhat.
  Includes sin waves to simulate a calm ocean or lake. Includes the new
  properties SmoothwaveHeight and
   SmoothwaveFrequency. Smoothwaveheight defines the height of a sin wave
   (relative units) and smoothwavefrequency defines the frequency of the
   sine waves (relative units also). The rain waves and sine waves can
   run together but sine waves modify the rain waves (and not vice versa).

   The only real modification made in the  IterComputeNormals procedure where
   the rain waves are modified using sin waves.
}



{: A plane simulating animated water<p>



	<b>History : </b><font size=-1><ul>
      <li>02/04/03 - EG - More optimizations, mask support
      <li>01/04/03 - EG - Cleanup and optimizations
      <li>14/11/03 - Mrqzzz - Tried "CreateRippleAtWorldPos" to work at any position/rotation, but need expert's help.. :(
      <li>13/11/03 - Mrqzzz - Tried to add timing indipendence (quite not precise yet)
      <li>12/11/03 - Mrqzzz - Added some properties & small optims added
      <li>01/01/03 - Sternas Stefanos - Original code
   </ul></font>

   <p>The Original Code is part of Cosmos4D<br>
   http://users.hol.gr/~sternas/<br>
   Sternas Stefanos 2003
}
unit GLWaterPlane;

interface

uses Classes, VectorTypes, VectorGeometry, GLScene, GLTexture,
   GLMisc, OpenGL1x, VectorLists, GLCrossPlatform, PersistentClasses, sysUtils,windows;

type

   // TGLWaterPlaneOption
   //
   TGLWaterPlaneOption = (wpoTextured);
   TGLWaterPlaneOptions = set of TGLWaterPlaneOption;

const
   cDefaultWaterPlaneOptions = [wpoTextured];

type

   // TGLWaterPlane
   //
   TGLWaterPlane = class (TGLSceneObject)
		private
         { Private Declarations }
         FLocks : packed array of ByteBool;
         FPositions, FVelocity : packed array of Single;
         FPlaneQuadIndices : TPersistentObjectList;
         FPlaneQuadTexCoords : TTexPointList;
         FPlaneQuadVertices : TAffineVectorList;
         FPlaneQuadNormals : TAffineVectorList;
         FActive : Boolean;
         FRainTimeInterval : Integer;
         FRainForce : Single;
         FViscosity : Single;
         FElastic : Single;
         FResolution : Integer;
         FSimulationFrequency, FTimeToNextUpdate : Single;
         FTimeToNextRainDrop : Single;
         FMaximumCatchupIterations : Integer;
         FLastIterationStepTime : Single;
         FMask : TGLPicture;
         FOptions : TGLWaterPlaneOptions;
         FSmoothWaveHeight : Single;
         FSmoothWaveFrequency : single;

      protected
         { Protected Declarations }
         procedure SetElastic(const value : Single);
         procedure SetResolution(const value : Integer);
         procedure SetRainTimeInterval(const val : Integer);
         procedure SetViscosity(const val : Single);
         procedure SetRainForce(const val : Single);
         procedure SetSimulationFrequency(const val : Single);
         procedure SetMask(val : TGLPicture);
         procedure SetOptions(const val : TGLWaterPlaneOptions);

         procedure DoMaskChanged(Sender : TObject);
         procedure InitResolution;

         procedure IterComputeVelocity;
         procedure IterComputePositions;
         procedure IterComputeNormals;
         procedure Iterate;

      public
         { Public Declarations }
         iTick:Longint;
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoProgress(const progressTime : TProgressTimes); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;

         procedure CreateRippleAtGridPos(X,Y:integer);
         procedure CreateRippleAtWorldPos(const x, y, z : Single); overload;
         procedure CreateRippleAtWorldPos(const pos : TVector); overload;
         procedure CreateRippleRandom;
         procedure Reset;

         {: CPU time (in seconds) taken by the last iteration step. }
         property LastIterationStepTime : Single read FLastIterationStepTime;

      published
         { Published Declarations }

         property Active : Boolean read FActive write FActive default True;

         {: Delay between raindrops in milliseconds (0 = no rain) }
         property RainTimeInterval : Integer read FRainTimeInterval write SetRainTimeInterval default 500;
         property RainForce : Single read FRainForce write SetRainForce;

         property Viscosity : Single read FViscosity write SetViscosity ;
         property Elastic : Single read FElastic write SetElastic;
         property Resolution : Integer read FResolution write SetResolution default 64;
         property Options : TGLWaterPlaneOptions read FOptions write SetOptions default cDefaultWaterPlaneOptions;
         property SmoothwaveHeight:Single read FSmoothWaveHeight write FSmoothWaveHeight;
         property SmoothwaveFrequency:Single read FSmoothWaveFrequency write FSmoothWaveFrequency;

         {: A picture whose pixels determine what part of the waterplane is active.<p>
            Pixels with a green/gray component beyond 128 are active, the others
            are not (in short, white = active, black = inactive).<p>
            The picture will automatically be stretched to match the resolution. }
         property Mask : TGLPicture read FMask write SetMask;

         {: Maximum frequency (in Hz) at which simulation iterations happen. }
         property SimulationFrequency : Single read FSimulationFrequency write SetSimulationFrequency;
         {: Maximum number of simulation iterations during catchups.<p>
            Catchups happen when for a reason or another, the DoProgress doesn't
            happen as fast SimulationFrequency. }
         property MaximumCatchupIterations : Integer read FMaximumCatchupIterations write FMaximumCatchupIterations default 1;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// Create
//
constructor TGLWaterPlane.Create(AOwner : TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];

   FElastic:=10;
   FActive:=True;
   FRainTimeInterval:=500;
   FRainForce:=5000;
   FViscosity:=0.99;
   FSimulationFrequency:=100; // 100 Hz
   FMaximumCatchupIterations:=1;
   FOptions:=cDefaultWaterPlaneOptions;

   FPlaneQuadIndices:=TPersistentObjectList.Create;
   FPlaneQuadTexCoords:=TTexPointList.Create;
   FPlaneQuadVertices:=TAffineVectorList.Create;
   FPlaneQuadNormals:=TAffineVectorList.Create;
   FMask:=TGLPicture.Create;
   FMask.OnChange:=DoMaskChanged;

   FSmoothWaveHeight:=1;
   FSmoothWaveFrequency:=0;
//GL1DPerlin:=TGL1DPerlin.create(self);
//gl1dperlin.number_of_octaves:=6;
//gl1dperlin.Persistence:=1;

//GL2DPerlin:=TGL2DPerlin.create(nil);
//gl2dperlin.number_of_octaves:=8;
//gl2dperlin.Persistence:=1;

   iTick:=getTickCount;

   SetResolution(64);
end;

// Destroy
//
destructor TGLWaterPlane.Destroy;
begin
   //      GL2DPerlin.free;
   FMask.Free;
   FPlaneQuadNormals.Free;
   FPlaneQuadVertices.Free;
   FPlaneQuadTexCoords.Free;
   FPlaneQuadIndices.CleanFree;
   inherited;
end;

// DoProgress
//
procedure TGLWaterPlane.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
begin
   inherited;

   //    gl2dperlin.generate;

   if Active and Visible then begin
      // new raindrops
      if FRainTimeInterval>0 then begin
         FTimeToNextRainDrop:=FTimeToNextRainDrop-progressTime.deltaTime;
         i:=FMaximumCatchupIterations;
         while FTimeToNextRainDrop<=0 do begin
            CreateRippleRandom;
            FTimeToNextRainDrop:=FTimeToNextRainDrop+FRainTimeInterval*0.001;
            Dec(i);
            if i<0 then begin
               if FTimeToNextRainDrop<0 then FTimeToNextRainDrop:=FRainTimeInterval*0.001;
               Break;
            end;
         end;
      end;
      // iterate simulation
      FTimeToNextUpdate:=FTimeToNextUpdate-progressTime.deltaTime;
      if FTimeToNextUpdate<=0 then begin
         i:=FMaximumCatchupIterations;
         while FTimeToNextUpdate<=0 do begin
            Iterate;
            FTimeToNextUpdate:=FTimeToNextUpdate+1/FSimulationFrequency;
            Dec(i);
            if i<0 then begin
               if FTimeToNextUpdate<0 then FTimeToNextUpdate:=1/FSimulationFrequency;
               Break;
            end;
         end;
         StructureChanged;
      end;
   end;
end;

// CreateRippleAtGridPos
//
procedure TGLWaterPlane.CreateRippleAtGridPos(x, y : Integer);
var
 i:integer;
begin
   if (x>0) and (y>0) and (x<Resolution-1) and (y<Resolution-1) then
   begin
      FVelocity[x+y*Resolution]:=FRainForce;
     // for i:=x-5 to x+5 do
     // FVelocity[{x}i+y*Resolution]:=FRainForce;
   end;
end;

// CreateRippleAtWorldPos
//
procedure TGLWaterPlane.CreateRippleAtWorldPos(const x, y, z : Single);
var
   vv : TVector;
begin
   vv:=AbsoluteToLocal(PointMake(x, y, z));
   CreateRippleAtGridPos(Round((vv[0]+0.5)*Resolution),
                         Round((vv[2]+0.5)*Resolution));
end;

// CreateRippleAtWorldPos
//
procedure TGLWaterPlane.CreateRippleAtWorldPos(const pos : TVector);
var
   vv : TVector;
begin
   vv:=AbsoluteToLocal(PointMake(pos));
   CreateRippleAtGridPos(Round((vv[0]+0.5)*Resolution),
                         Round((vv[2]+0.5)*Resolution));
end;

// CreateRippleRandom
//
procedure TGLWaterPlane.CreateRippleRandom;
begin
   CreateRippleAtGridPos(Random(Resolution-3)+2, Random(Resolution-3)+2);
end;

// InitResolution
//
procedure TGLWaterPlane.InitResolution;
var
   i, j : Integer;
   v : TAffineVector;
   resSqr : Integer;
   invResol : Single;
begin
   resSqr:=FResolution*FResolution;
   FPlaneQuadIndices.Capacity:=resSqr*2;
   FPlaneQuadTexCoords.Clear;
   FPlaneQuadTexCoords.Capacity:=resSqr;
   FPlaneQuadVertices.Clear;
   FPlaneQuadVertices.Capacity:=resSqr;

   invResol:=1/Resolution;
   for j:=0 to Resolution-1 do begin
      for i:=0 to Resolution-1 do begin
         FPlaneQuadTexCoords.Add(i*invResol, j*invResol);
         FPlaneQuadVertices.Add((i-Resolution*0.5)*invResol,
                                0,
                                (j-Resolution*0.5)*invResol);
      end;
   end;

   FPlaneQuadNormals.Count:=resSqr;
   v[0]:=0;
   v[1]:=2048;
   v[2]:=0;
   for i:=0 to FPlaneQuadNormals.Count-1 do
      FPlaneQuadNormals.List[i]:=v;

   SetLength(FPositions, resSqr);
   SetLength(FVelocity, resSqr);
   SetLength(FLocks, resSqr);

   Reset;
   Iterate;

   StructureChanged;
end;

// Reset
//
procedure TGLWaterPlane.Reset;
var
   i, j, ij, resSqr : Integer;
   maskBmp : TGLBitmap;
   scanLine : PIntegerArray;
   il : TIntegerList;
   locked : Boolean;
begin
   resSqr:=FResolution*FResolution;
   for i:=0 to resSqr-1 do begin
      FPositions[i]:=0;
      FVelocity[i]:=0;
      FLocks[i]:=False;
   end;
   if FMask.Width>0 then begin
      maskBmp:=TGLBitmap.Create;
      try
         maskBmp.PixelFormat:=glpf32bit;
         maskBmp.Width:=Resolution;
         maskBmp.Height:=Resolution;
         maskBmp.Canvas.StretchDraw(Rect(0, 0, Resolution, Resolution), FMask.Graphic);
         for j:=0 to Resolution-1 do begin
            scanLine:=maskBmp.ScanLine[Resolution-1-j];
            for i:=0 to Resolution-1 do
               FLocks[i+j*Resolution]:=(((scanLine[i] shr 8) and $FF)<128);
         end;
      finally
         maskBmp.Free;
      end;
   end;

   FPlaneQuadIndices.Clean;
   for j:=0 to Resolution-2 do begin
      il:=TIntegerList.Create;
      for i:=0 to Resolution-1 do begin
         ij:=i+j*Resolution;
         if (il.Count and 2)<>0 then
            locked:=False
         else begin
            locked:=FLocks[ij] and FLocks[ij+Resolution];
            if locked and (i<Resolution-1) then
               locked:=FLocks[ij+1] and FLocks[ij+Resolution+1];
         end;
         if not locked then
            il.Add(ij, ij+Resolution)
         else if il.Count>0 then begin
            FPlaneQuadIndices.Add(il);
            il:=TIntegerList.Create;
         end;
      end;
      if il.Count>0 then
         FPlaneQuadIndices.Add(il)
      else il.Free;
   end;
end;

// IterComputeVelocity
//
procedure TGLWaterPlane.IterComputeVelocity;
var
   i, j, ij : Integer;
   f1, f2 : Single;
   posList, velList : PSingleArray;
   lockList : PByteArray;
begin
   f1:=0.05;
   f2:=0.01*FElastic;

   posList:=@FPositions[0];
   velList:=@FVelocity[0];
   lockList:=@FLocks[0];
   for i:=1 to Resolution-2 do begin
      ij:=i*Resolution;
      for j:=1 to Resolution-2 do begin
         Inc(ij);
         if lockList[ij]<>0 then continue;
         velList[ij]:= //cos(ij+kk mod 3);{//sin((kk+ij)/10){
         velList[ij]
                      +f2*( posList[ij]
                           -f1*(4*( posList[ij-1]         +posList[ij+1]
                                    +posList[ij-Resolution]+posList[ij+Resolution])
                                +posList[ij-1-Resolution]+posList[ij+1-Resolution]
                                +posList[ij-1+Resolution]+posList[ij+1+Resolution]));
      end;
   end;
end;

// IterComputePositions
//
procedure TGLWaterPlane.IterComputePositions;
const
   cVelocityIntegrationCoeff : Single = 0.02;
   cHeightFactor : Single = 1e-4;
var
   ij : Integer;
   f  : Single;
   coeff : Single;
   posList, velList : PSingleArray;
   lockList : PByteArray;
   i,j:integer;

begin
   // Calculate the new ripple positions and update vertex coordinates
   coeff:=cVelocityIntegrationCoeff*Resolution;
   f:=cHeightFactor/Resolution;
   posList:=@FPositions[0];
   velList:=@FVelocity[0];
   lockList:=@FLocks[0];
   //for i:=0 to resolution-1 do
    // for j:=0 to resolution-1 do
   // begin
       //ij:=i*j;

{       cx=100
cy=75

for y=0 to 199
  for x=0 to 319
    xx = x-cx
    yy = y-cy
    d = square_root(xx*xx-yy*yy) * .1
    plot_pixel(x,y, (sin(d)+1)*31)
  end of x loop
end of y loop
}

   for ij:=0 to Resolution*Resolution-1 do
     if lockList[ij]=0 then
     begin
       posList[ij]:=posList[ij]-coeff*velList[ij];
       velList[ij]:=velList[ij]*FViscosity;
       FPlaneQuadVertices.List[ij][1]:=posList[ij]*f;
     end;


end;

// IterComputeNormals
//
procedure TGLWaterPlane.IterComputeNormals;
var
   i, j, ij : Integer;
   pv : PAffineVector;
   posList : PSingleArray;
   normList : PAffineVectorArray;
   ijm1, ijp1, ijpresolution,ijmresolution:single;
   FrequencyFactor:single;
   tickdiff:integer;
begin
   // Calculate the new vertex normals (not normalized, the hardware will handle that)
   posList:=@FPositions[0];
   normList:=FPlaneQuadNormals.List;
           tickdiff:=(gettickcount-iTick);
   for i:=1 to Resolution-2 do begin
      ij:=i*Resolution;
      for j:=1 to Resolution-2 do begin
         Inc(ij);
         pv:=@normList[ij];
         ijm1:=posList[ij-1];
         ijp1:=posList[ij+1];
         ijmresolution:=posList[ij-Resolution];
         ijpresolution:=posList[ij+Resolution];
         if FSmoothWaveFrequency>0 then
         begin
           FrequencyFactor:=3/FSmoothWaveFrequency;
           ijm1:=ijm1+FSmoothWaveHeight*2000*sin((ij-1)/FrequencyFactor-0.01*tickdiff/FrequencyFactor);
           ijp1:=ijp1+FSmoothWaveHeight*2000*sin((ij+1)/FrequencyFactor-0.01*tickdiff/FrequencyFactor);
           ijmresolution:=ijmresolution+FSmoothWaveHeight*2000*sin((ij-Resolution)/FrequencyFactor-0.01*tickdiff/FrequencyFactor);
           ijpresolution:=ijpresolution+FSmoothWaveHeight*2000*sin((ij+Resolution)/FrequencyFactor-0.01*tickdiff/FrequencyFactor);
         end;
         pv[0]:=ijp1-ijm1;
         pv[2]:=ijpresolution-ijmresolution;
         //pv[0]:=posList[ij+1]-posList[ij-1];
        // pv[2]:=posList[ij+Resolution]-posList[ij-Resolution];
      end;
   end;
end;

// Iterate
//
procedure TGLWaterPlane.Iterate;
var
   t : Int64;
begin
   if Visible then begin
      t:=StartPrecisionTimer;
      //if FRainTimeInterval>0 then
        IterComputeVelocity;
      IterComputePositions;
      IterComputeNormals;

      FLastIterationStepTime:=StopPrecisionTimer(t);
   end;
end;

// BuildList
//
procedure TGLWaterPlane.BuildList(var rci : TRenderContextInfo);
var
   i : Integer;
   il : TIntegerList;
begin
   glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

   glEnableClientState(GL_VERTEX_ARRAY);
   glVertexPointer(3, GL_FLOAT, 0, FPlaneQuadVertices.List);
   glEnableClientState(GL_NORMAL_ARRAY);
   glNormalPointer(GL_FLOAT, 0, FPlaneQuadNormals.List);
   if wpoTextured in Options then begin
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glTexCoordPointer(2, GL_FLOAT, 0, FPlaneQuadTexCoords.List);
   end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);

   if GL_EXT_compiled_vertex_array then
      glLockArraysEXT(0, FPlaneQuadVertices.Count);

   for i:=0 to FPlaneQuadIndices.Count-1 do begin
      il:=TIntegerList(FPlaneQuadIndices[i]);
      glDrawElements(GL_QUAD_STRIP, il.Count, GL_UNSIGNED_INT, il.List);
   end;

   if GL_EXT_compiled_vertex_array then
      glUnLockArraysEXT;

   glPopClientAttrib;
end;

// Assign
//
procedure TGLWaterPlane.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLWaterPlane) then begin
      Active:=TGLWaterPlane(Source).Active;
      RainTimeInterval:=TGLWaterPlane(Source).RainTimeInterval;
      RainForce:=TGLWaterPlane(Source).RainForce;
      Viscosity:=TGLWaterPlane(Source).Viscosity;
   end;
   inherited Assign(Source);
end;

// SetElastic
//
procedure TGLWaterPlane.SetElastic(const Value: single);
begin
   FElastic:=Value;
end;

// SetResolution
//
procedure TGLWaterPlane.SetResolution(const value : Integer);
begin
   if value<>FResolution then begin
      FResolution:=Value;
      if FResolution<16 then FResolution:=16;
      InitResolution;
   end;
end;

// SetRainTimeInterval
//
procedure TGLWaterPlane.SetRainTimeInterval(Const val:integer);
begin
   if (val>=0) and (Val<=1000000) then
   begin
      fRainTimeInterval:=val;
      if val=0 then
        InitResolution;
   end;
end;

// SetViscosity
//
Procedure TGLWaterPlane.SetViscosity(const val : Single);
begin
   if (val>=0) and (val<=1) then
      FViscosity:=val;
end;

// SetRainForce
//
procedure TGLWaterPlane.SetRainForce(const val : Single);
begin
   if (val>=0) and (val<=1000000) then
      FRainForce:=val;
end;

// SetSimulationFrequency
//
procedure TGLWaterPlane.SetSimulationFrequency(const val : Single);
begin
   if FSimulationFrequency<>val then begin
      FSimulationFrequency:=val;
      if FSimulationFrequency<1 then FSimulationFrequency:=1;
      FTimeToNextUpdate:=0;
   end;
end;

// SetMask
//
procedure TGLWaterPlane.SetMask(val : TGLPicture);
begin
   FMask.Assign(val);
end;

// DoMaskChanged
//
procedure TGLWaterPlane.DoMaskChanged(Sender : TObject);
begin
   Reset;
   StructureChanged;
end;

// SetOptions
//
procedure TGLWaterPlane.SetOptions(const val : TGLWaterPlaneOptions);
begin
   if FOptions<>val then begin
      FOptions:=val;
      StructureChanged;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLWaterPlane]);

end.
