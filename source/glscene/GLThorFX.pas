//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLThorFX<p>

  <b>History : </b><font size=-1><ul>
    <li>23/12/04 - PhP - GLScenestyled Header
    <li>06/04/04 - PhP - Removed property Paused use of property Disabled instead
    <li>04/15/03 - Added initialization to CalcThor, to fix an error
                   Thanks to Martin Kirsch for this solution
    <li>12/08/01 - EG - Dropped unused Handle allocation (leftover from FirexFX)
                        Fixed leaks (colors)
    <li>09/03/01 - René Lindsay - unit created
  </ul></font>
}
unit GLThorFX;

interface

uses Classes, GLScene, GLMisc, XCollection, VectorGeometry, GLTexture, GLCadencer;

type
  PThorpoint = ^TThorpoint;
  TThorpoint = record
    Position: TVector;  // Position
    Size: single;       // particle size
  end;

  PThorpointArray = ^TThorpointArray;
  TThorpointArray = array [0..MAXINT shr 6]of TThorpoint;

  TGLBThorFX = class;

  TCalcPointEvent = procedure(Sender: TObject;PointNo :integer; var x:single; var y:single; var z: single) of object;

	// TGLThorFXManager
	//
  {: Thor special effect manager. }
  TGLThorFXManager = class (TGLCadenceAbleComponent)
  private
  { Private Declarations }
    FClients : TList;
    FThorpoints : PThorpointArray;
    FTarget : TGLCoordinates;
    FCadencer : TGLCadencer;
    FMaxpoints : Integer;
    FGlowSize :Single;
    FVibrate  :Single;
    FWildness :Single;
    NP : Integer;
    FInnerColor, FOuterColor, FCoreColor: TGLColor;
    FDisabled, FCore, FGlow : boolean;
    FOnCalcPoint :TCalcPointEvent;
  protected
  { Protected Declarations }
    procedure RegisterClient(aClient: TGLBThorFX);
    procedure DeRegisterClient(aClient: TGLBThorFX);
    procedure DeRegisterAllClients;
    procedure SetTarget(const val: TGLCoordinates);
    procedure SetCadencer(const val: TGLCadencer);
    procedure SetMaxpoints(const val: Integer);
    function StoreGlowSize: Boolean;
    function StoreVibrate: Boolean;
    procedure SetInnerColor(const val: TGLcolor);
    procedure SetOuterColor(const val: TGLcolor);
    procedure SetCoreColor(const val: TGLcolor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ThorInit;
    procedure CalcThor;
    procedure CalcFrac(left,right:integer; lh,rh :single; xyz:integer);
  public
  { Public Declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime : TProgressTimes); override;
  published
  { Published Declarations }
    property Target: TGLCoordinates read FTarget write SetTarget;
    property Cadencer: TGLCadencer read FCadencer write SetCadencer;
    property Maxpoints: Integer read FMaxpoints write SetMaxpoints default 256;
    property GlowSize: Single read FGlowSize write FGlowSize stored StoreGlowSize;
    property Vibrate: Single read FVibrate write FVibrate stored StoreVibrate;
    property InnerColor: TGLcolor read FInnerColor write SetInnerColor;
    property OuterColor: TGLcolor read FOuterColor write SetOuterColor;// default clrWhite;
    property CoreColor: TGLcolor read FCoreColor write SetCoreColor;// default clrWhite;
    property Disabled: boolean read FDisabled write FDisabled;
    property Core: boolean read FCore write FCore;
    property Glow: boolean read FGlow write FGlow;
    property Wildness: Single read FWildness write FWildness;
    property OnCalcPoint :TCalcPointEvent read FOnCalcPoint write FOnCalcPoint;
	end;

 	// TGLBThorFX
	//
	{: Thor special effect }
	TGLBThorFX = class(TGLObjectPostEffect)
	private
  { Private Declarations }
    FManager : TGLThorFXManager;
    FManagerName : String; // NOT persistent, temporarily used for persistence
    FTarget: TGLCoordinates;
  protected
  { Protected Declarations }
    procedure SetManager(const val : TGLThorFXManager);
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure Loaded; override;
    procedure SetTarget(const val: TGLCoordinates);
  public
  { Public Declarations }
    constructor Create(aOwner : TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName : String; override;
		class function FriendlyDescription : String; override;
    procedure Render(sceneBuffer : TGLSceneBuffer; var rci : TRenderContextInfo); override;
  published
	{ Published Declarations }
    {: Refers the collision manager. }
    property Manager : TGLThorFXManager read FManager write SetManager;
	end;

{: Returns or creates the TGLBThorFX within the given object's effects.<p> }
function GetOrCreateThorFX(obj : TGLBaseSceneObject; const name : String = '') : TGLBThorFX;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, VectorLists;


// ------------------
// ------------------ TGLThorFXManager ------------------
// ------------------

// Create
//
constructor TGLThorFXManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TList.Create;
  RegisterManager(Self);
  FTarget := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
  FMaxpoints:=64;
  FGlowSize:=0.2;
  FVibrate:=0;
  FWildness:=1;
  FInnerColor:=TGLColor.Create(Self);
  FInnerColor.Initialize(clrWhite);
  FOuterColor:=TGLColor.Create(Self);
  FOuterColor.Initialize(clrBlue);
  FOuterColor.Alpha:=0;
  FCoreColor:=TGLColor.Create(Self);
  FCoreColor.Initialize(clrWhite);
  FCore := True;
  FGlow := True;
  ThorInit;
end;

// Destroy
//
destructor TGLThorFXManager.Destroy;
begin
  DeRegisterAllClients;
  DeRegisterManager(Self);
  FreeMem(FThorpoints);
  FreeAndNil(FClients);
  FreeAndNil(FInnerColor);
  FreeAndNil(FOuterColor);
  FreeAndNil(FCoreColor);  
  FreeAndNil(FTarget);
	inherited Destroy;
end;

// RegisterClient
//
procedure TGLThorFXManager.RegisterClient(aClient : TGLBThorFX);
begin
  if Assigned(aClient) then
    if FClients.IndexOf(aClient)<0 then begin
      FClients.Add(aClient);
      aClient.FManager:=Self;
    end;
end;

// DeRegisterClient
//
procedure TGLThorFXManager.DeRegisterClient(aClient : TGLBThorFX);
begin
  if Assigned(aClient) then begin
    aClient.FManager:=nil;
    FClients.Remove(aClient);
  end;
end;

// DeRegisterAllClients
//
procedure TGLThorFXManager.DeRegisterAllClients;
var
   i : Integer;
begin
   // Fast deregistration
  for i:=0 to FClients.Count-1 do
    TGLBThorFX(FClients[i]).FManager:=nil;
  FClients.Clear;
end;

procedure TGLThorFXManager.SetTarget(const val: TGLCoordinates);
begin
  FTarget.Assign(val);
  ThorInit;
end;

// SetCadencer
//
procedure TGLThorFXManager.SetCadencer(const val : TGLCadencer);
begin
  if FCadencer<>val then begin
    if Assigned(FCadencer) then
      FCadencer.UnSubscribe(Self);
    FCadencer:=val;
    if Assigned(FCadencer) then
      FCadencer.Subscribe(Self);
  end;
end;

// SetMaxpoints
//
procedure TGLThorFXManager.SetMaxpoints(const val : Integer);
begin
  if FMaxpoints<>val then begin
    FMaxpoints:=val;
    ThorInit;
  end;
end;

// StoreGlowSize
//
function TGLThorFXManager.StoreGlowSize : Boolean;
begin
  Result:=(FGlowSize<>1);
end;

// StoreGlowSize
//
function TGLThorFXManager.StoreVibrate : Boolean;
begin
  Result:=(FVibrate<>1);
end;

// SetInnerColor
//
procedure TGLThorFXManager.SetInnerColor(const val : TGLcolor);
begin
  if FInnerColor<>val then begin
    FInnerColor.color:=val.color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TGLThorFXManager.SetOuterColor(const val : TGLcolor);
begin
  if FOuterColor<>val then begin
    FOuterColor.color:=val.color;
    ThorInit;
  end;
end;

// SetOuterColor
//
procedure TGLThorFXManager.SetCoreColor(const val : TGLcolor);
begin
  if FCoreColor<>val then begin
    FCoreColor.color:=val.color;
    ThorInit;
  end;
end;

// Notification
//
procedure TGLThorFXManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FCadencer) then
    Cadencer:=nil;
  inherited;
end;

// DoProgress
//
procedure TGLThorFXManager.DoProgress(const progressTime : TProgressTimes);
var
  I: integer;

begin
  if not FDisabled then
    CalcThor;
  // Invalidate all clients
  //for I := 0 to FClients.Count - 1 do
  //  TGLBThorFX(FClients[I]).OwnerBaseSceneObject.NotifyChange(TGLBThorFX(FClients[i]));
end;

// ThorInit
//
procedure TGLThorFXManager.ThorInit;
begin
  ReallocMem(FThorpoints, FMaxpoints * Sizeof(TThorpoint));
end;

// CalcThor
//
procedure TGLThorFXManager.CalcThor;
var
   N : Integer;
   vec, axs, nvec: TVector;
   dist :single;
   a,b :single;
   len :single;
begin
 // initialise all points with valid data
 for N := 0 to Maxpoints-1 do
   SetVector(FThorpoints[N].Position,0,0,0);

 //------------------Calculate fractal (wildness)---------------
//  SetVector(FThorpoints[0].Position,0,0,0);
  SetVector(FThorpoints[Maxpoints-1].Position,0,0,0);

  CalcFrac(0,maxpoints-1,0,0,0);
  CalcFrac(0,maxpoints-1,0,0,1);
//  CalcFrac(0,maxpoints-1,0,FTarget.z,2);

 //---------------Rotate Vector to target-------------
 SetVector(nvec,FTarget.x,FTarget.y,FTarget.z);
 Len:=VectorLength(nvec);
 NormalizeVector(nvec);
 a:=ArcCos(nvec[2]);
 b:=ArcTan2(nvec[0],nvec[1]);

 N:=0;
 While (N<Maxpoints) do begin
    dist:=N/Maxpoints*len;
    vec:=FThorpoints[N].Position;
    vec[2]:=dist;

    if Assigned(OnCalcPoint) then OnCalcPoint(self,N,Vec[0],vec[1],vec[2]); //Let user mess around with point position

    SetVector(axs,1,0,0);            //Rotate up
    RotateVector(vec,axs,a);
    SetVector(axs,0,0,1);            //Rotate to the sides
    RotateVector(vec,axs,b);
    FThorpoints[N].Position:=vec;
    inc(N);
 end;
 //----------------------------------------------------
 NP:=Maxpoints;
end;

procedure TGLThorFXManager.CalcFrac(left,right:integer; lh,rh :single; xyz:integer);
var midh :single;
    mid :integer;
    res :integer;
    fracScale:single;
begin
 mid:=(left+right) div 2;
 res:=(left+right) mod 2;
 fracScale:=(right-left)/maxpoints;
 midh:= (lh+rh)/2 + (fracScale*FWildness*random)-(fracScale*FWildness)/2     ;
 FThorpoints[mid].Position[xyz]:=midh+(FVibrate*Random-(FVibrate/2));
// if res=1 then FThorpoints[right-1].Position[xyz]:=
//    (FThorpoints[right].Position[xyz]+midh)/(right-mid)*(right-mid-1);
 if res=1 then FThorpoints[right-1].Position[xyz]:=FThorpoints[right].Position[xyz];

 if (mid-left)>1  then CalcFrac(left,mid,lh,midh,xyz);
 if (right-mid)>1 then CalcFrac(mid,right,midh,rh,xyz);
end;

// ------------------
// ------------------ TGLBThorFX ------------------
// ------------------

// Create
//
constructor TGLBThorFX.Create(aOwner : TXCollection);
begin
  inherited Create(aOwner);
  FTarget := TGLCoordinates.CreateInitialized(Self, VectorMake(0, 1, 0));
end;

// Destroy
//
destructor TGLBThorFX.Destroy;
begin
  Manager:=nil;
  FreeAndNil(FTarget);
  inherited Destroy;
end;

// FriendlyName
//
class function TGLBThorFX.FriendlyName : String;
begin
  Result:='ThorFX';
end;

// FriendlyDescription
//
class function TGLBThorFX.FriendlyDescription : String;
begin
  Result:='Thor FX';
end;

// WriteToFiler
//
procedure TGLBThorFX.WriteToFiler(writer : TWriter);
begin
  with writer do begin
    WriteInteger(0); // ArchiveVersion 0
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

// ReadFromFiler
//
procedure TGLBThorFX.ReadFromFiler(reader : TReader);
begin
  with reader do begin
    Assert(ReadInteger=0);
    FManagerName:=ReadString;
    Manager:=nil;
  end;
end;

// Loaded
//
procedure TGLBThorFX.Loaded;
var
  mng : TComponent;

begin
  inherited;
  if FManagerName<>'' then begin
    mng:=FindManager(TGLThorFXManager, FManagerName);
    if Assigned(mng) then
      Manager:=TGLThorFXManager(mng);
    FManagerName:='';
  end;
end;

// Assign
//
procedure TGLBThorFX.Assign(Source: TPersistent);
begin
  if Source is TGLBThorFX then begin
    Manager:=TGLBThorFX(Source).Manager;
  end;
  inherited Assign(Source);
end;

// SetTarget
//
procedure TGLBThorFX.SetTarget(const val: TGLCoordinates);
begin
  FTarget.Assign(val);
end;

// SetManager
//
procedure TGLBThorFX.SetManager(const val : TGLThorFXManager);
begin
  if val<>FManager then begin
    if Assigned(FManager) then
      FManager.DeRegisterClient(Self);
    if Assigned(val) then
      val.RegisterClient(Self);
  end;
end;

// Render
//
procedure TGLBThorFX.Render(sceneBuffer : TGLSceneBuffer;
                            var rci : TRenderContextInfo);
var
   N: Integer;
   I: Integer;
   //absPos :TVector;
   innerColor : TVector;
   lastTr : TAffineVector;
   distList : TSingleList;
   objList : TList;
   fp : PThorpoint;
   mat : TMatrix;

   vx, vy : TVector;
   m : Integer;
   Icol, Ocol, Ccol :TColorVector;
   Ppos, Ppos2 : TAffineVector;
   p: Single;
begin
   if Manager=nil then Exit;

   glPushAttrib(GL_ALL_ATTRIB_BITS);
   glPushMatrix;
   // we get the object position and apply translation...
   //absPos:=OwnerBaseSceneObject.AbsolutePosition;
   // ...should be removed when absolute coords will be handled directly
   // in the point system (and will also make a better flame effect)

   glDisable(GL_CULL_FACE);
   glDisable(GL_TEXTURE_2D);
   glDisable(GL_LIGHTING);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE);
   glEnable(GL_BLEND);
   
   glDepthMask(False);
   
   n := Manager.NP;

   if n>1 then begin
   
     glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
     for m:=0 to 2 do begin
        vx[m]:=mat[m][0]*Manager.GlowSize;
        vy[m]:=mat[m][1]*Manager.GlowSize;
     end;

     SetVector(innerColor, Manager.FInnerColor.Color);

     glEnable(GL_LINE_SMOOTH);
     glDepthFunc(GL_LEQUAL);        //Stops particles at same distanceform overwriting each-other
     glLineWidth(3);
      
     Icol:=Manager.FInnerColor.Color;
     Ocol:=Manager.FOuterColor.color;
     Ccol:=Manager.FCoreColor.color;
     
     //---Core Line---
     if Manager.FCore then begin
     glDisable(GL_BLEND);
     glColor4fv(@Ccol);
     glBegin(GL_LINE_STRIP);
     for i:=0 to n-1 do begin
         fp:=@(Manager.FThorpoints[i]);
         SetVector(Ppos, fp.position);
         glVertex3f(Ppos[0],Ppos[1],Ppos[2]);
     end;
     glEnd;
     end;
     
     //---Point Glow---
     if Manager.FGlow then begin
       glEnable(GL_BLEND);
       for i:=n-1 downto 0 do begin
       fp:=@(Manager.FThorpoints[i]);
       SetVector(Ppos, fp.position);
       
       glVertex3f(Ppos[0],Ppos[1],Ppos[2]);

       glBegin(GL_TRIANGLE_FAN);
            glColor4fv(@Icol);
            glVertex3f(ppos[0],ppos[1],ppos[2]);//middle1
            glColor4fv(@Ocol);
            glVertex3f( vx[0]+vy[0]+ppos[0] , vx[1]+vy[1]+ppos[1] , vx[2]+vy[2]+ppos[2]   );//TopRight
            glVertex3f( vx[0]*1.4  +ppos[0] , vx[1]*1.4  +ppos[1] , vx[2]*1.4  +ppos[2]   );//Right1
            glVertex3f( vx[0]-vy[0]+ppos[0] , vx[1]-vy[1]+ppos[1] , vx[2]-vy[2]+ppos[2]   );//BottomRight
            glVertex3f(-vy[0]*1.4  +ppos[0] ,-vy[1]*1.4  +ppos[1] ,-vy[2]*1.4  +ppos[2]   );//bottom1
            glVertex3f(-vx[0]-vy[0]+ppos[0] ,-vx[1]-vy[1]+ppos[1] ,-vx[2]-vy[2]+ppos[2]   );//BottomLeft
            glVertex3f(-vx[0]*1.4  +ppos[0] ,-vx[1]*1.4  +ppos[1] ,-vx[2]*1.4  +ppos[2]   );//left1
            glVertex3f(-vx[0]+vy[0]+ppos[0] ,-vx[1]+vy[1]+ppos[1] ,-vx[2]+vy[2]+ppos[2]   );//TopLeft
            glVertex3f( vy[0]*1.4  +ppos[0] , vy[1]*1.4  +ppos[1] , vy[2]*1.4  +ppos[2]   );//top1
            glVertex3f( vx[0]+vy[0]+ppos[0] , vx[1]+vy[1]+ppos[1] , vx[2]+vy[2]+ppos[2]   );//TopRight
         glEnd;

       end;//Glow
       end;

   end;
   
   glDepthMask(True);
   
   glPopAttrib;
   glPopMatrix;
end;

// GetOrCreateThorFX
//
function GetOrCreateThorFX(obj : TGLBaseSceneObject; const name : String = '') : TGLBThorFX;
var
	i : Integer;
begin
   with obj.Effects do begin
      if name='' then begin
      	i:=IndexOfClass(TGLBThorFX);
      	if i>=0 then
	      	Result:=TGLBThorFX(Items[i])
      	else Result:=TGLBThorFX.Create(obj.Effects);
      end else begin
         i:=IndexOfName(name);
         if i>=0 then
            Result:=(Items[i] as TGLBThorFX)
         else begin
            Result:=TGLBThorFX.Create(obj.Effects);
            Result.Name:=name;
         end;
      end;
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
	RegisterXCollectionItemClass(TGLBThorFX);

end.

