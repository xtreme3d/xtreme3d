{: GLTexCombineShader<p>

   A shader that allows texture combiner setup.<p>

   <b>History : </b><font size=-1><ul>
      <li>23/05/03 - EG - Added support for binding two extra texture units
      <li>16/05/03 - EG - Creation
   </ul></font>
}
unit GLTexCombineShader;

interface

uses Classes, GLTexture;

type

   // TGLTexCombineShader
   //
   {: A shader that can setup the texture combiner.<p> }
   TGLTexCombineShader = class (TGLShader)
	   private
	      { Protected Declarations }
         FCombiners : TStrings;
         FCombinerIsValid : Boolean; // to avoid reparsing invalid stuff
         FDesignTimeEnabled : Boolean;

         FMaterialLibrary : TGLMaterialLibrary;
         FLibMaterial3Name : TGLLibMaterialName;
         currentLibMaterial3 : TGLLibMaterial;
         FLibMaterial4Name : TGLLibMaterialName;
         currentLibMaterial4 : TGLLibMaterial;

         FApplied3, FApplied4 : Boolean;

	   protected
			{ Protected Declarations }
         procedure SetCombiners(const val : TStrings);
         procedure SetDesignTimeEnabled(const val : Boolean);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetLibMaterial3Name(const val : TGLLibMaterialName);
         procedure SetLibMaterial4Name(const val : TGLLibMaterialName);

         procedure NotifyLibMaterial3Destruction;
         procedure NotifyLibMaterial4Destruction;

         procedure DoInitialize; override;
         procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
         function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
         procedure DoFinalize; override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure NotifyChange(Sender : TObject); override;

      published
	      { Published Declarations }
         property Combiners : TStrings read FCombiners write SetCombiners;
         property DesignTimeEnabled : Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;

			property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
			property LibMaterial3Name : TGLLibMaterialName read FLibMaterial3Name write SetLibMaterial3Name;
			property LibMaterial4Name : TGLLibMaterialName read FLibMaterial4Name write SetLibMaterial4Name;
   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLTextureCombiners, OpenGL1x, XOpenGL, GLMisc, GLCrossPlatform;

procedure Register;
begin
	RegisterComponents('GLScene Shaders', [TGLTexCombineShader]);
end;

// ------------------
// ------------------ TGLTexCombineShader ------------------
// ------------------

// Create
//
constructor TGLTexCombineShader.Create(AOwner : TComponent);
begin
	inherited;
   ShaderStyle:=ssLowLevel;
   FCombiners:=TStringList.Create;
   TStringList(FCombiners).OnChange:=NotifyChange;
   FCombinerIsValid:=True;
end;

// Destroy
//
destructor TGLTexCombineShader.Destroy;
begin
   if Assigned(currentLibMaterial3) then
      currentLibMaterial3.UnregisterUser(Self);
   if Assigned(currentLibMaterial4) then
      currentLibMaterial4.UnregisterUser(Self);
	inherited;
   FCombiners.Free;
end;

// Notification
//
procedure TGLTexCombineShader.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (FMaterialLibrary=AComponent) and (Operation=opRemove) then begin
      NotifyLibMaterial3Destruction;
      NotifyLibMaterial4Destruction;
      FMaterialLibrary:=nil;
   end;
   inherited;
end;

// NotifyChange
//
procedure TGLTexCombineShader.NotifyChange(Sender : TObject);
begin
   FCombinerIsValid:=True;
   inherited NotifyChange(Sender);
end;

// NotifyLibMaterial3Destruction
//
procedure TGLTexCombineShader.NotifyLibMaterial3Destruction;
begin
   FLibMaterial3Name:='';
   currentLibMaterial3:=nil;
end;

// NotifyLibMaterial4Destruction
//
procedure TGLTexCombineShader.NotifyLibMaterial4Destruction;
begin
   FLibMaterial4Name:='';
   currentLibMaterial4:=nil;
end;

// SetMaterialLibrary
//
procedure TGLTexCombineShader.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   FMaterialLibrary:=val;
   SetLibMaterial3Name(LibMaterial3Name);
   SetLibMaterial4Name(LibMaterial4Name);
end;

// SetLibMaterial3Name
//
procedure TGLTexCombineShader.SetLibMaterial3Name(const val : TGLLibMaterialName);
var
   newLibMaterial : TGLLibMaterial;
begin
   // locate new libmaterial
   if Assigned(FMaterialLibrary) then
      newLibMaterial:=MaterialLibrary.Materials.GetLibMaterialByName(val)
   else newLibMaterial:=nil;
   FLibMaterial3Name:=val;
   // unregister if required
   if newLibMaterial<>currentLibMaterial3 then begin
      // unregister from old
      if Assigned(currentLibMaterial3) then
         currentLibMaterial3.UnregisterUser(Self);
      currentLibMaterial3:=newLibMaterial;
      // register with new
      if Assigned(currentLibMaterial3) then
         currentLibMaterial3.RegisterUser(Self);
      NotifyChange(Self);
   end;
end;

// SetLibMaterial4Name
//
procedure TGLTexCombineShader.SetLibMaterial4Name(const val : TGLLibMaterialName);
var
   newLibMaterial : TGLLibMaterial;
begin
   // locate new libmaterial
   if Assigned(FMaterialLibrary) then
      newLibMaterial:=MaterialLibrary.Materials.GetLibMaterialByName(val)
   else newLibMaterial:=nil;
   FLibMaterial4Name:=val;
   // unregister if required
   if newLibMaterial<>currentLibMaterial4 then begin
      // unregister from old
      if Assigned(currentLibMaterial4) then
         currentLibMaterial4.UnregisterUser(Self);
      currentLibMaterial4:=newLibMaterial;
      // register with new
      if Assigned(currentLibMaterial4) then
         currentLibMaterial4.RegisterUser(Self);
      NotifyChange(Self);
   end;
end;

// DoInitialize
//
procedure TGLTexCombineShader.DoInitialize;
begin
end;

// DoApply
//
procedure TGLTexCombineShader.DoApply(var rci : TRenderContextInfo; Sender : TObject);
var
   n, units : Integer;
begin
   if not GL_ARB_multitexture then Exit;
   FApplied3:=False;
   FApplied4:=False;
   if FCombinerIsValid and (FDesignTimeEnabled or (not (csDesigning in ComponentState)))  then  begin
      try
         if Assigned(currentLibMaterial3) or Assigned(currentLibMaterial4) then begin
            glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @n);
            units:=0;
            if Assigned(currentLibMaterial3) and (n>=3) then begin
               with currentLibMaterial3.Material.Texture do begin
                  if Enabled then begin
                     ApplyAsTextureN(3, rci, currentLibMaterial3);
                     Inc(units, 4);
                     FApplied3:=True;
                  end;
               end;
            end;
            if Assigned(currentLibMaterial4) and (n>=4) then begin
               with currentLibMaterial4.Material.Texture do begin
                  if Enabled then begin
                     ApplyAsTextureN(4, rci, currentLibMaterial4);
                     Inc(units, 8);
                     FApplied4:=True;
                  end;
               end;
            end;
            if units>0 then
               xglMapTexCoordToArbitraryAdd(units);
         end;
         SetupTextureCombiners(FCombiners.Text);
      except
         on E: Exception do begin
            FCombinerIsValid:=False;
            InformationDlg(E.ClassName+': '+E.Message);
         end;
      end;
   end;
end;

// DoUnApply
//
function TGLTexCombineShader.DoUnApply(var rci : TRenderContextInfo) : Boolean;
begin
   if FApplied3 then with currentLibMaterial3.Material.Texture do
      UnApplyAsTextureN(3, rci, currentLibMaterial3);
   if FApplied4 then with currentLibMaterial4.Material.Texture do
      UnApplyAsTextureN(4, rci, currentLibMaterial4);
   Result:=False;
end;

// DoFinalize
//
procedure TGLTexCombineShader.DoFinalize;
begin
end;

// SetCombiners
//
procedure TGLTexCombineShader.SetCombiners(const val : TStrings);
begin
   if val<>FCombiners then begin
      FCombiners.Assign(val);
      NotifyChange(Self);
   end;
end;

// SetDesignTimeEnabled
//
procedure TGLTexCombineShader.SetDesignTimeEnabled(const val : Boolean);
begin
   if val<>FDesignTimeEnabled then begin
      FDesignTimeEnabled:=val;
      NotifyChange(Self);
   end;
end;

end.
