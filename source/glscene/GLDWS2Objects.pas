// GLDWS2Objects
{: Base classes and logic for DelphiWebScriptII enabled
   objects in GLScene<p>

   <b>History : </b><font size=-1><ul>
      <li>04/11/2004 - SG - Moved TGLDelphiWebScriptII to GLScriptDWS2 unit. 
      <li>06/04/2004 - SG - Creation
   </ul></font>
}
unit GLDWS2Objects;

interface

uses
  Classes, SysUtils, dws2Comp, dws2Exprs, dws2Symbols,
  GLScene, GLMisc, XCollection, GLScriptDWS2;

type
  // TGLDWS2ActiveBehaviour
  //
  { A DelphiWebScriptII enabled behaviour. This behaviour also calls
    on the OnProgress and OnBeginProgram procedures in the script if
    they are found. Once compiled and executed the program remains
    active until killed, deactivated or the script is invalidated. }
  TGLDWS2ActiveBehaviour = class (TGLBehaviour)
    private
      FActive : Boolean;
      FScript : TStringList;
      FDWS2Program : TProgram;
      FCompiler : TGLDelphiWebScriptII;
      FCompilerName : String;

      procedure SetActive(const Value : Boolean);
      procedure SetScript(const Value : TStringList);
      procedure SetCompiler(const Value : TGLDelphiWebScriptII);

      procedure CompileProgram;
      procedure BeginProgram;
      procedure EndProgram;
      procedure KillProgram;

    protected
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      procedure Loaded; override;

    public
      constructor Create(AOwner : TXCollection); override;
      destructor Destroy; override;
      class function FriendlyName : String; override;
      procedure DoProgress(const ProgressTimes : TProgressTimes); override;
      procedure InvalidateScript;

      property DWS2Program : TProgram read FDWS2Program;

    published
      property Active : Boolean read FActive write SetActive;
      property Script : TStringList read FScript write SetScript;
      property Compiler : TGLDelphiWebScriptII read FCompiler write SetCompiler;
  end;

procedure Register;

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
implementation
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

// ----------
// ---------- Miscellaneous ----------
// ----------

procedure Register;
begin
  RegisterClasses([TGLDWS2ActiveBehaviour]);
end;


// ----------
// ---------- TGLDWS2ActiveBehaviour ----------
// ----------

// Create
//
constructor TGLDWS2ActiveBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FScript:=TStringList.Create;
end;

// Destroy
//
destructor TGLDWS2ActiveBehaviour.Destroy;
begin
  KillProgram;
  FScript.Free;
  inherited;
end;

// FriendlyName
//
class function TGLDWS2ActiveBehaviour.FriendlyName: String;
begin
  Result:='DWS2 Active Script';
end;

// DoProgress
//
procedure TGLDWS2ActiveBehaviour.DoProgress(const ProgressTimes: TProgressTimes);
var
  Symbol : TSymbol;
begin
  inherited;
  if Assigned(FDWS2Program) then begin
    if FDWS2Program.ProgramState = psRunning then begin
      Symbol:=DWS2Program.Table.FindSymbol('OnProgress');
      if Assigned(Symbol) then
        if Symbol is TFuncSymbol then
          DWS2Program.Info.Func['OnProgress'].Call([ProgressTimes.newTime, ProgressTimes.deltaTime]);
    end;
  end;
end;

// Loaded
//
procedure TGLDWS2ActiveBehaviour.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TGLDelphiWebScriptII, FCompilerName);
    if Assigned(temp) then
      Compiler:=TGLDelphiWebScriptII(temp);
    FCompilerName:='';
    CompileProgram;
    if Active then BeginProgram;
  end;
end;

// ReadFromFiler
//
procedure TGLDWS2ActiveBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do begin
    Assert(ReadInteger = 0); // Archive version
    Active:=ReadBoolean;
    FCompilerName:=ReadString;
    Script.Text:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TGLDWS2ActiveBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do begin
    WriteInteger(0); // Archive version
    WriteBoolean(FActive);
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else WriteString('');
    WriteString(Script.Text);
  end;
end;

// CompileProgram
//
procedure TGLDWS2ActiveBehaviour.CompileProgram;
begin
  if Assigned(Compiler) then begin
    KillProgram;
    FDWS2Program:=Compiler.Compile(Script.Text);
    if Active then
      BeginProgram;
  end;
end;

// BeginProgram
//
procedure TGLDWS2ActiveBehaviour.BeginProgram;
var
  Symbol : TSymbol;
  ObjectID : Variant;
  Obj : TGLBaseSceneObject;
begin
  if Assigned(DWS2Program) then begin
    if DWS2Program.ProgramState = psReadyToRun then begin
      DWS2Program.BeginProgram;
      if FDWS2Program.ProgramState = psRunning then begin
        Symbol:=DWS2Program.Table.FindSymbol('OnBeginProgram');
        if Assigned(Symbol) then
          if Symbol is TFuncSymbol then begin
            Obj:=OwnerBaseSceneObject;
            if Assigned(Obj) then begin
              ObjectID:=DWS2Program.Info.RegisterExternalObject(Obj, False, False);
              DWS2Program.Info.Func['OnBeginProgram'].Call([ObjectID]);
            end;
          end;
      end;
    end;
  end;
end;

// EndProgram
//
procedure TGLDWS2ActiveBehaviour.EndProgram;
begin
  if Assigned(DWS2Program) then begin
    if DWS2Program.ProgramState = psRunning then
      DWS2Program.EndProgram;
  end;
end;

// KillProgram
//
procedure TGLDWS2ActiveBehaviour.KillProgram;
begin
  if Assigned(DWS2Program) then begin
    EndProgram;
    FreeAndNil(FDWS2Program);
  end;
end;

// InvalidateScript
//
procedure TGLDWS2ActiveBehaviour.InvalidateScript;
begin
  KillProgram;
  CompileProgram;
end;

// SetActive
//
procedure TGLDWS2ActiveBehaviour.SetActive(const Value: Boolean);
begin
  if Value<>FActive then begin
    EndProgram;
    FActive:=Value;
    if Active then
      BeginProgram;
  end;
end;

// SetScript
//
procedure TGLDWS2ActiveBehaviour.SetScript(const Value: TStringList);
begin
  if Assigned(Value) then begin
    KillProgram;
    FScript.Assign(Value);
    if Assigned(Compiler) then begin
      CompileProgram;
      if Active then BeginProgram;
    end;
  end;
end;

// SetCompiler
//
procedure TGLDWS2ActiveBehaviour.SetCompiler(const Value: TGLDelphiWebScriptII);
begin
  if Value<>FCompiler then begin
    if Assigned(FCompiler) then
      KillProgram;
    FCompiler:=Value;
    if Assigned(FCompiler) then begin
      RegisterManager(FCompiler);
      CompileProgram;
      if Active then BeginProgram;
    end;
  end;
end;


// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
initialization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  RegisterXCollectionItemClass(TGLDWS2ActiveBehaviour);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TGLDWS2ActiveBehaviour);

end.