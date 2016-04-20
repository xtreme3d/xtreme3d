// GLScriptDWS2
{: DelphiWebScriptII implementation for the GLScene scripting layer.<p>

   <b>History : </b><font size=-1><ul>
      <li>04/11/2004 - SG - Creation
   </ul></font>
}
unit GLScriptDWS2;

interface

uses
  Classes, SysUtils, XCollection, GLMisc, GLScriptBase, dws2Comp, dws2Exprs,
  dws2Symbols;

type
  // TGLDelphiWebScriptII
  //
  {: This class only adds manager registration logic to the TDelphiWebScriptII
     class to enable the XCollection items (ie. TGLScriptDWS2) retain it's
     assigned compiler from design to run -time. }
  TGLDelphiWebScriptII = class(TDelphiWebScriptII)
    public
      constructor Create(AOnwer : TComponent); override;
      destructor Destroy; override;
  end;

  // GLScriptDWS2
  //
  {: Implements DelphiWebScriptII scripting functionality through the
     abstracted GLScriptBase . }
  TGLScriptDWS2 = class(TGLScriptBase)
    private
      { Private Declarations }
      FDWS2Program : TProgram;
      FCompiler : TGLDelphiWebScriptII;
      FCompilerName : String;

    protected
      { Protected Declarations }
      procedure SetCompiler(const Value : TGLDelphiWebScriptII);

      procedure ReadFromFiler(reader : TReader); override;
      procedure WriteToFiler(writer : TWriter); override;
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      function GetState : TGLScriptState; override;

    public
      { Public Declarations }
      destructor Destroy; override;

      procedure Assign(Source: TPersistent); override;

      procedure Compile; override;
      procedure Start; override;
      procedure Stop; override;
      procedure Execute; override;
      procedure Invalidate; override;
      function Call(aName : String;
        aParams : array of Variant) : Variant; override;

      class function FriendlyName : String; override;
      class function FriendlyDescription : String; override;
      class function ItemCategory : String; override;

      property DWS2Program : TProgram read FDWS2Program;

    published
      { Published Declarations }
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

// ---------------
// --------------- Miscellaneous ---------------
// ---------------

// Register
//
procedure Register;
begin
  RegisterClasses([TGLDelphiWebScriptII, TGLScriptDWS2]);
  RegisterComponents('GLScene DWS2', [TGLDelphiWebScriptII]);
end;


// ----------
// ---------- TGLDelphiWebScriptII ----------
// ----------

// Create
//
constructor TGLDelphiWebScriptII.Create(AOnwer : TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

// Destroy
//
destructor TGLDelphiWebScriptII.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TGLScriptDWS2 ---------------
// ---------------

// Destroy
//
destructor TGLScriptDWS2.Destroy;
begin
  Invalidate;
  inherited;
end;

// Assign
//
procedure TGLScriptDWS2.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLScriptDWS2 then begin
    Compiler:=TGLScriptDWS2(Source).Compiler;
  end;
end;

// ReadFromFiler
//
procedure TGLScriptDWS2.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FCompilerName:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TGLScriptDWS2.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do begin
    if Assigned(FCompiler) then
      WriteString(FCompiler.GetNamePath)
    else
      WriteString('');
  end;
end;

// Loaded
//
procedure TGLScriptDWS2.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FCompilerName<>'' then begin
    temp:=FindManager(TGLDelphiWebScriptII, FCompilerName);
    if Assigned(temp) then
      Compiler:=TGLDelphiWebScriptII(temp);
    FCompilerName:='';
  end;
end;

// Notification
//
procedure TGLScriptDWS2.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Compiler) and (Operation = opRemove) then
    Compiler:=nil;
end;

// FriendlyName
//
class function TGLScriptDWS2.FriendlyName : String;
begin
  Result:='GLScriptDWS2';
end;

// FriendlyDescription
//
class function TGLScriptDWS2.FriendlyDescription : String;
begin
  Result:='DelphiWebScriptII script';
end;

// ItemCategory
//
class function TGLScriptDWS2.ItemCategory : String;
begin
  Result:='';
end;

// Compile
//
procedure TGLScriptDWS2.Compile;
begin
  Invalidate;
  if Assigned(Compiler) then
    FDWS2Program:=Compiler.Compile(Text.Text)
  else
    raise Exception.Create('No compiler assigned!');
end;

// Execute
//
procedure TGLScriptDWS2.Execute;
begin
  if (State = ssUncompiled) then
    Compile
  else if (State = ssRunning) then
    Stop;
  if (State = ssCompiled) then
    FDWS2Program.Execute;
end;

// Invalidate
//
procedure TGLScriptDWS2.Invalidate;
begin
  if (State <> ssUncompiled) or Assigned(FDWS2Program) then begin
    Stop;
    FreeAndNil(FDWS2Program);
  end;
end;

// Start
//
procedure TGLScriptDWS2.Start;
begin
  if (State = ssUncompiled) then
    Compile;
  if (State = ssCompiled) then
    FDWS2Program.BeginProgram(False);
end;

// Stop
//
procedure TGLScriptDWS2.Stop;
begin
  if (State = ssRunning) then
    FDWS2Program.EndProgram;
end;

// Call
//
function TGLScriptDWS2.Call(aName: String;
  aParams: array of Variant) : Variant;
var
  Symbol : TSymbol;
  Output : IInfo;
begin
  if (State <> ssRunning) then
    Start;
  if State = ssRunning then begin
    Symbol:=FDWS2Program.Table.FindSymbol(aName);
    if Assigned(Symbol) then begin
      if Symbol is TFuncSymbol then begin
        Output:=FDWS2Program.Info.Func[aName].Call(aParams);
        if Assigned(Output) then
          Result:=Output.Value;
      end else
        raise Exception.Create('Expected TFuncSymbol but found '+Symbol.ClassName+' for '+aName);
    end else
      raise Exception.Create('Symbol not found for '+aName);
  end;
end;

// SetCompiler
//
procedure TGLScriptDWS2.SetCompiler(const Value : TGLDelphiWebScriptII);
begin
  if Value<>FCompiler then begin
    FCompiler:=Value;
    Invalidate;
  end;
end;

// GetState
//
function TGLScriptDWS2.GetState : TGLScriptState;
begin
  Result:=ssUncompiled;
  if Assigned(FDWS2Program) then begin
    case FDWS2Program.ProgramState of
      psReadyToRun : Result:=ssCompiled;
      psRunning : Result:=ssRunning;
    else
      if FDWS2Program.Msgs.HasErrors then begin
        if FDWS2Program.Msgs.HasCompilerErrors then
          Result:=ssCompileErrors
        else if FDWS2Program.Msgs.HasExecutionErrors then
          Result:=ssRunningErrors;
        Errors.Text:=FDWS2Program.Msgs.AsInfo;
      end;
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

  RegisterXCollectionItemClass(TGLScriptDWS2);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TGLScriptDWS2);

end.