// GLScriptPython
{: Python implementation for the GLScene scripting layer.<p>

   This unit is experimental.<p>

   <b>History : </b><font size=-1><ul>
      <li>11/11/2004 - SG - Creation
   </ul></font>
}
unit GLScriptPython;

interface

uses
  Classes, SysUtils, XCollection, GLMisc, GLScriptBase, PythonEngine;

type
  // TGLPythonEngine
  //
  {: This class only adds manager registration logic to the TPythonEngine
     class to enable the XCollection items (ie. TGLScriptPython) retain it's
     assigned compiler from design to run -time. }
  TGLPythonEngine = class(TPythonEngine)
    public
      constructor Create(AOnwer : TComponent); override;
      destructor Destroy; override;
  end;

  // GLScriptPython
  //
  {: Implements Python scripting functionality through the
     abstracted GLScriptBase. }
  TGLScriptPython = class(TGLScriptBase)
    private
      { Private Declarations }
      FEngine : TGLPythonEngine;
      FEngineName : String;
      FCompiled,
      FStarted : Boolean;

    protected
      { Protected Declarations }
      procedure SetEngine(const Value : TGLPythonEngine);

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

    published
      { Published Declarations }
      property Engine : TGLPythonEngine read FEngine write SetEngine;

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
  RegisterClasses([TGLPythonEngine, TGLScriptPython]);
  RegisterComponents('GLScene Python', [TGLPythonEngine]);
end;


// ----------
// ---------- TGLPythonEngine ----------
// ----------

// Create
//
constructor TGLPythonEngine.Create(AOnwer : TComponent);
begin
  inherited;
  RegisterManager(Self);
end;

// Destroy
//
destructor TGLPythonEngine.Destroy;
begin
  DeregisterManager(Self);
  inherited;
end;


// ---------------
// --------------- TGLScriptPython ---------------
// ---------------

// Destroy
//
destructor TGLScriptPython.Destroy;
begin
  Invalidate;
  inherited;
end;

// Assign
//
procedure TGLScriptPython.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLScriptPython then begin
    Engine:=TGLScriptPython(Source).Engine;
  end;
end;

// ReadFromFiler
//
procedure TGLScriptPython.ReadFromFiler(reader : TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FEngineName:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TGLScriptPython.WriteToFiler(writer : TWriter);
begin
  inherited;
  writer.WriteInteger(0); // archiveVersion

  with writer do begin
    if Assigned(FEngine) then
      WriteString(FEngine.GetNamePath)
    else
      WriteString('');
  end;
end;

// Loaded
//
procedure TGLScriptPython.Loaded;
var
  temp : TComponent;
begin
  inherited;
  if FEngineName<>'' then begin
    temp:=FindManager(TGLPythonEngine, FEngineName);
    if Assigned(temp) then
      Engine:=TGLPythonEngine(temp);
    FEngineName:='';
  end;
end;

// Notification
//
procedure TGLScriptPython.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = Engine) and (Operation = opRemove) then
    Engine:=nil;
end;

// FriendlyName
//
class function TGLScriptPython.FriendlyName : String;
begin
  Result:='TGLScriptPython';
end;

// FriendlyDescription
//
class function TGLScriptPython.FriendlyDescription : String;
begin
  Result:='Python script';
end;

// ItemCategory
//
class function TGLScriptPython.ItemCategory : String;
begin
  Result:='';
end;

// Compile
//
procedure TGLScriptPython.Compile;
begin
  Invalidate;
  if Assigned(Engine) then begin
    Engine.ExecStrings(Text);
    FCompiled:=True;
    FStarted:=False;
  end else
    raise Exception.Create('No engine assigned!');
end;

// Execute
//
procedure TGLScriptPython.Execute;
begin
  Compile;
end;

// Invalidate
//
procedure TGLScriptPython.Invalidate;
begin
  FStarted:=False;
  FCompiled:=False;
end;

// Start
//
procedure TGLScriptPython.Start;
begin
  Compile;
  FStarted:=True;
end;

// Stop
//
procedure TGLScriptPython.Stop;
begin
  FStarted:=False;
end;

// Call
//
function TGLScriptPython.Call(aName: String;
  aParams: array of Variant) : Variant;
var
  func : PPyObject;
  args : array of TVarRec;
  i : Integer;
begin
  if State = ssUncompiled then
    Start;
  if State = ssRunning then begin
    func:=Engine.FindFunction('__main__', aName);
    if Assigned(func) then
      if Length(aParams)>0 then begin
        SetLength(args, Length(aParams));
        for i:=0 to Length(aParams)-1 do begin
          args[i].VType:=vtVariant;
          args[i].VVariant:=@aParams[i];
        end;
        Result:=Engine.EvalFunction(func, args);
      end else
        Result:=Engine.EvalFunctionNoArgs(func);
  end;
end;

// SetCompiler
//
procedure TGLScriptPython.SetEngine(const Value : TGLPythonEngine);
begin
  if Value<>FEngine then begin
    FEngine:=Value;
    Invalidate;
  end;
end;

// GetState
//
function TGLScriptPython.GetState : TGLScriptState;
begin
  Result:=ssUncompiled;
  if Assigned(Engine) and FCompiled and FStarted then
    Result:=ssRunning;
end;

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
initialization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  RegisterXCollectionItemClass(TGLScriptPython);

// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------
finalization
// --------------------------------------------------
// --------------------------------------------------
// --------------------------------------------------

  UnregisterXCollectionItemClass(TGLScriptPython);

end.