// GLScriptBase
{: An abstract scripting interface for GLScene<p>

   This unit provides the base methods for compiling and executing scripts as
   well as calling scripted functions. No scripting APIs are implemented here,
   only abstracted functions.<p>

   <b>History : </b><font size=-1><ul>
      <li>04/11/2004 - SG - Creation
   </ul></font>
}
unit GLScriptBase;

interface

uses
  Classes, XCollection;

type
  TGLScriptState = ( ssUncompiled,    // The script has yet to be compiled.
                     ssCompileErrors, // Errors occurred while compiling.
                     ssCompiled,      // The script has been compiled and is
                                      // ready to be executed/started.
                     ssRunningErrors, // Errors occured while the script was
                                      // running.
                     ssRunning );     // The script is currently active and
                                      // is running without error.

  // TGLScriptBase
  //
  {: The base script class that defines the abstract functions and properties. 
     Don't use this class directly, use the script classes descended from this 
     base class.  }
  TGLScriptBase = class(TXCollectionItem)
		private
      { Private Declarations }
      FText : TStringList;
      FDescription : String;
      FErrors : TStringList; // not persistent

		protected
			{ Protected Declarations }
      procedure WriteToFiler(writer : TWriter); override;
      procedure ReadFromFiler(reader : TReader); override;
      function GetState : TGLScriptState; virtual; abstract;
      procedure SetText(const Value : TStringList);
      procedure Notification(AComponent: TComponent; Operation: TOperation); virtual;

		public
      { Public Declarations }
      constructor Create(aOwner : TXCollection); override;
      destructor Destroy; override;

      procedure Assign(Source: TPersistent); override;

      procedure Compile; virtual; abstract;
      procedure Start; virtual; abstract;
      procedure Stop; virtual; abstract;
      procedure Execute; virtual; abstract;
      procedure Invalidate; virtual; abstract;
      function Call(aName : String;
        aParams : array of Variant) : Variant; virtual; abstract;
      

      property Errors : TStringList read FErrors;
      property State : TGLScriptState read GetState;

		published
      { Published Declarations }
      property Text : TStringList read FText write SetText;
      property Description : String read FDescription write FDescription;

  end;

  // TGLScripts
  //
  {: XCollection descendant for storing and handling scripts. }
  TGLScripts = class(TXCollection)
		private
			{ Private Declarations }

		protected
			{ Protected Declarations }
      function GetItems(index : Integer) : TGLScriptBase;

		public
			{ Public Declarations }
			procedure Assign(Source: TPersistent); override;

      class function ItemsClass : TXCollectionItemClass; override;

      function CanAdd(aClass : TXCollectionItemClass) : Boolean; override;
      property Items[index : Integer] : TGLScriptBase read GetItems; default;

  end;

  // TGLScriptLibrary
  //
  {: Encapsulation of the scripts XCollection to help with script handling at
     design-time. Links the scripts to Delphi's persistence model. }
  TGLScriptLibrary = class (TComponent)
    private
      { Private Declarations }
      FScripts : TGLScripts;

    protected
      { Protected Declarations }
      procedure DefineProperties(Filer : TFiler); override;
      procedure WriteScriptsData(Stream : TStream);
      procedure ReadScriptsData(Stream : TStream);
      procedure Loaded; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
      { Public Declarations }
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

    published
      { Published Declarations }
      property Scripts : TGLScripts read FScripts;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterClasses([TGLScriptLibrary, TGLScripts, TGLScriptBase]);
end;

// ---------------
// --------------- TGLScriptBase ---------------
// ---------------

// Create
//
constructor TGLScriptBase.Create(aOwner: TXCollection);
begin
  inherited;
  FText:=TStringList.Create;
  FErrors:=TStringList.Create;
end;

// Destroy
//
destructor TGLScriptBase.Destroy;
begin
  FText.Free;
  FErrors.Free;
  inherited;
end;

// Assign
//
procedure TGLScriptBase.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TGLScriptBase then begin
    Text.Assign(TGLScriptBase(Source).Text);
    Description:=TGLScriptBase(Source).Description;
  end;
end;

// ReadFromFiler
//
procedure TGLScriptBase.ReadFromFiler(reader: TReader);
var
  archiveVersion : Integer;
begin
  inherited;
  archiveVersion:=reader.ReadInteger;
  Assert(archiveVersion = 0);

  with reader do begin
    FText.Text:=ReadString;
    FDescription:=ReadString;
  end;
end;

// WriteToFiler
//
procedure TGLScriptBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  writer.WriteInteger(0);

  with writer do begin
    WriteString(FText.Text);
    WriteString(FDescription);
  end;
end;

// SetText
//
procedure TGLScriptBase.SetText(const Value : TStringList);
begin
  Text.Assign(Value);
end;

// Notification
//
procedure TGLScriptBase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Virtual
end;

// ---------------
// --------------- TGLScripts ---------------
// ---------------

// Assign
//
procedure TGLScripts.Assign(Source: TPersistent);
begin
  inherited;
  // Nothing yet
end;

// GetItems
//
function TGLScripts.GetItems(index: Integer): TGLScriptBase;
begin
  Result:=TGLScriptBase(inherited GetItems(index));
end;

// ItemsClass
//
class function TGLScripts.ItemsClass: TXCollectionItemClass;
begin
  Result:=TGLScriptBase;
end;

// CanAdd
//
function TGLScripts.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result:=aClass.InheritsFrom(TGLScriptBase);
end;


// ---------------
// --------------- TGLScriptLibrary ---------------
// ---------------

// Create
//
constructor TGLScriptLibrary.Create(AOwner : TComponent);
begin
  inherited;
  FScripts:=TGLScripts.Create(Self);
end;

// Destroy
//
destructor TGLScriptLibrary.Destroy;
begin
  FScripts.Free;
  inherited;
end;

// DefineProperties
//
procedure TGLScriptLibrary.DefineProperties(Filer : TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ScriptsData',
    ReadScriptsData, WriteScriptsData, (Scripts.Count>0));
end;

// WriteScriptsData
//
procedure TGLScriptLibrary.WriteScriptsData(Stream : TStream);
var
  writer : TWriter;
begin
  writer:=TWriter.Create(stream, 16384);
  try
    Scripts.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

// ReadScriptsData
//
procedure TGLScriptLibrary.ReadScriptsData(Stream : TStream);
var
  reader : TReader;
begin
  reader:=TReader.Create(stream, 16384);
  try
    Scripts.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

// Loaded
//
procedure TGLScriptLibrary.Loaded;
begin
  inherited;
  Scripts.Loaded;
end;

// Notification
//
procedure TGLScriptLibrary.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : Integer;
begin
  if Assigned(Scripts) then
    for i:=0 to Scripts.Count-1 do
      Scripts[i].Notification(AComponent, Operation);
  inherited;
end;

end.
