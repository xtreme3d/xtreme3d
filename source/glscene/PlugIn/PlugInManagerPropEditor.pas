unit PlugInManagerPropEditor;

interface

uses
  Forms, Dialogs, StdCtrls, Controls, Buttons, Classes, PlugInManager;

type
  TPlugInManagerPropForm = class(TForm)
    OpenDialog: TOpenDialog;
    ListBox: TListBox;
    Label1: TLabel;
    OKButton: TSpeedButton;
    LoadButton: TSpeedButton;
    UnloadButton: TSpeedButton;
    GroupBox: TGroupBox;
    DescriptionMemo: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    DateLabel: TLabel;
    SizeLabel: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ServiceBox: TComboBox;
    NameBox: TComboBox;
    procedure OKButtonClick(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure UnloadButtonClick(Sender: TObject);
    procedure ServiceBoxChange(Sender: TObject);
  private
    FManager : TPlugInManager;
  public
    class procedure EditPlugIns(AManager: TPlugInManager);
  end;

var PlugInManagerPropForm: TPlugInManagerPropForm;

//------------------------------------------------------------------------------

implementation

uses PlugInIntf, SysUtils;

{$R *.DFM}

//------------------------------------------------------------------------------

procedure TPlugInManagerPropForm.OKButtonClick(Sender: TObject);

begin
  Close;
end;

//------------------------------------------------------------------------------

procedure TPlugInManagerPropForm.LoadButtonClick(Sender: TObject);

var I, Index : Integer;

begin
  with OpenDialog do
   if Execute then
     for I:=0 to Files.Count-1 do
     begin
       Index:=FManager.AddPlugIn(Files[I]);
       if Index > -1 then
         if Index >= ListBox.Items.Count then
         begin
           FManager.PlugIns.Objects[Index];
           ListBox.Items.Add(FManager.PlugIns.Strings[I]);
         end else
       else MessageDlg(Format('Error while loading %s'+#13+'not a valid GLScene plug-in',[Files[I]]),
                       mtError,[mbOK],0);
     end;
end;

//------------------------------------------------------------------------------

class procedure TPlugInManagerPropForm.EditPlugIns(AManager: TPlugInManager);

begin
  // ensure only one instance
  if assigned(PlugInManagerPropForm) then PlugInManagerPropForm.Free;
  PlugInManagerPropForm:=TPlugInManagerPropForm.Create(Application);
  with PlugInManagerPropForm do
  begin
    ListBox.Items:=AManager.PlugIns;
    FManager:=AManager;
    ShowModal;
    Free;
  end;
  PlugInManagerPropForm:=nil;
end;

//------------------------------------------------------------------------------

procedure TPlugInManagerPropForm.ListBoxClick(Sender: TObject);

var Entry    : Integer;
    Service  : TPIServiceType;
    Services : TPIServices;

begin
  Entry:=ListBox.ItemIndex;
  if Entry > -1 then
  begin
    SizeLabel.Caption:=Format('%n KB',[FManager.PlugIns[Entry].FileSize/1000]);
    SizeLabel.Enabled:=True;
    DateLabel.Caption:=DateToStr(FManager.PlugIns[Entry].FileDate);
    DateLabel.Enabled:=True;
    DescriptionMemo.SetTextBuf(FManager.PlugIns[Entry].GetDescription);
    ServiceBox.Items.Clear;
    ServiceBox.Enabled:=True;
    Services:=FManager.PlugIns[Entry].GetServices;
    for Service:=Low(TPIServiceType) to High(TPIServiceType) do
      if Service in Services then
      case Service of
        stRaw     : begin
                      Entry:=ServiceBox.Items.Add('Raw');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stRaw);
                    end;
        stObject  : begin
                      Entry:=ServiceBox.Items.Add('Object');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stObject);
                    end;
        stBitmap  : begin
                      Entry:=ServiceBox.Items.Add('Bitmap');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stBitmap);
                    end;
        stTexture : begin
                      Entry:=ServiceBox.Items.Add('Texture');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stTexture);
                    end;
        stImport  : begin
                      Entry:=ServiceBox.Items.Add('Import');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stImport);
                    end;
        stExport  : begin
                      Entry:=ServiceBox.Items.Add('Export');
                      ServiceBox.Items.Objects[Entry]:=Pointer(stExport);
                    end;  
      end;
    ServiceBox.ItemIndex:=0;
    ServiceBox.OnChange(ServiceBox);
  end;
end;

//------------------------------------------------------------------------------

procedure TPlugInManagerPropForm.UnloadButtonClick(Sender: TObject);

var I : Integer;

begin
  for I:=0 to ListBox.Items.Count-1 do
    if ListBox.Selected[I] then
    begin
      FManager.RemovePlugIn(I);
      ListBox.Items.Delete(I);
    end;
  DescriptionMemo.Clear;
  DateLabel.Caption:='???'; DateLabel.Enabled:=False;
  SizeLabel.Caption:='???'; SizeLabel.Enabled:=False;
  ServiceBox.ItemIndex:=-1; ServiceBox.Enabled:=False;
  NameBox.ItemIndex:=-1;    NameBox.Enabled:=False;
end;

//------------------------------------------------------------------------------

procedure NameCallback(Name: PChar); stdcall;

begin
  PlugInManagerPropForm.NameBox.Items.Add(StrPas(Name));
end;

//------------------------------------------------------------------------------

procedure TPlugInManagerPropForm.ServiceBoxChange(Sender: TObject);

begin
  NameBox.Items.Clear;
  with ServiceBox, Items do
    FManager.PlugIns[ListBox.ItemIndex].EnumResourceNames(TPIServiceType(Objects[ItemIndex]),NameCallback);
  NameBox.ItemIndex:=0;
  NameBox.Enabled:=True;
end;

//------------------------------------------------------------------------------

end.

