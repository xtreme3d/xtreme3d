//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGui<p>

  In GL windows management classes and structures<p>

	<b>History : </b><font size=-1><ul>
      <li>16/12/05 - DK - Removed GuiSkinEditorFormUnit dependancy
      <li>30/11/04 - DB - Fixed memory leaks (thanks dikoe Kenguru)
      <li>16/07/03 - EG - TGLBaseGuiObject moved in along with RecursiveVisible mechanism
      <li>25/11/02 - EG - TGLGuiLayout.Clear fix (Sternas Stefanos)
      <li>06/09/02 - JAJ - Updated and added to CVS..
      <li>01/06/02 - JAJ - Base Unit built..
	</ul></font>
}
unit GLGui;

interface

uses
  Classes, GLScene, GLMisc, GLBitmapFont, GLTexture, GLCrossPlatform,
  OpenGL1x, SysUtils, PersistentClasses;

Type

   TGLBaseGuiObject = class (TGLBaseSceneObject)
      private
         FRecursiveVisible : Boolean;
         FWidth : Single;
         FHeight : Single;

      protected
         //: self notification on hide. Also notifies children.
         procedure NotifyHide; dynamic;
         //: child notification on show. Also notifies children.
         procedure NotifyShow; dynamic;

         procedure SetLeft(const val : Single);
         Function  GetLeft : Single;
         procedure SetTop(const val : Single);
         Function  GetTop : Single;
         procedure SetWidth(const val : Single);
         procedure SetHeight(const val : Single);
         procedure SetVisible(aValue : Boolean); override;

      public
         constructor Create(AOwner: TComponent); override;

         procedure AddChild(AChild: TGLBaseSceneObject); override;
         procedure Insert(aIndex : Integer; aChild : TGLBaseSceneObject); override;

         {: GuiComponent Width in 3D world units. }
         property Width : Single read FWidth write SetWidth;
         {: GuiComponent Height in 3D world units. }
         property Height : Single read FHeight write SetHeight;
         {: GuiComponent Left in 3D world units. }
         property Left : Single read GetLeft write SetLeft;
         {: GuiComponent Top in 3D world units. }
         property Top : Single read GetTop write SetTop;

         property RecursiveVisible : Boolean read FRecursiveVisible;
   end;

  TGUIAlignments = (GLAlTopLeft, GLAlTop, GLAlTopRight, GLAlLeft,GLAlCenter, GLAlRight, GLAlBottomLeft,GLAlBottom, GLAlBottomRight, GLAlBorder);
  TGUIRect       = Record
    X1 : TGLFloat;
    Y1 : TGLFloat;
    X2 : TGLFloat;
    Y2 : TGLFloat;
    XTiles  : TGLFloat;
    YTiles  : TGLFloat;
  End;
  TGUIDrawResult  = Array [TGUIAlignments] of TGUIRect;

  TGLGuiElementName = String;
  TGLGuiElement    = class(TCollectionItem)
  private
    FTopLeft      : TGLCoordinates;
    FBottomRight  : TGLCoordinates;
    FScale        : TGLCoordinates;
    FAlign        : TGUIAlignments;
    FName         : TGLGuiElementName;
  protected
    function GetDisplayName : String; override;
    procedure SetName(const val : TGLGuiElementName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure   AssignTo(Dest: TPersistent); override;
  published
    property TopLeft      : TGLCoordinates read FTopLeft       write FTopLeft;
    property BottomRight  : TGLCoordinates read FBottomRight   write FBottomRight;
    property Scale        : TGLCoordinates read FScale         write FScale;
    property Align        : TGUIAlignments read FAlign         write FAlign;
    property Name         : TGLGuiElementName read FName       write SetName;
  End;

  TGLGuiComponent = class;

  TGLGuiElementList  = class(TOwnedCollection)
  private
    FGuiComponent : TGLGuiComponent;
  protected
    procedure SetItems(index : Integer; const val : TGLGuiElement);
    function GetItems(index : Integer) : TGLGuiElement;
  public
    Constructor Create(AOwner : TGLGuiComponent);
    procedure   AssignTo(Dest: TPersistent); override;

    function  GetOwner: TPersistent; override;
    property Items[index : Integer] : TGLGuiElement read GetItems write SetItems; default;
  end;

  TGLGuiComponentName = String;

  TGLGuiComponentList = class;
  TGLGuiComponent  = class(TCollectionItem)
  private
    FElements : TGLGuiElementList;
    FName : TGLGuiComponentName;
  protected
    function GetDisplayName : String; override;
    procedure SetName(const val : TGLGuiComponentName);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure   AssignTo(Dest: TPersistent); override;
    Procedure RenderToArea(X1,Y1,X2,Y2 : TGLFloat; Var Res : TGUIDrawResult; Refresh : Boolean = True; Scale : TGLFloat = 1);
    Function GetOwnerList : TGLGuiComponentList;
    property Owner : TGLGuiComponentList read GetOwnerList;
  published
    property Elements : TGLGuiElementList read FElements write FElements;
    property Name : TGLGuiComponentName read FName write SetName;
  End;

  TGLGuiLayout = class;
  TGLGuiComponentList  = class(TOwnedCollection)
  private
    FLayout : TGLGuiLayout;
  protected
    procedure SetItems(index : Integer; const val : TGLGuiComponent);
    function GetItems(index : Integer) : TGLGuiComponent;
  public
    Constructor Create(AOwner : TGLGuiLayout);

    function  GetOwner: TPersistent; override;
    function  FindItem(name : TGLGuiComponentName) : TGLGuiComponent;
    property Items[index : Integer] : TGLGuiComponent read GetItems write SetItems; default;
  end;

  TGLGuiLayout     = class(TGLUpdateableComponent)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FMaterial   : TGLMaterial;
    FGuiComponents : TGLGuiComponentList;
    FFileName   : String;
    FGuiComponentList : TList;
  protected
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;

    Procedure   SetFileName(newName : String);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;

    Procedure   LoadFromStream(Stream : TStream);
    Procedure   LoadFromFile(FN : String);

    Procedure   Clear;

    Procedure   SaveToStream(Stream : TStream);
    Procedure   SaveToFile(FN : String);
    Procedure   AddGuiComponent(Component : TGLUpdateableComponent);
    Procedure   RemoveGuiComponent(Component : TGLUpdateableComponent);

    procedure   NotifyChange(Sender : TObject); override;
  published
    property BitmapFont : TGLCustomBitmapFont read FBitmapFont write FBitmapFont;
    property Material   : TGLMaterial read FMaterial write FMaterial;
    property GuiComponents : TGLGuiComponentList read FGuiComponents write FGuiComponents;
    property FileName   : String read FFileName Write SetFileName;
  end;

Const
  GuiNullRect : TGUIRect =(X1:0.0;Y1:0.0;X2:0.0;Y2:0.0);

Function IsInRect(Const R : TGUIRect; X,Y : Single) : Boolean;

implementation

Function IsInRect(Const R : TGUIRect; X,Y : Single) : Boolean;

Begin
  Result := (R.X1 <= X) and (R.X2 >= X) and (R.Y1 <= Y) and (R.Y2 >= Y);
End;

// ------------------
// ------------------ TGLBaseGuiObject ------------------
// ------------------

// Create
//
constructor TGLBaseGuiObject.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FRecursiveVisible:=Visible;
end;

// SetLeft
//
procedure TGLBaseGuiObject.SetLeft(const val : TGLFloat);
begin
	if Position.X<>val then begin
		Position.X:=val;
	end;
end;

// GetLeft
//
Function  TGLBaseGuiObject.GetLeft : Single;
begin
   Result := Position.X;
end;

// SetTop
//
procedure TGLBaseGuiObject.SetTop(const val : TGLFloat);
begin
	if Position.Y<>val then begin
		Position.Y:=val;
	end;
end;

// GetTop
//
Function  TGLBaseGuiObject.GetTop : Single;
begin
   Result := Position.Y;
end;

// SetWidth
//
procedure TGLBaseGuiObject.SetWidth(const val : TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
      NotifyChange(Self);
	end;
end;

// SetHeight
//
procedure TGLBaseGuiObject.SetHeight(const val : TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
      NotifyChange(Self);
	end;
end;

// NotifyHide
//
procedure TGLBaseGuiObject.NotifyHide;
var
   child : TGLBaseSceneObject;
   xc : Integer;
begin
   if RecursiveVisible then begin
      FRecursiveVisible:=False;
      for xc := 0 to Count-1 do begin
         child:=Children[xc];
         if child is TGLBaseGuiObject then
            TGLBaseGuiObject(child).NotifyHide;
      end;
   end;
end;

// NotifyShow
//
procedure TGLBaseGuiObject.NotifyShow;
var
   child : TGLBaseSceneObject;
   xc : Integer;
begin
  If not RecursiveVisible then begin
     FRecursiveVisible := True;
     For xc := 0 to Count-1 do begin
        child:=Children[xc];
        if child is TGLBaseGuiObject then
           TGLBaseGuiObject(child).NotifyShow;
     end;
  end;
end;

// AddChild
//
procedure TGLBaseGuiObject.AddChild(aChild : TGLBaseSceneObject);
begin
   inherited;
   if AChild is TGLBaseGuiObject then begin
      if RecursiveVisible then
         TGLBaseGuiObject(AChild).NotifyShow
      else TGLBaseGuiObject(AChild).NotifyHide;
   end;
end;

// Insert
//
procedure TGLBaseGuiObject.Insert(aIndex : Integer; aChild : TGLBaseSceneObject);
begin
   inherited;
   if AChild is TGLBaseGuiObject then begin
      if RecursiveVisible then
         TGLBaseGuiObject(AChild).NotifyShow
      else TGLBaseGuiObject(AChild).NotifyHide;
   end;
end;

// SetVisible
//
procedure TGLBaseGuiObject.SetVisible(aValue : Boolean);
begin
   if Visible<>aValue then begin
      inherited SetVisible(aValue);
      if aValue then begin
         if Parent<>nil then
            if Parent is TGLBaseGuiObject then begin
               if TGLBaseGuiObject(Parent).RecursiveVisible then
                  NotifyShow;
            end else begin
               if Parent.Visible then
                  NotifyShow;
            end;
      end else begin
         if RecursiveVisible then
            NotifyHide;
      end;
   end;
end;

Constructor TGLGuiLayout.Create(AOwner : TComponent);

Begin
  FGuiComponentList := TList.Create;
  inherited;
  FGuiComponents := TGLGuiComponentList.Create(Self);
  FMaterial      := TGLMaterial.Create(Self);
End;

Destructor  TGLGuiLayout.Destroy;

Begin
  Clear;
  FMaterial.Free;
  FGuiComponents.Free;
  inherited;
  FGuiComponentList.Free;
End;

Procedure   TGLGuiLayout.SetFileName(newName : String);

Begin
  If newName <> FFileName then
  Begin
    FFileName := newName;
    If FileExists(FFileName) then
    Begin
      Clear;
      loadFromFile(FFileName);
    End;
  End;
End;

Procedure   TGLGuiLayout.LoadFromFile(FN : String);

Var
  Stream : TMemoryStream;

Begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FN);
    LoadFromStream(stream);
  finally
    stream.Free;
  End;
End;

Procedure   TGLGuiLayout.SaveToFile(FN : String);

Var
  Stream : TMemoryStream;

Begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Stream.SaveToFile(FN);
  finally
    Stream.Free;
  End;
End;

Procedure   TGLGuiLayout.AddGuiComponent(Component : TGLUpdateableComponent);

Begin
  FreeNotification(Component);
  FGuiComponentList.Add(Component);
End;

Procedure   TGLGuiLayout.RemoveGuiComponent(Component : TGLUpdateableComponent);

Begin
  FGuiComponentList.Remove(Component);
  RemoveFreeNotification(Component);
End;

Procedure   TGLGuiLayout.Clear;

Var
  XC : Integer;

Begin
  For XC := FGuiComponents.Count-1 downto 0 do
  Begin
    FGuiComponents.Delete(XC);
  End;
  NotifyChange(Self);
end;

procedure   TGLGuiLayout.NotifyChange(Sender : TObject);
Var
  XC : Integer;
Begin
  inherited;

  For XC := FGuiComponentList.Count-1 downto 0 do
  TGLUpdateAbleComponent(FGuiComponentList[XC]).NotifyChange(Self);
End;

Procedure   TGLGuiLayout.LoadFromStream(Stream : TStream);

Var
  TmpComponent : TGLGuiComponent;
  XC,YC,ZC : Integer;
  TmpElement   : TGLGuiElement;
  TmpAlignment : TGUIAlignments;
  Version : Integer;
  Data : TBinaryReader;

Begin
  Data := TBinaryReader.Create(Stream);
  try

    Version := Data.ReadInteger;
    If Version <> 1 then Exit;


    For XC := 0 to Data.ReadInteger-1 do
    Begin
      TmpComponent := FGuiComponents.Add as TGLGuiComponent;
      TmpComponent.FName := Data.ReadString;
      For YC := 0 to Data.ReadInteger-1 do
      Begin
        TmpElement   := TmpComponent.FElements.add as TGLGuiElement;

        TmpElement.FName := Data.ReadString;

        TmpElement.FTopLeft.X := Data.ReadFloat;
        TmpElement.FTopLeft.Y := Data.ReadFloat;
        TmpElement.FTopLeft.Z := Data.ReadFloat;

        TmpElement.FBottomRight.X := Data.ReadFloat;
        TmpElement.FBottomRight.Y := Data.ReadFloat;
        TmpElement.FBottomRight.Z := Data.ReadFloat;

        TmpElement.FScale.X := Data.ReadFloat;
        TmpElement.FScale.Y := Data.ReadFloat;
        TmpElement.FScale.Z := Data.ReadFloat;

        For ZC := 0 to Data.ReadInteger-1 do
        Begin
          TmpAlignment :=  TGUIAlignments(Data.ReadInteger);
          TmpElement.FAlign := TmpAlignment;
        End;
      End;
    End;
  finally
    Data.Free;
  end;
  NotifyChange(Self);
End;

Procedure   TGLGuiLayout.SaveToStream(stream : TStream);

Var
  TmpComponent : TGLGuiComponent;
  Alignments, XC, YC : Integer;
  TmpElement   : TGLGuiElement;
  TmpAlignment : TGUIAlignments;
  Data : TBinaryWriter;

Begin
  Data := TBinaryWriter.Create(Stream);
  try
    Data.WriteInteger(1);
    Data.WriteInteger(FGuiComponents.Count);
    For XC := 0 to FGuiComponents.Count-1 do
    Begin
      TmpComponent := FGuiComponents.Items[XC];
      Data.WriteString(TmpComponent.FName);

      Data.WriteInteger(TmpComponent.FElements.Count);

      For YC := 0 to TmpComponent.FElements.Count-1 do
      Begin
        TmpElement   := TmpComponent.FElements.Items[YC];

        Data.WriteString(TmpElement.FName);

        Data.WriteFloat(TmpElement.FTopLeft.X);
        Data.WriteFloat(TmpElement.FTopLeft.Y);
        Data.WriteFloat(TmpElement.FTopLeft.Z);

        Data.WriteFloat(TmpElement.FBottomRight.X);
        Data.WriteFloat(TmpElement.FBottomRight.Y);
        Data.WriteFloat(TmpElement.FBottomRight.Z);

        Data.WriteFloat(TmpElement.FScale.X);
        Data.WriteFloat(TmpElement.FScale.Y);
        Data.WriteFloat(TmpElement.FScale.Z);

        Alignments := 0;
        For TmpAlignMent := GLAlTopLeft to GLAlBorder do
        Begin
          If TmpAlignMent = TmpElement.FAlign then inc(Alignments);
        End;

        Data.WriteInteger(Alignments);

        For TmpAlignMent := GLAlTopLeft to GLAlBorder do
        Begin
          If TmpAlignMent = TmpElement.FAlign then
          Data.WriteInteger(Integer(TmpAlignMent));
        End;
      End;
    End;
  finally
    Data.Free;
  End;
End;

Constructor TGLGuiComponentList.Create(AOwner : TGLGuiLayout);

Begin
  inherited Create(AOwner, TGLGuiComponent);
  FLayout := AOwner;
End;

function  TGLGuiComponentList.GetOwner: TPersistent;
Begin
  Result := FLayout;
End;

procedure TGLGuiComponentList.SetItems(index : Integer; const val : TGLGuiComponent);
begin
	inherited Items[index]:=val;
end;

function  TGLGuiComponentList.FindItem(name : TGLGuiComponentName) : TGLGuiComponent;

Var
  XC : Integer;
  gc : TGLGuiComponent;
Begin
  Result := Nil;
  If Name = '' then Exit;
  For XC := 0 to Count-1 do
  Begin
    gc := Items[xc];
    If gc.FName = Name then
    Begin
      Result := gc;
      Break;
    End;
  End;
End;


function TGLGuiComponentList.GetItems(index : Integer) : TGLGuiComponent;
begin
	Result:=TGLGuiComponent(inherited Items[index]);
end;

Procedure TGLGuiComponent.RenderToArea(X1,Y1,X2,Y2 : TGLFloat; Var Res : TGUIDrawResult; Refresh : Boolean = True; Scale : TGLFloat = 1);

Var
  XC          : Integer;
  ThisElement : TGLGuiElement;
  W,H         : TGLFloat;
  Len1,Len2   : TGLFloat;
  Layout      : TGLGuiLayout;
  LibMaterial : TGLLibMaterial;
  Material    : TGLMaterial;
  TexWidth,
  TexHeight   : TGLFloat;
  AlignCount  : TGUIAlignments;
  TmpElement  : TGLGuiElement;

Procedure Prepare;

Begin
  Len1 := (ThisElement.FTopLeft.x-ThisElement.FBottomRight.x)*ThisElement.Scale.X*Scale;
  Len2 := (ThisElement.FTopLeft.y-ThisElement.FBottomRight.y)*ThisElement.Scale.Y*Scale;
  If Len1 < 0 then
  Begin
    If Len2 < 0 then
    Begin
      W     := -Len1;
      H     := -Len2;
    End else
    Begin
      W     := -Len1;
      H     := Len2;
    End;
  End else
  Begin
    If Len2 < 0 then
    Begin
      W     := Len1;
      H     := -Len2;
    End else
    Begin
      W     := Len1;
      H     := Len2;
    End;
  End;
End;

Procedure RenderIt(Var ARect : TGuiRect; AElement : TGLGuiElement);

Var
  XC : TGLFloat;
  YC : TGLFloat;
  XPos,X2Pos : TGLFloat;
  YPos,y2Pos : TGLFloat;
  tx1,ty1,tx2,ty2 : TGLFloat;
  XTileSize, YTileSize : TGLFloat;
  tx3,ty3 : TGLFloat;
  tx,ty : TGLFloat;

Begin
  If (ARect.XTiles = 1) and (ARect.YTiles = 1) then
  Begin
    glTexCoord2f( AElement.TopLeft.X/TexWidth,     -AElement.TopLeft.Y/TexHeight);
    glVertex2f(ARect.X1, -ARect.Y1);

    glTexCoord2f( AElement.TopLeft.X/TexWidth,     -AElement.BottomRight.Y/TexHeight);
    glVertex2f(ARect.X1, -ARect.Y2);

    glTexCoord2f( AElement.BottomRight.X/TexWidth, -AElement.BottomRight.Y/TexHeight);
    glVertex2f(ARect.X2, -ARect.Y2);

    glTexCoord2f( AElement.BottomRight.X/TexWidth, -AElement.TopLeft.Y/TexHeight);
    glVertex2f(ARect.X2, -ARect.Y1);
  End else
  Begin
    XTileSize := (ARect.X2 - ARect.X1) / ARect.XTiles;
    YTileSize := (ARect.Y2 - ARect.Y1) / ARect.YTiles;
    tx1 := AElement.TopLeft.X/TexWidth;
    ty1 := -AElement.TopLeft.Y/TexHeight;
    tx2 := AElement.BottomRight.X/TexWidth;
    ty2 := -AElement.BottomRight.Y/TexHeight;
    tx3 := (AElement.TopLeft.X+(AElement.BottomRight.X-AElement.TopLeft.X)*Frac(ARect.XTiles))/TexWidth;
    ty3 := -(AElement.TopLeft.y+(AElement.BottomRight.y-AElement.TopLeft.y)*Frac(ARect.yTiles))/TexHeight;

    XC := ARect.XTiles;
    XPos := ARect.X1;
    tx := tx2;
    While XC > 0 do
    Begin
      YC := ARect.YTiles;
      YPos := ARect.Y1;
      ty := ty2;

      If XC >= 1 then X2Pos := XPos+XTileSize
      else
      Begin
        X2Pos := ARect.X2;
        tx := tx3;
      End;

      While YC > 0 do
      Begin
        If YC >= 1 then Y2Pos := YPos+YTileSize
        else
        Begin
          Y2Pos := ARect.Y2;
          ty := ty3;
        End;

        glTexCoord2f( tx1,     ty1);
        glVertex2f(XPos, -YPos);

        glTexCoord2f( tx1,     ty);
        glVertex2f(XPos, -Y2Pos);

        glTexCoord2f( tx,     ty);
        glVertex2f(X2Pos, -Y2Pos);

        glTexCoord2f( tx,     ty1);
        glVertex2f(X2Pos, -YPos);
        yc := yc - 1.0;
        ypos := Y2Pos;
      End;
      xc := xc - 1.0;
      xpos := X2Pos;
    End;
  End;
End;

Procedure RenderBorder(AElement : TGLGuiElement);

Var
  TmpElement : TGLGuiElement;

Begin
  TmpElement := TGLGuiElement.Create(Nil);
  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
  TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y;
  TmpElement.FBottomRight.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FBottomRight.Y := ThisElement.FTopLeft.Y+ThisElement.Scale.Y;
  TmpElement.Scale.SetPoint(1,1,1);
  RenderIt(Res[GLALTopLeft],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FBottomRight.X := ThisElement.FBottomRight.X-ThisElement.Scale.X;
  RenderIt(Res[GLALTop],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FBottomRight.X-ThisElement.Scale.X;
  TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
  RenderIt(Res[GLALTopRight],TmpElement);

  TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y+ThisElement.Scale.Y;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  RenderIt(Res[GLALRight],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FBottomRight.X-ThisElement.Scale.X;
  TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  TmpElement.FBottomRight.X := ThisElement.FBottomRight.X;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
  RenderIt(Res[GLALBottomRight],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  TmpElement.FBottomRight.X := ThisElement.FBottomRight.X-ThisElement.Scale.X;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
  RenderIt(Res[GLALBottom],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
  TmpElement.FTopLeft.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  TmpElement.FBottomRight.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y;
  RenderIt(Res[GLALBottomLeft],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X;
  TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y+ThisElement.Scale.Y;
  TmpElement.FBottomRight.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  RenderIt(Res[GLALLeft],TmpElement);

  TmpElement.FTopLeft.X := ThisElement.FTopLeft.X+ThisElement.Scale.X;
  TmpElement.FTopLeft.Y := ThisElement.FTopLeft.Y+ThisElement.Scale.Y;
  TmpElement.FBottomRight.X := ThisElement.FBottomRight.X-ThisElement.Scale.X;
  TmpElement.FBottomRight.Y := ThisElement.FBottomRight.Y-ThisElement.Scale.Y;
  RenderIt(Res[GLALCenter],TmpElement);
End;



Begin
  Layout := ((GetOwner as TGLGuiComponentList).GetOwner as TGLGuiLayout);
  Material := Nil;
  If Assigned(Layout.Material.MaterialLibrary) and (Layout.Material.LibMaterialName <> '') then
  Begin
    LibMaterial := Layout.Material.MaterialLibrary.Materials.GetLibMaterialByName(Layout.Material.LibMaterialName);
    If Assigned(LibMaterial) then
    Material := LibMaterial.Material;
  End;
  If not Assigned(Material) then
  Begin
    Material := Layout.Material;
  End;

  If Refresh Then
  Begin
    Res[GLALtopLeft].X1 := X1;
    Res[GLALtopLeft].Y1 := Y1;
    Res[GLALtopLeft].X2 := X1;
    Res[GLALtopLeft].Y2 := Y1;

    Res[GLALtopRight].X1 := X2;
    Res[GLALtopRight].Y1 := Y1;
    Res[GLALtopRight].X2 := X2;
    Res[GLALtopRight].Y2 := Y1;

    Res[GLALBottomLeft].X1 := X1;
    Res[GLALBottomLeft].Y1 := Y2;
    Res[GLALBottomLeft].X2 := X1;
    Res[GLALBottomLeft].Y2 := Y2;

    Res[GLALBottomRight].X1 := X2;
    Res[GLALBottomRight].Y1 := Y2;
    Res[GLALBottomRight].X2 := X2;
    Res[GLALBottomRight].Y2 := Y2;

    For XC := 0 to FElements.Count-1 do
    Begin
      ThisElement := FElements[XC];
      If GLAlBorder = ThisElement.Align then
      Begin
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALtopLeft].Y2 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;

        Res[GLALtop].X1 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].X2 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALtop].Y2 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;

        Res[GLALtopRight].X1 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;

        Res[GLALRight].X1 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALRight].Y1 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALRight].X2 := X2;
        Res[GLALRight].Y2 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;

        Res[GLALBottomRight].X1 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALBottomRight].Y1 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;

        Res[GLALBottom].X1 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALBottom].Y1 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALBottom].X2 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALBottom].Y2 := Y2;

        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALBottomLeft].X2 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALBottomLeft].Y2 := Y2;

        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].Y1 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALLeft].X2 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALLeft].Y2 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;

        Res[GLALCenter].X1 := X1+ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALCenter].Y1 := Y1+ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
        Res[GLALCenter].X2 := X2-ThisElement.Scale.X*Scale*ThisElement.Scale.Z;
        Res[GLALCenter].Y2 := Y2-ThisElement.Scale.Y*Scale*ThisElement.Scale.Z;
      End;


      If GLALtopLeft = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALtopLeft].X1 := X1;
        Res[GLALtopLeft].Y1 := Y1;
        Res[GLALtopLeft].X2 := X1+W;
        Res[GLALtopLeft].Y2 := Y1+H;
      End;
      If GLALtopRight = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALtopRight].X1 := X2-W;
        Res[GLALtopRight].Y1 := Y1;
        Res[GLALtopRight].X2 := X2;
        Res[GLALtopRight].Y2 := Y1+H;
      End;
      If GLALBottomLeft = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALBottomLeft].X1 := X1;
        Res[GLALBottomLeft].Y1 := Y2-H;
        Res[GLALBottomLeft].X2 := X1+W;
        Res[GLALBottomLeft].Y2 := Y2;
      End;
      If GLALBottomRight = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALBottomRight].X1 := X2-W;
        Res[GLALBottomRight].Y1 := Y2-H;
        Res[GLALBottomRight].X2 := X2;
        Res[GLALBottomRight].Y2 := Y2;
      End;
    End;

    Res[GLALtop].X1 := Res[GLALtopLeft].X2;
    Res[GLALtop].Y1 := Res[GLALtopRight].Y1;
    Res[GLALtop].X2 := Res[GLALtopRight].X1;
    Res[GLALtop].Y2 := Res[GLALtopLeft].Y2;


    Res[GLALBottom].X1 := Res[GLALBottomLeft].X2;
    Res[GLALBottom].Y1 := Res[GLALBottomLeft].Y1;
    Res[GLALBottom].X2 := Res[GLALBottomRight].X1;
    Res[GLALBottom].Y2 := Res[GLALBottomRight].Y2;

    Res[GLALLeft].X1 := Res[GLALtopLeft].X1;
    Res[GLALLeft].Y1 := Res[GLALtopLeft].Y2;
    Res[GLALLeft].X2 := Res[GLALBottomLeft].X2;
    Res[GLALLeft].Y2 := Res[GLALBottomLeft].Y1;

    Res[GLALRight].X1 := Res[GLALtopRight].X1;
    Res[GLALRight].Y1 := Res[GLALtopRight].Y2;
    Res[GLALRight].X2 := Res[GLALBottomRight].X2;
    Res[GLALRight].Y2 := Res[GLALBottomRight].Y1;

    For XC := 0 to FElements.Count-1 do
    Begin
      ThisElement := FElements[XC];
      If GLALtop = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALtop].Y1 := Y1;
        Res[GLALtop].Y2 := Y1+H;
      End;
      If GLALBottom = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALBottom].Y1 := Y2-H;
        Res[GLALBottom].Y2 := Y2;
      End;
      If GLALLeft = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALLeft].X1 := X1;
        Res[GLALLeft].X2 := X1+W;
      End;
      If GLALRight = ThisElement.Align then
      Begin
        Prepare;
        Res[GLALRight].X1 := X2-W;
        Res[GLALRight].X2 := X2;
      End;
    End;

    Res[GLALCenter].X1 := Res[GLALLeft].X2;
    Res[GLALCenter].Y1 := Res[GLALtop].Y2;
    Res[GLALCenter].X2 := Res[GLALRight].X1;
    Res[GLALCenter].Y2 := Res[GLALBottom].Y1;
  End;


  TexWidth  := Material.Texture.TexWidth;
  If TexWidth = 0 then
  TexWidth := Material.Texture.Image.Width;
  
  TexHeight := Material.Texture.TexHeight;
  If TexHeight = 0 then
  TexHeight := Material.Texture.Image.Height;

  glBegin(GL_QUADS);

    For XC := 0 to FElements.Count-1 do
    Begin
      ThisElement := FElements[XC];
      For AlignCount := GLAlTopLeft to GLAlBottomRight do
      If (AlignCount = ThisElement.Align) then
      Begin
        If Refresh then
        Begin
          Res[AlignCount].XTiles  := ((Res[AlignCount].X2-Res[AlignCount].X1)/(ThisElement.FBottomRight.X-ThisElement.FTopLeft.X))/ThisElement.Scale.X;
          Res[AlignCount].YTiles  := ((Res[AlignCount].Y2-Res[AlignCount].Y1)/(ThisElement.FBottomRight.Y-ThisElement.FTopLeft.Y))/ThisElement.Scale.Y;
        End;
        RenderIt(Res[AlignCount],ThisElement);
      End;
      If (GLALBorder = ThisElement.Align) then
      Begin
        RenderBorder(ThisElement);
      End;

    End;
  glEnd();
End;

Function TGLGuiComponent.GetOwnerList : TGLGuiComponentList;

Begin
  Result := GetOwner as TGLGuiComponentList;
End;

function TGLGuiComponent.GetDisplayName : String;

Begin
  Result := FName;
End;

procedure TGLGuiComponent.SetName(const val : TGLGuiComponentName);

Begin
  FName := Val;
End;

constructor TGLGuiComponent.Create(Collection: TCollection);

Begin
  inherited;
  FElements := TGLGuiElementList.Create(Self);
End;

destructor TGLGuiComponent.Destroy;
Begin
  FElements.Free;
  inherited;
End;

Constructor TGLGuiElementList.Create(AOwner : TGLGuiComponent);

Begin
  inherited Create(AOwner,TGLGuiElement);
  FGuiComponent := AOwner;
End;

function  TGLGuiElementList.GetOwner: TPersistent;

Begin
  Result := FGuiComponent;
End;

procedure TGLGuiElementList.SetItems(index : Integer; const val : TGLGuiElement);

Begin
  inherited Items[index]:=val;
End;

function TGLGuiElementList.GetItems(index : Integer) : TGLGuiElement;

begin
  Result:=TGLGuiElement(inherited Items[index]);
end;

function TGLGuiElement.GetDisplayName : String;

Begin
  Result := FName;
End;

procedure TGLGuiElement.SetName(const val : TGLGuiElementName);

Begin
  FName := Val;
End;

constructor TGLGuiElement.Create(Collection: TCollection);

Begin
  inherited;
  FTopLeft := TGLCoordinates.Create(Self);
  FBottomRight := TGLCoordinates.Create(Self);
  FScale := TGLCoordinates.Create(Self);
  FScale.X := 1;
  FScale.Y := 1;
End;

destructor TGLGuiElement.Destroy;
begin
  FTopLeft.Free;
  FBottomRight.Free;
  FScale.Free;
  inherited;
end;

procedure TGLGuiLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FBitmapFont) then
      BitmapFont:=nil;
   NotifyChange(Self); // EG : looks suspicious...
   inherited;
end;

procedure TGLGuiComponent.AssignTo(Dest: TPersistent);
begin
   if Dest is TGLGuiComponent then
   Begin
     TGLGuiComponent(Dest).Elements.Assign(Elements);
   End else inherited;
end;

procedure TGLGuiElementList.AssignTo(Dest: TPersistent);
Var
   i : Integer;
   //element : TGLGuiElement;
begin
   if Dest is TGLGuiElementList then
   Begin
      for i := 0 to Count-1 do
      Begin
         TGLGuiElementList(Dest).Add.Assign(Items[i]);
      End;
   End else inherited;
end;

procedure TGLGuiElement.AssignTo(Dest: TPersistent);
Var
   //i : Integer;
   element : TGLGuiElement;
begin
   if Dest is TGLGuiElement then
   Begin
      element := TGLGuiElement(Dest);

      element.TopLeft.Assign(TopLeft);
      element.BottomRight.Assign(BottomRight);
      element.Scale.Assign(Scale);
      element.Align := Align;
      element.Name  := Name;
   End else inherited;
end;

end.
