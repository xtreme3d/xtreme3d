//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGameMenu<p>

   Manages a basic game menu UI<p>

	<b>History : </b><font size=-1><ul>
      <li>03/27/06 - DaveK - added mouse selection support
      <li>03/03/05 - EG - Creation
   </ul></font>
}
unit GLGameMenu;

interface

uses Classes, GLScene, GLTexture, GLBitmapFont, GLCrossPlatform;

type

   // TGLGameMenuScale
   //
   TGLGameMenuScale = (gmsNormal, gms1024x768);

   // TGLGameMenu
   //
   {: Classic game menu interface made of several lines.<p> }
   TGLGameMenu = class(TGLSceneObject)
      private
         { Private Properties }
         FItems : TStrings;
         FSelected : Integer;
         FFont : TGLCustomBitmapFont;
         FMarginVert, FMarginHorz, FSpacing : Integer;
         FMenuScale : TGLGameMenuScale;
         FBackColor : TGLColor;
         FInactiveColor, FActiveColor, FDisabledColor : TGLColor;
         FMaterialLibrary : TGLMaterialLibrary;
         FTitleMaterialName : String;
         FTitleWidth, FTitleHeight : Integer;
         FOnSelectedChanged : TNotifyEvent;
         FBoxTop, FBoxBottom, FBoxLeft, FBoxRight: Integer;
         FMenuTop: integer;

		protected
         { Protected Properties }
         procedure SetMenuScale(val : TGLGameMenuScale);
         procedure SetMarginHorz(val : Integer);
         procedure SetMarginVert(val : Integer);
         procedure SetSpacing(val : Integer);
         procedure SetFont(val : TGLCustomBitmapFont);
         procedure SetBackColor(val : TGLColor);
         procedure SetInactiveColor(val : TGLColor);
         procedure SetActiveColor(val : TGLColor);
         procedure SetDisabledColor(val : TGLColor);
         function  GetEnabled(index : Integer) : Boolean;
         procedure SetEnabled(index : Integer; val : Boolean);
         procedure SetItems(val : TStrings);
         procedure SetSelected(val : Integer);
         function  GetSelectedText : String;
         procedure SetMaterialLibrary(val : TGLMaterialLibrary);
         procedure SetTitleMaterialName(const val : String);
         procedure SetTitleWidth(val : Integer);
         procedure SetTitleHeight(val : Integer);

         procedure ItemsChanged(Sender : TObject);

		public
         { Public Properties }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
		   procedure BuildList(var rci : TRenderContextInfo); override;

         property Enabled[index : Integer] : Boolean read GetEnabled write SetEnabled;
         property SelectedText : String read GetSelectedText;

         procedure SelectNext;
         procedure SelectPrev;

         procedure MouseMenuSelect(const X, Y: integer);

		published
         { Published Properties }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;

         property MenuScale : TGLGameMenuScale read FMenuScale write SetMenuScale default gmsNormal;
         property MarginHorz : Integer read FMarginHorz write SetMarginHorz default 16;
         property MarginVert : Integer read FMarginVert write SetMarginVert default 16;
         property Spacing : Integer read FSpacing write SetSpacing default 16;
         property Font : TGLCustomBitmapFont read FFont write SetFont;

         property TitleMaterialName : String read FTitleMaterialName write SetTitleMaterialName;
         property TitleWidth : Integer read FTitleWidth write SetTitleWidth default 0;
         property TitleHeight : Integer read FTitleHeight write SetTitleHeight default 0;

         property BackColor : TGLColor read FBackColor write SetBackColor;
         property InactiveColor : TGLColor read FInactiveColor write SetInactiveColor;
         property ActiveColor : TGLColor read FActiveColor write SetActiveColor;
         property DisabledColor : TGLColor read FDisabledColor write SetDisabledColor;

         property Items : TStrings read FItems write SetItems;
         property Selected : Integer read FSelected write SetSelected default -1;
         property OnSelectedChanged : TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;

         // these are the extents of the menu
         property BoxTop: integer read FBoxTop;
         property BoxBottom: integer read FBoxBottom;
         property BoxLeft: integer read FBoxLeft;
         property BoxRight: integer read FBoxRight;
         // this is the top of the first menu item
         property MenuTop: integer read FMenuTop;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLCanvas, OpenGL1x;

// ------------------
// ------------------ TGLGameMenu ------------------
// ------------------

// Create
//
constructor TGLGameMenu.Create(AOwner: TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FItems:=TStringList.Create;
   TStringList(FItems).OnChange:=ItemsChanged;
   FSelected:=-1;
   FMarginHorz:=16;
   FMarginVert:=16;
   FSpacing:=16;
   FMenuScale:=gmsNormal;
   FBackColor:=TGLColor.CreateInitialized(Self, clrTransparent, NotifyChange);
   FInactiveColor:=TGLColor.CreateInitialized(Self, clrGray75, NotifyChange);
   FActiveColor:=TGLColor.CreateInitialized(Self, clrWhite, NotifyChange);
   FDisabledColor:=TGLColor.CreateInitialized(Self, clrGray60, NotifyChange);
end;

// Destroy
//
destructor TGLGameMenu.Destroy;
begin
   inherited;
   Font:=nil;
   FBackColor.Free;
   FInactiveColor.Free;
   FActiveColor.Free;
   FDisabledColor.Free;
end;

// Notification
//
procedure TGLGameMenu.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if Operation=opRemove then begin
      if AComponent=Font then Font:=nil;
      if AComponent=MaterialLibrary then MaterialLibrary:=nil;
   end;
end;

// BuildList
//
procedure TGLGameMenu.BuildList(var rci : TRenderContextInfo);
var
   canvas : TGLCanvas;
   buffer : TGLSceneBuffer;
   i, w, h, tw, y : Integer;
   color : TColorVector;
   libMat : TGLLibMaterial;
begin
   if Font=nil then Exit;
   case MenuScale of
      gmsNormal : begin
         buffer:=TGLSceneBuffer(rci.buffer);
         canvas:=TGLCanvas.Create(buffer.Width, buffer.Height);
      end;
      gms1024x768 : canvas:=TGLCanvas.Create(1024, 768);
   else
      canvas:=nil;
      Assert(False);
   end;
   try
      // determine extents
      h:=FItems.Count*(Font.CharHeight+Spacing)-Spacing+MarginVert*2;
      if TitleHeight>0 then
         h:=h+TitleHeight+Spacing;
      w:=TitleWidth;
      for i:=0 to FItems.Count-1 do begin
         tw:=Font.TextWidth(FItems[i]);
         if tw>w then w:=tw;
      end;
      w:=w+2*MarginHorz;

      // calculate boundaries for user
      FBoxLeft   := Variant(Position.X)-w div 2;
      FBoxTop    := Variant(Position.Y)-h div 2;
      FBoxRight  := Variant(Position.X)+w div 2;
      FBoxBottom := Variant(Position.Y)+h div 2;

      // paint back
      if BackColor.Alpha>0 then begin
         canvas.PenColor:=BackColor.AsWinColor;
         canvas.PenAlpha:=BackColor.Alpha;
         canvas.FillRect(FBoxLeft, FBoxTop, FBoxRight, FBoxBottom);
      end;

      canvas.StopPrimitive;
      
      // paint items
      y:=Round(Position.Y-h div 2+MarginVert);
      if TitleHeight>0 then begin
         if (TitleMaterialName<>'') and (MaterialLibrary<>nil) and (TitleWidth>0) then begin
            libMat:=MaterialLibrary.LibMaterialByName(TitleMaterialName);
            if libMat<>nil then begin
               libMat.Apply(rci);
               glBegin(GL_QUADS);
                  glTexCoord2f(0, 1);  glVertex2f(Position.X-TitleWidth div 2, y);
                  glTexCoord2f(1, 1);  glVertex2f(Position.X+TitleWidth div 2, y);
                  glTexCoord2f(1, 0);  glVertex2f(Position.X+TitleWidth div 2, y+TitleHeight);
                  glTexCoord2f(0, 0);  glVertex2f(Position.X-TitleWidth div 2, y+TitleHeight);
               glEnd;
               libMat.UnApply(rci);
            end;
         end;
         y:=y+TitleHeight+Spacing;
         FMenuTop := y;
      end;
      for i:=0 to FItems.Count-1 do begin
         tw:=Font.TextWidth(FItems[i]);
         if not Enabled[i] then
            color:=DisabledColor.Color
         else if i=Selected then
            color:=ActiveColor.Color
         else color:=InactiveColor.Color;
         Font.TextOut(rci, Position.X-tw div 2, y, FItems[i], color);
         y:=y+Font.CharHeight+Spacing;
      end;
   finally
      canvas.Free;
   end;
   glEnable(GL_BLEND); // to match rci change
end;

// SelectNext
//
procedure TGLGameMenu.SelectNext;
var
   i : Integer;
begin
   i:=Selected;
   repeat
      i:=i+1;
   until (i>=Items.Count) or Enabled[i];
   if (i<Items.Count) and (i<>Selected) then
      Selected:=i;
end;

// SelectPrev
//
procedure TGLGameMenu.SelectPrev;
var
   i : Integer;
begin
   i:=Selected;
   repeat
      i:=i-1;
   until (i<0) or Enabled[i];
   if (i>=0) and (i<>Selected) then
      Selected:=i;
end;

// SetMenuScale
//
procedure TGLGameMenu.SetMenuScale(val : TGLGameMenuScale);
begin
   if FMenuScale<>val then begin
      FMenuScale:=val;
      StructureChanged;
   end;
end;

// SetMarginHorz
//
procedure TGLGameMenu.SetMarginHorz(val : Integer);
begin
   if FMarginHorz<>val then begin
      FMarginHorz:=val;
      StructureChanged;
   end;
end;

// SetMarginVert
//
procedure TGLGameMenu.SetMarginVert(val : Integer);
begin
   if FMarginVert<>val then begin
      FMarginVert:=val;
      StructureChanged;
   end;
end;

// SetSpacing
//
procedure TGLGameMenu.SetSpacing(val : Integer);
begin
   if FSpacing<>val then begin
      FSpacing:=val;
      StructureChanged;
   end;
end;

// SetFont
//
procedure TGLGameMenu.SetFont(val : TGLCustomBitmapFont);
begin
   if FFont<>nil then
      FFont.RemoveFreeNotification(Self);
   FFont:=val;
   if FFont<>nil then
      FFont.FreeNotification(Self);
end;

// SetBackColor
//
procedure TGLGameMenu.SetBackColor(val : TGLColor);
begin
   FBackColor.Assign(val);
end;

// SetInactiveColor
//
procedure TGLGameMenu.SetInactiveColor(val : TGLColor);
begin
   FInactiveColor.Assign(val);
end;

// SetActiveColor
//
procedure TGLGameMenu.SetActiveColor(val : TGLColor);
begin
   FActiveColor.Assign(val);
end;

// SetDisabledColor
//
procedure TGLGameMenu.SetDisabledColor(val : TGLColor);
begin
   FDisabledColor.Assign(val);
end;

// GetEnabled
//
function TGLGameMenu.GetEnabled(index : Integer) : Boolean;
begin
   Result:=not Boolean(FItems.Objects[index]);
end;

// SetEnabled
//
procedure TGLGameMenu.SetEnabled(index : Integer; val : Boolean);
begin
   FItems.Objects[index]:=TObject(not val);
   StructureChanged;
end;

// SetItems
//
procedure TGLGameMenu.SetItems(val : TStrings);
begin
   FItems.Assign(val);
   SetSelected(Selected);
end;

// SetSelected
//
procedure TGLGameMenu.SetSelected(val : Integer);
begin
   if val<-1 then val:=-1;
   if val>=FItems.Count then val:=FItems.Count-1;
   if val<>FSelected then begin
      FSelected:=val;
      StructureChanged;
      if Assigned(FOnSelectedChanged) then
         FOnSelectedChanged(Self);
   end;
end;

// GetSelectedText
//
function TGLGameMenu.GetSelectedText : String;
begin
   if Cardinal(Selected)<Cardinal(FItems.Count) then
      Result:=FItems[Selected]
   else Result:='';
end;

// SetMaterialLibrary
//
procedure TGLGameMenu.SetMaterialLibrary(val : TGLMaterialLibrary);
begin
   if FMaterialLibrary<>nil then
      FMaterialLibrary.RemoveFreeNotification(Self);
   FMaterialLibrary:=val;
   if FMaterialLibrary<>nil then
      FMaterialLibrary.FreeNotification(Self);
end;

// SetTitleMaterialName
//
procedure TGLGameMenu.SetTitleMaterialName(const val : String);
begin
   if FTitleMaterialName<>val then begin
      FTitleMaterialName:=val;
      StructureChanged;
   end;
end;

// SetTitleWidth
//
procedure TGLGameMenu.SetTitleWidth(val : Integer);
begin
   if val<0 then val:=0;
   if FTitleWidth<>val then begin
      FTitleWidth:=val;
      StructureChanged;
   end;
end;

// SetTitleHeight
//
procedure TGLGameMenu.SetTitleHeight(val : Integer);
begin
   if val<0 then val:=0;
   if FTitleHeight<>val then begin
      FTitleHeight:=val;
      StructureChanged;
   end;
end;

// ItemsChanged
//
procedure TGLGameMenu.ItemsChanged(Sender : TObject);
begin
   SetSelected(FSelected);
   StructureChanged;
end;

// MouseMenuSelect
//
procedure TGLGameMenu.MouseMenuSelect(const X, Y: integer);
var
  myIndex: integer;
begin
  myIndex := -1;
  if (X >= BoxLeft)  and (Y >= MenuTop) and
     (X <= BoxRight) and (Y <= BoxBottom) then
  begin
    myIndex := (Y-MenuTop) div (Font.CharHeight + Spacing);
  end;
  Selected := myIndex;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClass(TGLGameMenu);

end.
