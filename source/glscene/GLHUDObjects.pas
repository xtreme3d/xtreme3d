// GLHUDObjects
{: GLScene objects that get rendered in 2D coordinates<p>

	<b>History : </b><font size=-1><ul>
      <li>28/06/04 - LR - Change TTextLayout to TGLTextLayout for Linux
      <li>27/11/02 - EG - HUDSprite and HUDText now honour renderDPI
      <li>23/11/02 - EG - Added X/YTiles to HUDSprite
      <li>12/05/02 - EG - ModulateColor for HUDText (Nelson Chu)
      <li>20/12/01 - EG - PolygonMode properly adjusted for HUDText
      <li>18/07/01 - EG - VisibilityCulling compatibility changes
      <li>20/06/01 - EG - Default hud sprite size is now 16x16
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
	   <li>15/01/01 - EG - Creation
	</ul></font>
}
unit GLHUDObjects;

interface

uses
   Classes, GLScene, VectorGeometry, GLMisc, GLObjects, GLBitmapFont,
   GLTexture, GLCrossPlatform;

type

   // TGLHUDSprite
   //
	{: A rectangular area, NOT perspective projected.<p>
      (x, y) coordinates map directly to the viewport (in pixels) and refer
      the center of the area.<br>
      The coordinate system is that of an equivalent TCanvas, ie. top-left
      point is the origin (0, 0).<p>
      The z component is ignored and Z-Buffer is disabled when rendering.<p>
      <b>Using TGLHUDSprite in 2D only scenes :</b><br>
      The most convenient way to use a TGLHUDSprite as a simple 2D sprite with
      blending capabilities (transparency or additive), is to set the texture
      mode to tmModulate, in FrontProperties, to use the Emission color to
      control coloring/intensity, and finally use the Diffuse color's alpha
      to control transparency (while setting the other RGB components to 0).<br>
      You can also control aplha-blending by defining a <1 value in the sprite's
      AlphaChannel field. This provides you with hardware accelerated,
      alpha-blended blitting.<p>
      Note : since TGLHUDSprite works in absolute coordinates, TGLProxyObject
      can't be used to duplicate an hud sprite. }
	TGLHUDSprite = class (TGLSprite)
	   private
			{ Private Declarations }
         FXTiles, FYTiles : Integer;

		protected
			{ Protected Declarations }
         procedure SetXTiles(const val : Integer);
         procedure SetYTiles(const val : Integer);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

	   published
	      { Published Declarations }
         property XTiles : Integer read FXTiles write SetXTiles default 1;
         property YTiles : Integer read FYTiles write SetYTiles default 1;
   end;

   // TGLHUDText
   //
   {: A 2D text displayed and positionned in 2D coordinates.<p>
      The HUDText uses a character font defined and stored by a TGLBitmapFont
      component. The text can be scaled and rotated (2D), the layout and
      alignment can also be controled. }
	TGLHUDText = class (TGLImmaterialSceneObject)
	   private
	      { Private Declarations }
         FFont : TGLFont;
         FText : String;
         FRotation : Single;
         FAlignment : TAlignment;
         FLayout : TGLTextLayout;
         FModulateColor : TGLColor;

	   protected
	      { Protected Declarations }
         procedure SetFont(const val : TGLFont);
         procedure SetText(const val : String);
         procedure SetRotation(const val : Single);
         procedure SetAlignment(const val : TAlignment);
         procedure SetLayout(const val : TGLTextLayout);
         procedure SetModulateColor(const val : TGLColor);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

		public
			{ Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

	   published
	      { Published Declarations }
         {: Refers the bitmap font to use.<p>
            The referred bitmap font component stores and allows access to
            individual character bitmaps. }
         property Font : TGLFont read FFont write SetFont;
         {: Text to render.<p>
            Be aware that only the characters available in the bitmap font will
            be rendered. CR LF sequences are allowed. }
         property Text : String read FText write SetText;
         {: Rotation angle in degrees (2d). }
         property Rotation : Single read FRotation write SetRotation;
         {: Controls the text alignment (horizontal).<p>
            Possible values : taLeftJustify, taRightJustify, taCenter }
         property Alignment : TAlignment read FAlignment write SetAlignment;
         {: Controls the text layout (vertical).<p>
            Possible values : tlTop, tlCenter, tlBottom }
         property Layout : TGLTextLayout read FLayout write SetLayout;
         {: Color modulation, can be used for fade in/out too.}
         property ModulateColor : TGLColor read FModulateColor write SetModulateColor;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLGraphics, XOpenGL, GLState;

// ------------------
// ------------------ TGLHUDSprite ------------------
// ------------------

// Create
//
constructor TGLHUDSprite.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   Width:=16;
   Height:=16;
   FXTiles:=1;
   FYTiles:=1;
end;

// SetXTiles
//
procedure TGLHUDSprite.SetXTiles(const val : Integer);
begin
   if val<>FXTiles then begin
      FXTiles:=val;
      StructureChanged;
   end;
end;

// SetYTiles
//
procedure TGLHUDSprite.SetYTiles(const val : Integer);
begin
   if val<>FYTiles then begin
      FYTiles:=val;
      StructureChanged;
   end;
end;

// DoRender
//
procedure TGLHUDSprite.DoRender(var rci : TRenderContextInfo;
                              renderSelf, renderChildren : Boolean);
var
	vx, vy, vx1, vy1, f : Single;
  u0, v0, u1, v1 : Single;
begin
   if rci.ignoreMaterials then Exit;
  	Material.Apply(rci);

   u0:=UVLeft * FXTiles;
   u1:=UVRight * FXTiles;
   v0:=UVTop * FYTiles;
   v1:=UVBottom * FYTiles;

   repeat
      if AlphaChannel<>1 then begin
         if rci.lightingDisabledCounter>0 then begin
            with Material.FrontProperties.Diffuse do
               glColor4f(Red, Green, Blue, AlphaChannel)
         end else rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
      end;
      // Prepare matrices
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
      if rci.renderDPI=96 then
         f:=1
      else f:=rci.renderDPI/96;
      glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
      glTranslatef(f*Position.X-rci.viewPortSize.cx*0.5,
                   rci.viewPortSize.cy*0.5-f*Position.Y, Position.Z);
      if Rotation<>0 then
         glRotatef(Rotation, 0, 0, 1);
      glTranslatef(-OriginX, OriginY, 0.0);
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      glDepthMask(False);
      // precalc coordinates
      vx:=-Width*0.5*f;    vx1:=vx+Width*f;
      vy:=+Height*0.5*f;   vy1:=vy-Height*f;
      // issue quad
      glBegin(GL_QUADS);
         glNormal3fv(@YVector);
         xglTexCoord2f(u0, v0); glVertex2f( vx, vy1);
         xglTexCoord2f(u1, v0); glVertex2f(vx1, vy1);
         xglTexCoord2f(u1, v1); glVertex2f(vx1,  vy);
         xglTexCoord2f(u0, v1); glVertex2f( vx,  vy);
      glEnd;
      // restore state
      glDepthMask(True);
      glPopAttrib;
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
   until not Material.UnApply(rci);
   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;

// ------------------
// ------------------ TGLHUDText ------------------
// ------------------

// Create
//
constructor TGLHUDText.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osNoVisibilityCulling];
   FModulateColor:=TGLColor.CreateInitialized(Self, clrWhite);
end;

// Destroy
//
destructor TGLHUDText.Destroy;
begin
   FModulateColor.Free;
   FFont:=nil;
   inherited;
end;

// Notification
//
procedure TGLHUDText.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FFont) then
      FFont:=nil;
   inherited;
end;

// SetBitmapFont
//
procedure TGLHUDText.SetFont(const val : TGLFont);
begin
   if val<>FFont then begin
      if Assigned(FFont) then
         FFont.UnRegisterUser(Self);
      FFont:=val;
      if Assigned(FFont) then begin
         FFont.RegisterUser(Self);
         FFont.FreeNotification(Self);
      end;
      StructureChanged;
   end;
end;

// SetText
//
procedure TGLHUDText.SetText(const val : String);
begin
   FText:=val;
   StructureChanged;
end;

// SetRotation
//
procedure TGLHUDText.SetRotation(const val : Single);
begin
   FRotation:=val;
   StructureChanged;
end;

// SetAlignment
//
procedure TGLHUDText.SetAlignment(const val : TAlignment);
begin
   FAlignment:=val;
   StructureChanged;
end;

// SetLayout
//
procedure TGLHUDText.SetLayout(const val : TGLTextLayout);
begin
   FLayout:=val;
   StructureChanged;
end;

// SetModulateColor
//
procedure TGLHUDText.SetModulateColor(const val: TGLColor);
begin
   FModulateColor.Assign(val);
end;

// DoRender
//
procedure TGLHUDText.DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean);
var
   f : Single;
begin
   if Assigned(FFont) and (Text<>'') then begin
      rci.GLStates.SetGLPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      // Prepare matrices
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
      if rci.renderDPI=96 then
         f:=1
      else f:=rci.renderDPI/96;
      glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
      glTranslatef(Position.X*f-rci.viewPortSize.cx/2,
                   rci.viewPortSize.cy/2-Position.Y*f, Position.Z);
      if FRotation<>0 then
         glRotatef(FRotation, 0, 0, 1);
      glScalef(Scale.DirectX*f, Scale.DirectY*f, 1);
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glPushAttrib(GL_ENABLE_BIT);
      glDisable(GL_DEPTH_TEST);
      // render text
      FFont.RenderString(rci, Text, FAlignment, FLayout, FModulateColor.Color);
      // restore state
      glPopAttrib;
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;
   end;
   if Count>0 then
      Self.RenderChildren(0, Count-1, rci);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TGLHUDText, TGLHUDSprite]);

end.

