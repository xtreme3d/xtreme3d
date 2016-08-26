{: GLProjectedTextures<p>

   Implements projected textures through a GLScene object.

   <b>History : </b><font size=-1><ul>
      <li>15/06/05 - Mathx - Added the Style property and inverse rendering
      <li>07/05/05 - Mathx - Support for tmBlend textures (by Ruben Javier)
      <li>01/10/04 - SG - Initial (by Matheus Degiovani)
   </ul></font>
}
unit GLProjectedTextures;

interface

uses
   Classes, GLScene, GLTexture, OpenGL1x, VectorGeometry, xopengl;

type
   {: Possible styles of texture projection. Possible values:<ul>
      <li>ptsOriginal: Original projection method (first pass,
          is default scene render, second pass is texture
          projection).
      <li>ptsInverse: Inverse projection method (first pass
          is texture projection, sencond pass is regular scene
          render). This method is useful if you want to simulate
          lighting only through projected textures (the textures
          of the scene are "masked" into the white areas of
          the projection textures).
      </ul> }
   TGLProjectedTexturesStyle = (ptsOriginal, ptsInverse);

   TGLProjectedTextures = class;

   // TGLTextureEmmiter
   //
   {: A projected texture emmiter.<p>
      It's material property will be used as the projected texture.
      Can be places anywhere in the scene. }
   TGLTextureEmitter = class(TGLSceneObject)
      private
         { Private Declarations }
         FFOVy: single;
         FAspect: single;

      protected
         { Protected Declarations }
         {: Sets up the base texture matrix for this emitter<p>
            Should be called whenever a change on its properties is made.}
         procedure SetupTexMatrix;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;

      published
         { Published Declarations }
         {: Indicates the field-of-view of the projection frustum.}
         property FOVy: single read FFOVy write FFOVy;

         {: x/y ratio. For no distortion, this should be set to
            texture.width/texture.height.}
         property Aspect: single read FAspect write FAspect;
   end;

   // TGLTextureEmitterItem
   //
   {: Specifies an item on the TGLTextureEmitters collection. }
   TGLTextureEmitterItem = class(TCollectionItem)
      private
         { Private Declarations }
         FEmitter : TGLTextureEmitter;

      protected
         { Protected Declarations }
         procedure SetEmitter(const val : TGLTextureEmitter);
         procedure RemoveNotification(aComponent : TComponent);
         function GetDisplayName : String; override;

      public
         { Public Declarations }
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;

      published
         { Published Declarations }
         property Emitter: TGLTextureEmitter read FEmitter write SetEmitter;

   end;

   // TGLTextureEmitters
   //
   {: Collection of TGLTextureEmitter. }
   TGLTextureEmitters = class (TCollection)
      private
         { Private Declarations }
         FOwner : TGLProjectedTextures;

      protected
         { Protected Declarations }
         function GetOwner : TPersistent; override;
         function GetItems(index : Integer) : TGLTextureEmitterItem;
         procedure RemoveNotification(aComponent : TComponent);

      public
         { Public Declarations }
         procedure AddEmitter(texEmitter: TGLTextureEmitter);

         property Items[index : Integer] : TGLTextureEmitterItem read GetItems; default;

   end;

   // TGLProjectedTexture
   //
   {: Projected Textures Manager.<p>
      Specifies active texture Emitters (whose texture will be projected)
      and receivers (children of this object). }
   TGLProjectedTextures = class(TGLImmaterialSceneObject)
      private
         { Private Declarations }
         FEmitters: TGLTextureEmitters;
         FStyle: TGLProjectedTexturesStyle;

         procedure LoseTexMatrix;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure DoRender(var rci : TRenderContextInfo;
                            renderSelf, renderChildren : Boolean); override;

      published
         { Published Declarations }

         {: List of texture emitters. }
         property Emitters: TGLTextureEmitters read FEmitters write FEmitters;

         {: Indicates the style of the projected textures. }
         property Style: TGLProjectedTexturesStyle read FStyle write FStyle;
     end;


//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

// ------------------
// ------------------ TGLTextureEmitter ------------------
// ------------------

// Create
//
constructor TGLTextureEmitter.Create(aOwner: TComponent);
begin
   inherited Create(aOwner);
   setupTexMatrix;
   FFOVy:= 90;
   FAspect:= 1;
end;

// SetupTexMatrix
//
procedure TGLTextureEmitter.SetupTexMatrix;
begin
   glMatrixMode(GL_TEXTURE);
   glLoadIdentity;

   // First scale and bias into [0..1] range.
   glTranslatef(0.5, 0.5, 0);
   glScalef(0.5, 0.5, 1);

   // Then set the projector's "perspective" (i.e. the "spotlight cone"):.
   gluPerspective(FFOVy, FAspect, 0.1, 1);
     
   glMultMatrixf(PGLFloat(invAbsoluteMatrixAsAddress));

   glMatrixMode(GL_MODELVIEW);
end;


// ------------------
// ------------------ TGLTextureEmitterItem ------------------
// ------------------

// Create
//
constructor TGLTextureEmitterItem.Create(Collection: TCollection);
begin
   inherited Create(Collection);
end;

// Assign
//
procedure TGLTextureEmitterItem.Assign(Source: TPersistent);
begin
   if Source is TGLTextureEmitterItem then begin
      FEmitter:=TGLTextureEmitterItem(Source).FEmitter;
      TGLProjectedTextures(TGLTextureEmitters(Collection).GetOwner).StructureChanged;
   end;
   inherited;
end;

// SetCaster
//
procedure TGLTextureEmitterItem.SetEmitter(const val : TGLTextureEmitter);
begin
   if FEmitter<>val then begin
      FEmitter:=val;
      TGLProjectedTextures(TGLTextureEmitters(Collection).GetOwner).StructureChanged;
   end;
end;

// RemoveNotification
//
procedure TGLTextureEmitterItem.RemoveNotification(aComponent : TComponent);
begin
   if aComponent=FEmitter then
      FEmitter:=nil;
end;

// GetDisplayName
//
function TGLTextureEmitterItem.GetDisplayName : String;
begin
   if Assigned(FEmitter) then begin
      Result:= '[TexEmitter] ' + FEmitter.Name;
   end else Result:='nil';
end;


// ------------------
// ------------------ TGLTextureEmitters ------------------
// ------------------

// GetOwner
//
function TGLTextureEmitters.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// GetItems
//
function TGLTextureEmitters.GetItems(index : Integer) : TGLTextureEmitterItem;
begin
   Result:=TGLTextureEmitterItem(inherited Items[index]);
end;

// RemoveNotification
//
procedure TGLTextureEmitters.RemoveNotification(aComponent : TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].RemoveNotification(aComponent);
end;

// AddEmitter
//
procedure TGLTextureEmitters.AddEmitter(texEmitter: TGLTextureEmitter);
var
item: TGLTextureEmitterItem;
begin
     item:= TGLTextureEmitterItem(self.Add);
     item.Emitter:= texEmitter;
end;

// ------------------
// ------------------ TGLProjectedTextures ------------------
// ------------------

// Create
//
constructor TGLProjectedTextures.Create(AOwner: TComponent);
begin
   inherited Create(aOWner);
   FEmitters:= TGLTextureEmitters.Create(TGLTextureEmitterItem);
   FEmitters.FOwner:= self;
end;

// Destroy
//
destructor TGLProjectedTextures.Destroy;
begin
   FEmitters.Free;
   inherited destroy;
end;

// LoseTexMatrix
//
procedure TGLProjectedTextures.LoseTexMatrix;
begin
   glBlendFunc(GL_ONE, GL_ZERO);
   glDisable(GL_TEXTURE_GEN_S);
   glDisable(GL_TEXTURE_GEN_T);
   glDisable(GL_TEXTURE_GEN_R);
   glDisable(GL_TEXTURE_GEN_Q);

   glMatrixMode(GL_TEXTURE);
   glLoadIdentity;
   glMatrixMode(GL_MODELVIEW);
end;

// DoRender
//
procedure TGLProjectedTextures.DoRender(var rci: TRenderContextInfo;
renderSelf, renderChildren: boolean);
const
   PS: array [0..3] of GLfloat = (1, 0, 0, 0);
   PT: array [0..3] of GLfloat = (0, 1, 0, 0);
   PR: array [0..3] of GLfloat = (0, 0, 1, 0);
   PQ: array [0..3] of GLfloat = (0, 0, 0, 1);
var
   i: integer;
   emitter: TGLTextureEmitter;
begin
   if not (renderSelf or renderChildren) then Exit;
   if (csDesigning in ComponentState) then begin
      inherited;
      Exit;
   end;

   //First pass of original style: render regular scene
   if Style = ptsOriginal then
      self.RenderChildren(0, Count-1, rci);

   glPushAttrib(GL_ALL_ATTRIB_BITS);

   //generate planes
   glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
   glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
   glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
   glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);

   glTexGenfv(GL_S, GL_EYE_PLANE, @PS);
   glTexGenfv(GL_T, GL_EYE_PLANE, @PT);
   glTexGenfv(GL_R, GL_EYE_PLANE, @PR);
   glTexGenfv(GL_Q, GL_EYE_PLANE, @PQ);

   //options
   glDisable(GL_LIGHTING);
   glDepthFunc(GL_LEQUAL);
   glEnable(GL_BLEND);
   glEnable(GL_TEXTURE_GEN_S);
   glEnable(GL_TEXTURE_GEN_T);
   glEnable(GL_TEXTURE_GEN_R);
   glEnable(GL_TEXTURE_GEN_Q);

   //second pass (original) first pass (inverse): for each emiter,
   //render projecting the texture summing all emitters
   for i:= 0 to Emitters.Count -1 do begin
       emitter:= Emitters[i].Emitter;
       if not assigned(emitter) then continue;
       if not emitter.Visible then continue;

       if Style = ptsOriginal then begin
          //on the original style, render blending the textures
          If emitter.Material.Texture.TextureMode <> tmBlend then
             glBlendFunc(GL_DST_COLOR,  GL_ONE)
          Else
             glBlendFunc(GL_DST_COLOR,  GL_ZERO);
       end else begin
          //on inverse style: the first texture projector should
          //be a regular rendering (i.e. no blending). All others
          //are "added" together creating an "illumination mask"
          if i = 0 then
             glBlendFunc(GL_ONE,  GL_ZERO)
          else
             glBlendFunc(GL_ONE,  GL_ONE);
       end;

       emitter.Material.Apply(rci);

       //get this emitter's tex matrix
       emitter.SetupTexMatrix;

       repeat
          rci.ignoreMaterials:= true;
          Self.RenderChildren(0, Count-1, rci);
          rci.ignoreMaterials:= false;
       until not emitter.Material.UnApply(rci);
   end;

   LoseTexMatrix;
   glPopAttrib;

   //second pass (inverse): render regular scene, blending it
   //with the "mask"
   if Style = ptsInverse then begin
      glPushAttrib(GL_ALL_ATTRIB_BITS);

      glEnable(GL_BLEND);
      glDepthFunc(GL_LEQUAL);

      glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);

      //second pass: render everything, blending with what is
      //already there
      rci.ignoreBlendingRequests:= true;
      self.RenderChildren(0, Count-1, rci);
      rci.ignoreBlendingRequests:= false;

      glPopAttrib;
   end;
end;

initialization

   RegisterClasses([TGLTextureEmitter, TGLProjectedTextures]);

end.