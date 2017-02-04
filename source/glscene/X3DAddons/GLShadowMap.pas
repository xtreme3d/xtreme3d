unit GLShadowMap;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils, GLFBO;

const
  // OpenGL 1.4 required 
  GL_CLAMP_TO_BORDER = $812D;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;
  GL_DEPTH_TEXTURE_MODE = $884B;

type

  TGLShadowMap = class(TComponent)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FInitialized: Boolean;
        FFramebuffer: GLuint;
        FDepthRenderBuffer: GLuint;
        FDepthTextureHandle: GLuint;
        FTexture: TGLTexture;
        FMainBuffer: TGLSceneBuffer;
        FWidth: Integer;
        FHeight: Integer;
        FDepthBorderColor: TGLColor;
        FShadowMatrix: TMatrix;
        FShadowCamera: TGLCamera;
        FCaster: TGLBaseSceneObject;
        FProjectionSize: Single;
        FZScale: Single;
        FZNear: Single;
        FZFar: Single;
        FFBO: TGLFBO;
        FMatLib: TGLMaterialLibrary;
        FOverrideMaterial: TGLLibMaterial;
        procedure SetTexture(texture: TGLTexture);
        procedure DoInitialize;
        procedure RenderToUserFBO;
        procedure RenderToOwnedFBO;
    public
        { Public Declarations }
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        procedure Render;
        property Texture: TGLTexture read FTexture write SetTexture;
        property MainBuffer: TGLSceneBuffer read FMainBuffer write FMainBuffer;
        property Width: Integer read FWidth write FWidth;
        property Height: Integer read FHeight write FHeight;
        property DepthTextureHandle: GLuint read FDepthTextureHandle write FDepthTextureHandle;
        property ShadowMatrix: TMatrix read FShadowMatrix;
        property ShadowCamera: TGLCamera read FShadowCamera write FShadowCamera;
        property Caster: TGLBaseSceneObject read FCaster write FCaster;
        property ProjectionSize: Single read FProjectionSize write FProjectionSize;
        property ZScale: Single read FZScale write FZScale;
        property ZNear: Single read FZNear write FZNear;
        property ZFar: Single read FZFar write FZFar;
        property FBO: TGLFBO read FFBO write FFBO;
        property OverrideMaterial: TGLLibMaterial read FOverrideMaterial;
  end;

implementation

procedure TGLShadowMap.SetTexture(texture: TGLTexture);
begin
  FTexture := texture;
end;

procedure TGLShadowMap.DoInitialize;
begin
  FInitialized := True;

  glGenTextures(1, @FDepthTextureHandle);
  glBindTexture(GL_TEXTURE_2D, FDepthTextureHandle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);

  glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, @FDepthBorderColor.color);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_R_TO_TEXTURE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_NONE);

	glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE, GL_INTENSITY);

  glTexImage2D(GL_TEXTURE_2D, 0,
      GL_DEPTH_COMPONENT, Width, Height, 0,
      GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nil);

  glBindTexture(GL_TEXTURE_2D, 0);

  glGenFramebuffers(1, @FFramebuffer);
	glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);
  glDrawBuffer(GL_NONE);
	glReadBuffer(GL_NONE);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, FDepthTextureHandle, 0);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

constructor TGLShadowMap.Create(AOwner: TComponent);
begin
  inherited;
  Width:=256;
  Height:=256;
  FInitialized := false;
  FDepthBorderColor := TGLColor.Create(Self);
  FDepthBorderColor.SetColor(1, 1, 1, 1);
  FProjectionSize := 20.0;
  FZScale := 1.0;
  FZNear := 0.0;
  FZFar := 100.0;
  FShadowMatrix := IdentityHmgMatrix;
  FFBO := nil;
  FMatLib := TGLMaterialLibrary.Create(self);
  FOverrideMaterial := FMatLib.AddTextureMaterial('mDefault', '', true);
  FOverrideMaterial.Material.CullFrontFaces := true;
  FOverrideMaterial.Material.FaceCulling := fcCull;
end;

destructor TGLShadowMap.Destroy;
begin
  if (glIsTexture(FDepthTextureHandle)) then
    glDeleteTextures(1, @FDepthTextureHandle);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDeleteFramebuffers(1, @FFramebuffer);
  FMatLib.Destroy;
  inherited Destroy;
end;

procedure TGLShadowMap.RenderToUserFBO;
var
   lightProjMat, lightModelViewMat, invViewMat: TMatrix;
   oldWidth, oldHeight: Integer;
   oldCamera: TGLCamera;
   oldFBOCamera: TGLCamera;
   oldFBOObject: TGLBaseSceneObject;
   oldOverrideMat: TGLLibMaterial;
begin
   if not Assigned(FShadowCamera) then
     Exit;

   oldWidth := MainBuffer.Width;
   oldHeight := MainBuffer.Height;
   oldCamera := MainBuffer.Camera;
   MainBuffer.Resize(FFBO.Width, FFBO.Height);
   MainBuffer.SetViewPort(0, 0, FFBO.Width, FFBO.Height);
   MainBuffer.Camera := FShadowCamera;

   MainBuffer.RenderingContext.Activate;
   if not FInitialized then
     DoInitialize;

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(-FProjectionSize, FProjectionSize,
           -FProjectionSize, FProjectionSize, FZNear, FZFar);
   glGetFloatv(GL_PROJECTION_MATRIX, @lightProjMat[0]);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   FShadowCamera.Apply;
   glGetFloatv(GL_MODELVIEW_MATRIX, @lightModelViewMat[0]);
   //glLoadIdentity();

   FFBO.Initialize();
   glBindFramebuffer(GL_FRAMEBUFFER, FFBO.Framebuffer);

   oldOverrideMat := MainBuffer.OverrideMaterial;
   if FFBO.OverrideMaterial <> nil then
   begin
     MainBuffer.OverrideMaterial := FFBO.OverrideMaterial;
   end
   else
   begin
     MainBuffer.OverrideMaterial := FOverrideMaterial;
   end;

   glClearColor(1, 1, 1, 1);
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glDisable(GL_LIGHTING);
   glDepthFunc(GL_LESS);
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
   glCullFace(GL_FRONT);

   MainBuffer.SimpleRender(FCaster);

   MainBuffer.OverrideMaterial := oldOverrideMat;

   glBindFramebuffer(GL_FRAMEBUFFER, 0);

   glCullFace(GL_BACK);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();
   oldCamera.Apply;
   glGetFloatv(GL_MODELVIEW_MATRIX, @invViewMat[0]);
   glPopMatrix();
   InvertMatrix(invViewMat);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();
   glTranslatef(0.5, 0.5, 0.5);
   glScalef(0.5, 0.5, 0.5);
   glMultMatrixf(@lightProjMat);
   glMultMatrixf(@lightModelViewMat);
   glMultMatrixf(@invViewMat);
   glScalef(FZScale, FZScale, FZScale);
   glGetFloatv(GL_MODELVIEW_MATRIX, @FShadowMatrix[0]);
   glPopMatrix();

   MainBuffer.RenderingContext.Deactivate;

   MainBuffer.Resize(oldWidth, oldHeight);
   MainBuffer.Camera := oldCamera;
end;

procedure TGLShadowMap.RenderToOwnedFBO;
var
   oldWidth, oldHeight: Integer;
   projMat, mvMat, mvpMat, tsMat, tmpMat, invCamMat: TMatrix;
   oldCamera: TGLCamera;
   oldOverrideMat: TGLLibMaterial;
begin
   if not Assigned(FShadowCamera) then
     Exit;

   oldWidth := MainBuffer.Width;
   oldHeight := MainBuffer.Height;
   oldCamera := MainBuffer.Camera;
   MainBuffer.Resize(FWidth, FHeight);
   MainBuffer.SetViewPort(0, 0, FWidth, FHeight);
   MainBuffer.Camera := FShadowCamera;

   MainBuffer.RenderingContext.Activate;
   if not FInitialized then
     DoInitialize;

   glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);

   oldOverrideMat := MainBuffer.OverrideMaterial;
   MainBuffer.OverrideMaterial := FOverrideMaterial;

   glClearColor(1, 1, 1, 1);
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glDisable(GL_LIGHTING);
   glDepthFunc(GL_LESS);
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
   glCullFace(GL_FRONT);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho(-FProjectionSize, FProjectionSize,
           -FProjectionSize, FProjectionSize, FZNear, FZFar);
   glGetFloatv(GL_PROJECTION_MATRIX, @projMat[0]);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   FShadowCamera.Apply;
   glGetFloatv(GL_MODELVIEW_MATRIX, @mvMat[0]);

   MainBuffer.SimpleRender(FCaster);

   MainBuffer.OverrideMaterial := oldOverrideMat;

   glBindFramebuffer(GL_FRAMEBUFFER, 0);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();
   oldCamera.Apply;
   glGetFloatv(GL_MODELVIEW_MATRIX, @invCamMat[0]);
   glPopMatrix();
   InvertMatrix(invCamMat);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix();
   glLoadIdentity();
   glTranslatef(0.5, 0.5, 0.5);
   glScalef(0.5, 0.5, 0.5);
   glMultMatrixf(@projMat);
   glMultMatrixf(@mvMat);
   glMultMatrixf(@invCamMat);
   glScalef(FZScale, FZScale, FZScale);
   glGetFloatv(GL_MODELVIEW_MATRIX, @FShadowMatrix[0]);
   glPopMatrix();

   glCullFace(GL_BACK);
   
   MainBuffer.RenderingContext.Deactivate;
   
   MainBuffer.Resize(oldWidth, oldHeight);
   MainBuffer.Camera := oldCamera;
end;

procedure TGLShadowMap.Render;
begin
   if FFBO <> nil then
     RenderToUserFBO()
   else
     RenderToOwnedFBO();
end;

end.
