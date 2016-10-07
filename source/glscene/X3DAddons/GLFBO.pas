unit GLFBO;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils;

const
  // OpenGL 1.4 required 
  GL_CLAMP_TO_BORDER = $812D;
  GL_TEXTURE_COMPARE_MODE = $884C;
  GL_TEXTURE_COMPARE_FUNC = $884D;
  GL_COMPARE_R_TO_TEXTURE = $884E;
  GL_DEPTH_TEXTURE_MODE = $884B;

type   

  TGLFBO = class(TPersistent)
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FInitialized: Boolean;
        FFramebuffer: GLuint;
        FStencilRenderbuffer: GLuint;
        FDepthTextureHandle: GLuint;
        FColorTextureHandle: GLuint;
        
        FTexture: TGLTexture;
        FMainBuffer: TGLSceneBuffer;
        FWidth: Integer;
        FHeight: Integer;
        FCamera: TGLCamera;
        FRenderObject: TGLBaseSceneObject;
        FProjectionSize: Single;
        procedure SetTexture(texture: TGLTexture);
        procedure DoInitialize;
    public
        { Public Declarations }
        constructor Create;
        destructor Destroy; override;
        procedure Render;
        property Texture: TGLTexture read FTexture write SetTexture;
        property MainBuffer: TGLSceneBuffer read FMainBuffer write FMainBuffer;
        property Width: Integer read FWidth write FWidth;
        property Height: Integer read FHeight write FHeight;
        property DepthTextureHandle: GLuint read FDepthTextureHandle write FDepthTextureHandle;
        property ColorTextureHandle: GLuint read FColorTextureHandle write FColorTextureHandle;
        property Camera: TGLCamera read FCamera write FCamera;
        property RenderObject: TGLBaseSceneObject read FRenderObject write FRenderObject;
  end;

implementation

procedure TGLFBO.SetTexture(texture: TGLTexture);
begin
  FTexture := texture;
end;

procedure TGLFBO.DoInitialize;
begin
  FInitialized := True;

  // Depth texture
  glGenTextures(1, @FDepthTextureHandle);
  glBindTexture(GL_TEXTURE_2D, FDepthTextureHandle);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  //glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, @FDepthBorderColor.color);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_COMPARE_R_TO_TEXTURE);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC, GL_LEQUAL);
  //glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE, GL_NONE);
  //glTexParameteri(GL_TEXTURE_2D, GL_DEPTH_TEXTURE_MODE, GL_INTENSITY);
  //glTexImage2D(GL_TEXTURE_2D, 0,
  //    GL_DEPTH_COMPONENT, Width, Height, 0,
  //    GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nil);
  glTexImage2D(GL_TEXTURE_2D, 0,
      GL_DEPTH24_STENCIL8, Width, Height, 0,
      GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, nil);
  glBindTexture(GL_TEXTURE_2D, 0);
  
  // Color texture
  glGenTextures(1, @FColorTextureHandle);
  glBindTexture(GL_TEXTURE_2D, FColorTextureHandle);
  glTexImage2D(GL_TEXTURE_2D, 0, 4, Width, Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glBindTexture(GL_TEXTURE_2D, 0);

  // Create FBO
  glGenFramebuffers(1, @FFramebuffer);
  //glGenRenderbuffers(1, @FStencilRenderbuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FColorTextureHandle, 0);         
  //glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, FDepthTextureHandle, 0);
  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, FDepthTextureHandle, 0);

  //glBindRenderbuffer(GL_RENDERBUFFER, FStencilRenderbuffer);
  //glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, Width, Height);
  //glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, FStencilRenderbuffer);

  glBindFramebuffer(GL_FRAMEBUFFER, 0);
    
end;

constructor TGLFBO.Create;
begin
  inherited Create;
  Width := 256;
  Height := 256;
  FInitialized := false;
  FProjectionSize := 20.0;
end;

destructor TGLFBO.Destroy;
begin
  if (glIsTexture(FDepthTextureHandle)) then
    glDeleteTextures(1, @FDepthTextureHandle);
  if (glIsTexture(FColorTextureHandle)) then
    glDeleteTextures(1, @FColorTextureHandle);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
  glDeleteFramebuffers(1, @FFramebuffer);
  inherited Destroy;
end;

procedure TGLFBO.Render;
var
   oldWidth, oldHeight: Integer;
   projMat, mvMat, mvpMat, tsMat, tmpMat, invCamMat: TMatrix;
   oldCamera: TGLCamera;
begin
   if not Assigned(FCamera) then
     Exit;

   oldWidth := MainBuffer.Width;
   oldHeight := MainBuffer.Height;
   oldCamera := MainBuffer.Camera;
   MainBuffer.Resize(FWidth, FHeight);
   MainBuffer.SetViewPort(0, 0, oldWidth, oldHeight);
   MainBuffer.Camera := FCamera;

   MainBuffer.RenderingContext.Activate;
   if not FInitialized then
     DoInitialize;
   glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);

   MainBuffer.SimpleRender2(FRenderObject, False, False, True, False);

   glBindFramebuffer(GL_FRAMEBUFFER, 0);

   MainBuffer.RenderingContext.Deactivate;
   
   MainBuffer.Resize(oldWidth, oldHeight);
   MainBuffer.Camera := oldCamera;
end;

end.
