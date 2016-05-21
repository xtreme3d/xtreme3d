unit GLShadowMap;

interface

uses
  Classes, Dialogs, VectorTypes, VectorGeometry,
  GLScene, GLTexture, OpenGL1x, GLUtils;

type
  TGLFBOViewer = class
    private
        { Private Declarations }
    protected
        { Protected Declarations }
        FInitialized: Boolean;
        FFramebuffer: GLuint;
        FDepthRenderBuffer: GLuint;
        FTexture: TGLTexture;
        FBuffer: TGLSceneBuffer;
        FWidth: Integer;
        FHeight: Integer;
        procedure SetTexture(texture: TGLTexture);
        procedure DoInitialize;
    public
        { Public Declarations }
        constructor Create;
        destructor Destroy; override;
        procedure Render(); 
        property Texture: TGLTexture read FTexture write SetTexture;
        property Buffer: TGLSceneBuffer read FBuffer write FBuffer;
        property Width: Integer read FWidth write FWidth;
        property Height: Integer read FHeight write FHeight;
  end;

implementation

procedure TGLFBOViewer.SetTexture(texture: TGLTexture);
begin
  FTexture := texture;
end;

procedure TGLFBOViewer.DoInitialize;
begin
  if FFramebuffer <> 0 then
    glDeleteFramebuffers(1, @FFramebuffer);
  glGenFramebuffers(1, @FFramebuffer);
  assert(FFramebuffer <> 0);
  FInitialized := True;

  glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);

  glGenRenderbuffers(1, @FDepthRenderBuffer);
  glBindRenderbuffer(GL_RENDERBUFFER, FDepthRenderBuffer);
  glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT, 256, 256);
  glBindRenderbuffer(GL_RENDERBUFFER, 0);

  glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, FTexture.Handle, 0);
  glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, FDepthRenderBuffer);
  glBindFramebuffer(GL_FRAMEBUFFER, 0);
end;

constructor TGLFBOViewer.Create;
begin
  inherited Create;
  Width:=256;
  Height:=256;
  FInitialized := false;
end;

destructor TGLFBOViewer.Destroy;
begin
  if FFramebuffer <> 0 then
  begin
    glDeleteRenderbuffers(1, @FDepthRenderBuffer);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);
    glDeleteFramebuffers(1, @FFramebuffer);
  end;
  inherited Destroy;
end;

procedure TGLFBOViewer.Render;
var
   oldWidth, oldHeight: Integer;
begin
   oldWidth := Buffer.Width;
   oldHeight := Buffer.Height;
   Buffer.Resize(Width, Height);
   Buffer.SetViewPort(0, 0, oldWidth, oldHeight);

   Buffer.RenderingContext.Activate;
   if not FInitialized then
     DoInitialize;
   glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffer);
   Buffer.Render(FBuffer.Camera.Scene.Objects, False);
   glBindFramebuffer(GL_FRAMEBUFFER, 0);
   Buffer.RenderingContext.Deactivate;
   
   Buffer.Resize(oldWidth, oldHeight);
end;

end.
