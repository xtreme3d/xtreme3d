{: GLUserShader<p>

   A shader that passes control of the DoApply and DoUnApply
   methods through published events. This component is 
   designed to make it a little easier to implement a 
   customized shader. Be sure to keep the shader balanced
   by returning the OpenGL state to how you found it.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/08/03 - SG - Creation
   </ul></font>
}
unit GLUserShader;

interface

uses
  Classes, GLTexture;

type
  TOnDoApplyEvent = procedure (Sender : TObject; var rci : TRenderContextInfo) of Object;
  TOnDoUnApplyEvent = procedure (Sender : TObject; Pass:Integer; var rci : TRenderContextInfo; var Continue : Boolean) of Object;
  
  TGLUserShader = class(TGLShader)
    private
      FPass : Integer;
      FOnDoApply : TOnDoApplyEvent;
      FOnDoUnApply : TOnDoUnApplyEvent;
    protected
      procedure DoApply(var rci : TRenderContextInfo; Sender : TObject); override;
      function DoUnApply(var rci : TRenderContextInfo) : Boolean; override;
    published
      property OnDoApply : TOnDoApplyEvent read FOnDoApply write FOnDoApply;
      property OnDoUnApply : TOnDoUnApplyEvent read FOnDoUnApply write FOnDoUnApply;
      property ShaderStyle;
  end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLUserShader]);
end;

// ------------------
// ------------------ TGLUserShader ------------------
// ------------------

// DoApply
//
procedure TGLUserShader.DoApply(var rci: TRenderContextInfo; Sender : TObject);
begin
  FPass:=1;
  if Assigned(FOnDoApply) and (not (csDesigning in ComponentState)) then
    FOnDoApply(Self,rci);
end;

// DoUnApply
//
function TGLUserShader.DoUnApply(var rci: TRenderContextInfo): Boolean;
begin
  Result:=False;
  if Assigned(FOnDoUnApply) and (not (csDesigning in ComponentState)) then begin
    FOnDoUnApply(Self,FPass,rci,Result);
    Inc(FPass);
  end;
end;

end.
