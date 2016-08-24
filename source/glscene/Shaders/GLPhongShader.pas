{: GLPhongShader<p>

   An ARBvp1.0 + ARBfp1.0 shader that implements phong shading.<p>

   <b>History : </b><font size=-1><ul>
      <li>11/10/04 - SG - Creation.
   </ul></font>
}
unit GLPhongShader;

interface

uses
   Classes, SysUtils, GLTexture, ARBProgram, VectorGeometry, VectorLists,
   OpenGL1x;
   
type
   TGLPhongShader = class(TGLShader)
      private
         { Private Declarations }
         FVPHandle : Cardinal;
         FFPHandle : Cardinal;
         FLightIDs : TIntegerList;
         FLightsEnabled : Integer;
         FDesignTimeEnabled : Boolean;
         FAmbientPass : Boolean;

      protected
         { Protected Declarations }
         function GenerateVertexProgram : String;
         function GenerateFragmentProgram : String;
         procedure DestroyARBPrograms;

         procedure SetDesignTimeEnabled(const Value : Boolean);

         procedure DoLightPass(lightID : Cardinal);
         procedure DoAmbientPass;

      public
         { Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

      published
         { Published Declarations }
         property DesignTimeEnabled : Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;

   end;

procedure Register;

implementation

procedure Register;
begin
   RegisterComponents('GLScene Shaders', [TGLPhongShader]);
end;

// Create
//
constructor TGLPhongShader.Create(AOwner : TComponent);
begin
   inherited;
   FLightIDs:=TIntegerList.Create;
   ShaderStyle:=ssLowLevel;
end;

// Destroy
//
destructor TGLPhongShader.Destroy;
begin
   DestroyARBPrograms;
   FLightIDs.Free;
   inherited;
end;

// GenerateVertexProgram
//
function TGLPhongShader.GenerateVertexProgram : String;
var
   VP : TStringList;
begin
   VP:=TStringList.Create;

   VP.Add('!!ARBvp1.0');
   VP.Add('OPTION ARB_position_invariant;');

   VP.Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
   VP.Add('PARAM mvit[4] = { state.matrix.modelview.invtrans };');
   VP.Add('PARAM lightPos = program.local[0];');
   VP.Add('TEMP light, normal, eye;');

   VP.Add('   ADD eye, mvit[3], -vertex.position;');
   VP.Add('   MOV eye.w, 0.0;');

   VP.Add('   DP4 light.x, mvinv[0], lightPos;');
   VP.Add('   DP4 light.y, mvinv[1], lightPos;');
   VP.Add('   DP4 light.z, mvinv[2], lightPos;');
   VP.Add('   ADD light, light, -vertex.position;');
   VP.Add('   MOV light.w, 0.0;');

   VP.Add('   MOV result.texcoord[0], vertex.normal;');
   VP.Add('   MOV result.texcoord[1], light;');
   VP.Add('   MOV result.texcoord[2], eye;');

   VP.Add('END');
   
   Result:=VP.Text;
   VP.Free;
end;

// GenerateFragmentProgram
//
function TGLPhongShader.GenerateFragmentProgram : String;
var
   FP : TStringList;
begin
   FP:=TStringList.Create;

   FP.Add('!!ARBfp1.0');

   FP.Add('PARAM lightDiff = program.local[0];');
   FP.Add('PARAM lightSpec = program.local[1];');
   FP.Add('PARAM materialDiff = state.material.diffuse;');
   FP.Add('PARAM materialSpec = state.material.specular;');
   FP.Add('PARAM shininess = state.material.shininess;');
   FP.Add('TEMP temp, light, normal, eye, R, diff, spec;');

   FP.Add('   DP3 temp, fragment.texcoord[0], fragment.texcoord[0];');
   FP.Add('   RSQ temp, temp.x;');
   FP.Add('   MUL normal, temp.x, fragment.texcoord[0];');
   FP.Add('   DP3 temp, fragment.texcoord[1], fragment.texcoord[1];');
   FP.Add('   RSQ temp, temp.x;');
   FP.Add('   MUL light, temp.x, fragment.texcoord[1];');
   FP.Add('   DP3 temp, fragment.texcoord[2], fragment.texcoord[2];');
   FP.Add('   RSQ temp, temp.x;');
   FP.Add('   MUL eye, temp.x, fragment.texcoord[2];');

   FP.Add('   DP3_SAT diff, normal, light;');
   FP.Add('   MUL diff, diff, lightDiff;');
   FP.Add('   MUL diff, diff, materialDiff;');

   FP.Add('   DP3 R, normal, light;');
   FP.Add('   MUL R, R.x, normal;');
   FP.Add('   MUL R, 2.0, R;');
   FP.Add('   ADD R, R, -light;');

   FP.Add('   DP3_SAT spec, R, eye;');
   FP.Add('   POW spec, spec.x, shininess.x;');
   FP.Add('   MUL spec, spec, lightDiff;');
   FP.Add('   MUL spec, spec, materialDiff;');

   FP.Add('   ADD_SAT result.color, diff, spec;');
   FP.Add('   MOV result.color.w, 1.0;');

   FP.Add('END');

   Result:=FP.Text;
   FP.Free;
end;

// DoLightPass
//
procedure TGLPhongShader.DoLightPass(lightID : Cardinal);
var
   light : TVector;
begin
   glEnable(GL_VERTEX_PROGRAM_ARB);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVPHandle);
   glGetLightfv(lightID, GL_POSITION, @light[0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 0, @light[0]);

   glEnable(GL_FRAGMENT_PROGRAM_ARB);
   glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, FFPHandle);
   glGetLightfv(lightID, GL_DIFFUSE, @light[0]);
   glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 0, @light[0]);
   glGetLightfv(lightID, GL_SPECULAR, @light[0]);
   glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 1, @light[0]);
end;

// DoAmbientPass
//
procedure TGLPhongShader.DoAmbientPass;
var
   ambient, materialAmbient : TVector;
begin
   glDisable(GL_LIGHTING);
   glActiveTextureARB(GL_TEXTURE0_ARB);
   glDisable(GL_TEXTURE_2D);
   glActiveTextureARB(GL_TEXTURE1_ARB);
   glDisable(GL_TEXTURE_2D);
   glActiveTextureARB(GL_TEXTURE0_ARB);

   glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
   glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
   ambient[0]:=ambient[0]*materialAmbient[0];
   ambient[1]:=ambient[1]*materialAmbient[1];
   ambient[2]:=ambient[2]*materialAmbient[2];
   glColor3fv(@ambient);

   FAmbientPass:=True;
end;

// DoApply
//
procedure TGLPhongShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
var
   maxLights, maxTextures, i : Integer;
   lightEnabled : GLboolean;
   success : Boolean;
   str : String;
begin
   if (csDesigning in ComponentState) and not DesignTimeEnabled then exit;
   if not Enabled then exit;

   glGetIntegerv(GL_MAX_LIGHTS, @maxLights);
   glGetIntegerv(GL_MAX_TEXTURE_UNITS_ARB, @maxTextures);

   success:=False;
   try
      if not GL_ARB_multitexture then
         raise Exception.Create('This shader requires GL_ARB_multitexture.');
      if  maxTextures<3 then
         raise Exception.Create('This shader requires 3 or more texture units.');

      if FVPHandle = 0 then begin
         str:=GenerateVertexProgram;
         LoadARBProgram(GL_VERTEX_PROGRAM_ARB, str, FVPHandle);
      end;

      if FFPHandle = 0 then begin
         str:=GenerateFragmentProgram;
         LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, str, FFPHandle);
      end;

      success:=True;

   finally
      if not success then begin
         Enabled:=False;
         DesignTimeEnabled:=False;
      end;
   end;

   glPushAttrib(GL_ENABLE_BIT or
                GL_TEXTURE_BIT or
                GL_DEPTH_BUFFER_BIT or
                GL_COLOR_BUFFER_BIT);

   FLightIDs.Clear;
   for i:=0 to maxLights-1 do begin
      glGetBooleanv(GL_LIGHT0+i, @lightEnabled);
      if lightEnabled then
         FLightIDs.Add(GL_LIGHT0+i);
   end;
   FLightsEnabled:=FLightIDs.Count;

   FAmbientPass:=False;

   if FLightIDs.Count>0 then begin

      glDepthFunc(GL_LEQUAL);
      glDisable(GL_BLEND);
      DoLightPass(FLightIDs[0]);
      FLightIDs.Delete(0);
      
   end else begin

      DoAmbientPass;

   end;
end;

// DoUnApply
//
function TGLPhongShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
begin
   Result:=False;
   if (csDesigning in ComponentState) and not DesignTimeEnabled then exit;
   if not Enabled then exit;

   if FLightIDs.Count>0 then begin

      glDepthFunc(GL_LEQUAL);
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);

      DoLightPass(FLightIDs[0]);
      FLightIDs.Delete(0);
      Result:=True;
      Exit;

   end else if not FAmbientPass then begin

      glDisable(GL_VERTEX_PROGRAM_ARB);
      glDisable(GL_FRAGMENT_PROGRAM_ARB);

      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);
      DoAmbientPass;

      Result:=True;
      Exit;

   end;

   glPopAttrib;
end;

// DestroyARBPrograms
//
procedure TGLPhongShader.DestroyARBPrograms;
begin
   if FVPHandle<>0 then
      glDeleteProgramsARB(1, @FVPHandle);
   FVPHandle:=0;
   if FFPHandle<>0 then
      glDeleteProgramsARB(1, @FFPHandle);
   FFPHandle:=0;
end;

// SetDesignTimeEnabled
//
procedure TGLPhongShader.SetDesignTimeEnabled(const Value: Boolean);
begin
   if Value<>FDesignTimeEnabled then begin
      FDesignTimeEnabled:=Value;
      NotifyChange(Self);
   end;
end;

end.