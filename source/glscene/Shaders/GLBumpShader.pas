{: GLBumpShader<p>

   A shader that applies bump mapping.<p>

   Notes:
   The normal map is expected to be the primary texture.<p>

   The secondary texture is used for the diffuse texture,
   to enable set boDiffuseTexture2 in the BumpOptions property.<p>

   The tertiary texture is used for the specular texture,
   to enable set boSpecularTexture3 in the BumpOptions property.
   The SpecularMode determines the specular highlight calculation
   (Blinn or Phong), smOff disables specular highlights in the 
   shader.<p>

   External tangent bump space expects tangent data under
   GL_TEXTURE1_ARB and binormal data under GL_TEXTURE2_ARB.<p>

   The boUseSecondaryTexCoords bump option tells the shader to use 
   the secondary texture coordinates for the diffuse and specular
   texture lookups.<p>

   <b>History : </b><font size=-1><ul>
      <li>15/04/05 - SG - Added parallax offset mapping for the BasicARBfp bump method (experimental)
                          Height data is expected in the normal map alpha channel.
      <li>21/12/04 - SG - Added light attenutation support through the 
                          boLightAttenutation option in the BumpOptions property.
      <li>27/10/04 - SG - Added boUseSecondaryTexCoords option to BumpOptions
      <li>11/10/04 - SG - Added SpecularMode to define the specular highlight equation,
                          Removed the boDisableSpecular bump option (depricated).
      <li>06/10/04 - SG - Added special functions for generating the ARB programs
                          which replace the string constants.
      <li>02/10/04 - SG - Changed render order a little, minimum texture units
                          is now 2 for dot3 texcombiner bump method.
                          Changed vertex programs to accept local program
                          params, now only 1 vertex and 1 fragment program is
                          required for all lights.
                          Vertex programs now apply the primary texture matrix.
      <li>30/09/04 - SG - Added fragment program logic,
                          Added bmBasicARBFP bump method, bsTangentExternal
                          bump space and associated ARB programs,
                          Various name changes and fixes
      <li>28/09/04 - SG - Vertex programs now use ARB_position_invariant option.
      <li>29/06/04 - SG - Quaternion tangent space fix in tangent bump vertex
                          program.
      <li>23/06/04 - SG - Added bsTangent option to TBumpSpace,
                          Added tangent space light vector vertex program.
      <li>22/06/04 - SG - Creation.
   </ul></font>
}
unit GLBumpShader;

interface

uses
   Classes, SysUtils, GLTexture, GLContext, GLGraphics, GLUtils,
   VectorGeometry, OpenGL1x, VectorLists, ARBProgram;

type
   TBumpMethod = (bmDot3TexCombiner, bmBasicARBFP);

   TBumpSpace = (bsObject, bsTangentExternal, bsTangentQuaternion);

   TBumpOption = ( boDiffuseTexture2,       // Use secondary texture as diffuse
                   boSpecularTexture3,      // Use tertiary texture as specular
                   boUseSecondaryTexCoords, // Pass through secondary texcoords
                   boLightAttenuation,      // Use light attenuation
                   boParallaxMapping        // Enable parallax offset mapping
                  );
   TBumpOptions = set of TBumpOption;

   TSpecularMode = (smOff, smBlinn, smPhong);

   // TGLBumpShader
   //
   {: A generic bump shader.<p> }
   TGLBumpShader = class (TGLShader)
      private
         FVertexProgramHandles,
         FFragmentProgramHandles : array of Cardinal;
         FLightIDs : TIntegerList;
         FLightsEnabled : Integer;
         FBumpMethod : TBumpMethod;
         FBumpSpace : TBumpSpace;
         FBumpOptions : TBumpOptions;
         FSpecularMode : TSpecularMode;
         FDesignTimeEnabled : Boolean;
         FAmbientPass : Boolean;
         FDiffusePass : Boolean;
         FVertexProgram : TStringList;
         FFragmentProgram : TStringList;
         FParallaxOffset : Single;

         function GenerateVertexProgram : String;
         function GenerateFragmentProgram : String;
         procedure DoLightPass(lightID : Cardinal);

      protected
         procedure SetBumpMethod(const Value : TBumpMethod);
         procedure SetBumpSpace(const Value : TBumpSpace);
         procedure SetBumpOptions(const Value : TBumpOptions);
         procedure SetSpecularMode(const Value : TSpecularMode);
         procedure SetDesignTimeEnabled(const Value : Boolean);
         procedure SetParallaxOffset(const Value : Single);
         procedure Loaded; override;
         procedure DeleteVertexPrograms;
         procedure DeleteFragmentPrograms;

      public
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoApply(var rci: TRenderContextInfo; Sender: TObject); override;
         function DoUnApply(var rci: TRenderContextInfo) : Boolean; override;

      published
         property BumpMethod : TBumpMethod read FBumpMethod write SetBumpMethod;
         property BumpSpace : TBumpSpace read FBumpSpace write SetBumpSpace;
         property BumpOptions : TBumpOptions read FBumpOptions write SetBumpOptions;
         property SpecularMode : TSpecularMode read FSpecularMode write SetSpecularMode;
         property DesignTimeEnabled : Boolean read FDesignTimeEnabled write SetDesignTimeEnabled;
         property ParallaxOffset : Single read FParallaxOffset write SetParallaxOffset;

   end;

procedure Register;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene Shaders', [TGLBumpShader]);
end;

// ------------------
// ------------------ TGLBumpShader ------------------
// ------------------

// Create
//
constructor TGLBumpShader.Create(AOwner : TComponent);
begin
   inherited;
   FLightIDs:=TIntegerList.Create;
   FBumpMethod:=bmDot3TexCombiner;
   FBumpSpace:=bsObject;
   FBumpOptions:=[];
   FSpecularMode:=smOff;
   ShaderStyle:=ssLowLevel;
   FParallaxOffset:=0.04;

   FVertexProgram:=TStringList.Create;
   FFragmentProgram:=TStringList.Create;
end;

// Destroy
//
destructor TGLBumpShader.Destroy;
begin
   DeleteVertexPrograms;
   DeleteFragmentPrograms;
   FLightIDs.Free;
   FVertexProgram.Free;
   FFragmentProgram.Free;
   inherited;
end;

// Loaded
//
procedure TGLBumpShader.Loaded;
begin
   inherited;
end;

// GenerateVertexProgram
//
function TGLBumpShader.GenerateVertexProgram : String;
var
   VP : TStringList;
   DoTangent, DoSpecular, DoParallaxOffset : Boolean;
   texcoord : Integer;
begin
   DoSpecular:=(BumpMethod = bmBasicARBFP) and not (SpecularMode = smOff);
   DoTangent:=(BumpSpace = bsTangentExternal) or (BumpSpace = bsTangentQuaternion);
   DoParallaxOffset:=(BumpMethod = bmBasicARBFP) and (boParallaxMapping in BumpOptions) and DoTangent;

   VP:=TStringList.Create;

   VP.Add('!!ARBvp1.0');
   VP.Add('OPTION ARB_position_invariant;');

   VP.Add('PARAM mv[4] = { state.matrix.modelview };');
   VP.Add('PARAM mvinv[4] = { state.matrix.modelview.inverse };');
   VP.Add('PARAM mvit[4] = { state.matrix.modelview.invtrans };');
   VP.Add('PARAM tex[4] = { state.matrix.texture[0] };');
   if boUseSecondaryTexCoords in BumpOptions then
     VP.Add('PARAM tex2[4] = { state.matrix.texture[1] };');
   VP.Add('PARAM lightPos = program.local[0];');
   VP.Add('PARAM lightAtten = program.local[1];');
   if BumpSpace = bsTangentExternal then begin
      VP.Add('ATTRIB tangent = vertex.texcoord[1];');
      VP.Add('ATTRIB binormal = vertex.texcoord[2];');
      VP.Add('ATTRIB normal = vertex.normal;');
   end;
   VP.Add('TEMP temp, temp2, light, eye, atten;');

   if (boLightAttenuation in BumpOptions) then begin

      VP.Add('   DP4 temp.x, mv[0], vertex.position;');
      VP.Add('   DP4 temp.y, mv[1], vertex.position;');
      VP.Add('   DP4 temp.z, mv[2], vertex.position;');
      VP.Add('   ADD light, lightPos, -temp;');

      VP.Add('   DP3 atten.y, light, light;');
      VP.Add('   RSQ atten.y, atten.y;');
      if BumpMethod = bmDot3TexCombiner then begin
         VP.Add('   RCP atten.y, atten.y;');
         VP.Add('   MUL atten.z, atten.y, atten.y;');
         VP.Add('   MAD atten.x, lightAtten.y, atten.y, lightAtten.x;');
         VP.Add('   MAD atten.x, lightAtten.z, atten.z, atten.x;');
         VP.Add('   RCP atten.x, atten.x;');
      end else if BumpMethod = bmBasicARBFP then begin
         // Store the distance in atten.x for ARBFP,
         // fragment program will calculate attenutation
         VP.Add('   RCP atten.x, atten.y;');
      end;

      VP.Add('   DP3 temp.x, mvinv[0], light;');
      VP.Add('   DP3 temp.y, mvinv[1], light;');
      VP.Add('   DP3 temp.z, mvinv[2], light;');
      VP.Add('   MOV light, temp;');
   end else begin
      VP.Add('   DP4 light.x, mvinv[0], lightPos;');
      VP.Add('   DP4 light.y, mvinv[1], lightPos;');
      VP.Add('   DP4 light.z, mvinv[2], lightPos;');
      VP.Add('   ADD light, light, -vertex.position;');
   end;

   if DoSpecular or DoParallaxOffset then
      VP.Add('   ADD eye, mvit[3], -vertex.position;');

   if DoTangent then begin
      if BumpSpace = bsTangentExternal then begin

         VP.Add('   DP3 temp.x, light, tangent;');
         VP.Add('   DP3 temp.y, light, binormal;');
         VP.Add('   DP3 temp.z, light, normal;');
         VP.Add('   MOV light, temp;');
         if DoSpecular or DoParallaxOffset then begin
            VP.Add('   DP3 temp.x, eye, tangent;');
            VP.Add('   DP3 temp.y, eye, binormal;');
            VP.Add('   DP3 temp.z, eye, normal;');
            VP.Add('   MOV eye, temp;');
         end;

      end else if BumpSpace = bsTangentQuaternion then begin

         VP.Add('   DP3 temp.x, light, light;');
         VP.Add('   RSQ temp.x, temp.x;');
         VP.Add('   MUL light, temp.x, light;');

         VP.Add('   MOV temp2.x, vertex.normal.y;');
         VP.Add('   ADD temp2.y, 0.0, -vertex.normal.x;');
         VP.Add('   MOV temp2.z, 0.0;');

         VP.Add('   DP3 temp.x, temp2, light;');
         VP.Add('   MUL temp.x, temp2.y, light.z;');
         VP.Add('   MAD temp.y, vertex.normal.z, light.x, temp.x;');
         VP.Add('   MUL temp.x, vertex.normal.y, light.z;');
         VP.Add('   MAD temp.z, vertex.normal.z, light.y, -temp.x;');
         VP.Add('   MUL temp.x, vertex.normal.y, light.y;');
         VP.Add('   MAD temp.x, vertex.normal.z, light.z, temp.x;');
         VP.Add('   MAD temp.w, -temp2.y, light.x, temp.x;');
         VP.Add('   MOV light, temp.yzwy;');

         if DoSpecular or DoParallaxOffset then begin
            VP.Add('   DP3 temp.x, temp2, eye;');
            VP.Add('   MUL temp.x, temp2.y, eye.z;');
            VP.Add('   MAD temp.y, vertex.normal.z, eye.x, temp.x;');
            VP.Add('   MUL temp.x, vertex.normal.y, eye.z;');
            VP.Add('   MAD temp.z, vertex.normal.z, eye.y, -temp.x;');
            VP.Add('   MUL temp.x, vertex.normal.y, eye.y;');
            VP.Add('   MAD temp.x, vertex.normal.z, eye.z, temp.x;');
            VP.Add('   MAD temp.w, -temp2.y, eye.x, temp.x;');
            VP.Add('   MOV eye, temp.yzwy;');
         end;

      end;
   end;

   if BumpMethod = bmDot3TexCombiner then begin

      if BumpSpace<>bsTangentQuaternion then begin
         VP.Add('   DP3 temp.x, light, light;');
         VP.Add('   RSQ temp, temp.x;');
         VP.Add('   MUL light, temp.x, light;');
      end;

      if boLightAttenuation in BumpOptions then
         VP.Add('   MUL light, atten.x, light;');

      VP.Add('   MAD result.color, light, 0.5, 0.5;');
      VP.Add('   MOV result.color.w, 1.0;');

   end else if BumpMethod = bmBasicARBFP then begin

      if boLightAttenuation in BumpOptions then
         VP.Add('   MOV light.w, atten.x;')
      else
         VP.Add('   MOV light.w, 0.0;');
      if DoSpecular or DoParallaxOffset then
         VP.Add('   MOV eye.w, 0.0;');

   end;

   texcoord:=0;

   VP.Add('   DP4 temp.x, vertex.texcoord[0], tex[0];');
   VP.Add('   DP4 temp.y, vertex.texcoord[0], tex[1];');
   VP.Add('   DP4 temp.z, vertex.texcoord[0], tex[2];');
   VP.Add('   DP4 temp.w, vertex.texcoord[0], tex[3];');
   VP.Add('   MOV result.texcoord['+IntToStr(texcoord)+'], temp;');
   Inc(texcoord);

   if boUseSecondaryTexCoords in BumpOptions then begin
      VP.Add('   DP4 temp.x, vertex.texcoord[1], tex2[0];');
      VP.Add('   DP4 temp.y, vertex.texcoord[1], tex2[1];');
      VP.Add('   DP4 temp.z, vertex.texcoord[1], tex2[2];');
      VP.Add('   DP4 temp.w, vertex.texcoord[1], tex2[3];');
      VP.Add('   MOV result.texcoord['+IntToStr(texcoord)+'], temp;');
      Inc(texcoord);
   end;

   if BumpMethod = bmDot3TexCombiner then begin
      if (boDiffuseTexture2 in BumpOptions)
      and not (boUseSecondaryTexCoords in BumpOptions) then
         VP.Add('   MOV result.texcoord['+IntToStr(texcoord)+'], temp;');
   end else begin
      VP.Add('   MOV result.texcoord['+IntToStr(texcoord)+'], light;');
      Inc(texcoord);
      if DoSpecular then
         VP.Add('   MOV result.texcoord['+IntToStr(texcoord)+'], eye;');
   end;
   
   VP.Add('END');

   FVertexProgram.Assign(VP);
   Result:=VP.Text;
   VP.Free;
end;

// GenerateFragmentProgram
//
function TGLBumpShader.GenerateFragmentProgram : String;
var
   FP : TStringList;
   DoSpecular,
   DoTangent,
   DoParallaxOffset : Boolean;
   texcoord,
   normalTexCoords,
   diffTexCoords,
   specTexCoords,
   lightTexCoords,
   eyeTexCoords : Integer;
begin
   DoSpecular:=not (SpecularMode = smOff);
   DoTangent:=(BumpSpace = bsTangentExternal) or (BumpSpace = bsTangentQuaternion);
   DoParallaxOffset:=(boParallaxMapping in BumpOptions) and DoTangent;

   texcoord:=0;
   normalTexCoords:=texcoord;
   if boUseSecondaryTexCoords in BumpOptions then
      Inc(texcoord);
   diffTexCoords:=texcoord;
   specTexCoords:=texcoord;
   Inc(texcoord);
   lightTexCoords:=texcoord;
   Inc(texcoord);
   eyeTexCoords:=texcoord;

   FP:=TStringList.Create;

   FP.Add('!!ARBfp1.0');

   FP.Add('PARAM lightDiffuse = program.local[0];');
   FP.Add('PARAM lightSpecular = program.local[1];');
   FP.Add('PARAM lightAtten = program.local[2];');
   FP.Add('PARAM materialDiffuse = state.material.diffuse;');
   FP.Add('PARAM materialSpecular = state.material.specular;');
   FP.Add('PARAM shininess = state.material.shininess;');
   FP.Add('TEMP temp, tex, light, eye, normal, col, diff, spec;');
   FP.Add('TEMP textureColor, reflect, atten, offset, texcoord;');

   if DoSpecular or DoParallaxOffset then begin
      // Get the eye vector
      FP.Add('   DP3 eye, fragment.texcoord['+IntToStr(eyeTexCoords)+'], fragment.texcoord['+IntToStr(eyeTexCoords)+'];');
      FP.Add('   RSQ eye, eye.x;');
      FP.Add('   MUL eye, fragment.texcoord['+IntToStr(eyeTexCoords)+'], eye.x;');
   end;

   if DoParallaxOffset then begin
      // Get the parallax offset
      FP.Add('   TEX textureColor, fragment.texcoord['+IntToStr(normalTexCoords)+'], texture[0], 2D;');
      FP.Add(Format('   MAD offset.x, textureColor.a, %f, %f;',[FParallaxOffset, -0.5*FParallaxOffset]));
      FP.Add('   MUL offset, eye, offset.x;');
      FP.Add('   ADD texcoord, fragment.texcoord['+IntToStr(normalTexCoords)+'], offset;');
   end else
      FP.Add('   MOV texcoord, fragment.texcoord['+IntToStr(normalTexCoords)+'];');

   // Get the normalized normal vector
   FP.Add('   TEX textureColor, texcoord, texture[0], 2D;');
   FP.Add('   ADD normal, textureColor, -0.5;');
   FP.Add('   DP3 temp, normal, normal;');
   FP.Add('   RSQ temp, temp.x;');
   FP.Add('   MUL normal, normal, temp.x;');
   
   // Get the normalized light vector
   FP.Add('   MOV light, fragment.texcoord['+IntToStr(lightTexCoords)+'];');
   if boLightAttenuation in BumpOptions then
      FP.Add('   MOV atten.x, light.w;');
   FP.Add('   DP3 light, light, light;');
   FP.Add('   RSQ light, light.x;');
   FP.Add('   MUL light, fragment.texcoord['+IntToStr(lightTexCoords)+'], light.x;');

   // Calculate the diffuse color
   FP.Add('   DP3 diff, normal, light;');
   FP.Add('   MUL diff, diff, lightDiffuse;');
   FP.Add('   MUL diff, diff, materialDiffuse;');
   if boDiffuseTexture2 in BumpOptions then begin
      if DoParallaxOffset then begin
         FP.Add('   ADD temp, fragment.texcoord['+IntToStr(diffTexCoords)+'], offset;');
         FP.Add('   TEX textureColor, temp, texture[1], 2D;');
      end else
         FP.Add('   TEX textureColor, fragment.texcoord['+IntToStr(diffTexCoords)+'], texture[1], 2D;');
      FP.Add('   MUL diff, diff, textureColor;');
   end;

   if DoSpecular then begin
      case SpecularMode of
         smBlinn : begin
            FP.Add('   ADD eye, eye, light;');
            FP.Add('   DP3 temp, eye, eye;');
            FP.Add('   RSQ temp, temp.x;');
            FP.Add('   MUL eye, eye, temp.x;');
            FP.Add('   DP3_SAT spec, normal, eye;');
         end;
         smPhong : begin
            FP.Add('   DP3 reflect, normal, light;');
            FP.Add('   MUL reflect, reflect.x, normal;');
            FP.Add('   MUL reflect, 2.0, reflect;');
            FP.Add('   ADD reflect, reflect, -light;');
            FP.Add('   DP3_SAT spec, reflect, eye;');
         end;
      else
         Assert(False, 'Invalid specular mode!');
      end;

      FP.Add('   POW spec, spec.x, shininess.x;');
      FP.Add('   MUL spec, spec, materialSpecular;');
      FP.Add('   MUL spec, spec, lightSpecular;');
      if boSpecularTexture3 in BumpOptions then begin
         if DoParallaxOffset then begin
            FP.Add('   ADD temp, fragment.texcoord['+IntToStr(specTexCoords)+'], offset;');
            FP.Add('   TEX textureColor, temp, texture[2], 2D;');
         end else
            FP.Add('   TEX textureColor, fragment.texcoord['+IntToStr(specTexCoords)+'], texture[2], 2D;');
         FP.Add('   MUL spec, spec, textureColor;');
      end;
   end;

   // Output
   if DoSpecular then
      FP.Add('   ADD temp, diff, spec;')
   else
      FP.Add('   MOV temp, diff;');

   if boLightAttenuation in BumpOptions then begin
      FP.Add('   MUL atten.y, atten.x, atten.x;');
      FP.Add('   MAD atten.x, lightAtten.y, atten.x, lightAtten.x;');
      FP.Add('   MAD atten.x, lightAtten.z, atten.y, atten.x;');
      FP.Add('   RCP atten.x, atten.x;');
      FP.Add('   MUL temp, temp, atten.x;');
   end;

   FP.Add('   MOV_SAT result.color, temp;');
   FP.Add('   MOV result.color.w, 1.0;');

   FP.Add('END');

   FFragmentProgram.Assign(FP);
   Result:=FP.Text;
   FP.Free;
end;

// DoLightPass
//
procedure TGLBumpShader.DoLightPass(lightID : Cardinal);
var
   dummyHandle, tempHandle : Integer;
   lightPos, lightAtten,
   materialDiffuse, lightDiffuse, lightSpecular : TVector;
begin
   glEnable(GL_VERTEX_PROGRAM_ARB);
   glBindProgramARB(GL_VERTEX_PROGRAM_ARB, FVertexProgramHandles[0]);

   // Set the light position to program.local[0]
   glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_POSITION, @lightPos[0]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 0, @lightPos[0]);

   // Set the light attenutation to program.local[1]
   glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_CONSTANT_ATTENUATION, @lightAtten[0]);
   glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_LINEAR_ATTENUATION, @lightAtten[1]);
   glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_QUADRATIC_ATTENUATION, @lightAtten[2]);
   glProgramLocalParameter4fvARB(GL_VERTEX_PROGRAM_ARB, 1, @lightAtten[0]);

   case FBumpMethod of
      bmDot3TexCombiner : begin
         glActiveTextureARB(GL_TEXTURE0_ARB);
         glGetIntegerv(GL_TEXTURE_BINDING_2D, @dummyHandle);
         glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_DOT3_RGB_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_TEXTURE0_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_PRIMARY_COLOR_ARB);

         glActiveTextureARB(GL_TEXTURE1_ARB);
         glEnable(GL_TEXTURE_2D);
         glGetIntegerv(GL_TEXTURE_BINDING_2D, @tempHandle);
         if tempHandle = 0 then
            glBindTexture(GL_TEXTURE_2D, dummyHandle);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_DIFFUSE, @lightDiffuse);
         glGetMaterialfv(GL_FRONT, GL_DIFFUSE, @materialDiffuse);
         lightDiffuse[0]:=lightDiffuse[0]*materialDiffuse[0];
         lightDiffuse[1]:=lightDiffuse[1]*materialDiffuse[1];
         lightDiffuse[2]:=lightDiffuse[2]*materialDiffuse[2];
         lightDiffuse[3]:=lightDiffuse[3]*materialDiffuse[3];
         glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, @lightDiffuse);
         glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB, GL_MODULATE);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB, GL_PREVIOUS_ARB);
         glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_ARB, GL_CONSTANT_COLOR_ARB);

         glActiveTextureARB(GL_TEXTURE2_ARB);
         glDisable(GL_TEXTURE_2D);

         glActiveTextureARB(GL_TEXTURE0_ARB);
      end;

      bmBasicARBFP : begin
         glEnable(GL_FRAGMENT_PROGRAM_ARB);
         glBindProgramARB(GL_FRAGMENT_PROGRAM_ARB, FFragmentProgramHandles[0]);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_DIFFUSE, @lightDiffuse[0]);
         glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 0, @lightDiffuse[0]);
         glGetLightfv(GL_LIGHT0+FLightIDs[0], GL_SPECULAR, @lightSpecular[0]);
         glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 1, @lightSpecular[0]);
         glProgramLocalParameter4fvARB(GL_FRAGMENT_PROGRAM_ARB, 2, @lightAtten[0]);
      end;

  else
     Assert(False, 'Invalid bump method!');
  end;
end;

// DoApply
//
procedure TGLBumpShader.DoApply(var rci: TRenderContextInfo; Sender: TObject);
var
   maxLights, maxTextures, i : Integer;
   lightEnabled : GLboolean;
   ambient, materialAmbient : TColorVector;
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
      if  (maxTextures<3) 
      and ((BumpMethod<>bmDot3TexCombiner) or (BumpSpace=bsTangentExternal)) then
         raise Exception.Create('The current shader settings require 3 or more texture units.');
      if  (maxTextures<4) 
      and (BumpMethod<>bmDot3TexCombiner) 
      and (boUseSecondaryTexCoords in BumpOptions) 
      and (SpecularMode<>smOff) then
         raise Exception.Create('The current shader settings require 4 or more texture units.');

      if Length(FVertexProgramHandles) = 0 then begin
         SetLength(FVertexProgramHandles, 1);
         str:=GenerateVertexProgram;
         LoadARBProgram(GL_VERTEX_PROGRAM_ARB, str, FVertexProgramHandles[0]);
      end;

      if Length(FFragmentProgramHandles) = 0 then
         if FBumpMethod = bmBasicARBFP then begin
            SetLength(FFragmentProgramHandles, 1);
            str:=GenerateFragmentProgram;
            LoadARBProgram(GL_FRAGMENT_PROGRAM_ARB, str, FFragmentProgramHandles[0])
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
   glActiveTextureARB(GL_TEXTURE0_ARB);
   if glIsEnabled(GL_TEXTURE_2D) then
      for i:=0 to maxLights-1 do begin
         glGetBooleanv(GL_LIGHT0+i, @lightEnabled);
         if lightEnabled then
            FLightIDs.Add(i);
   end;
   FLightsEnabled:=FLightIDs.Count;

   FAmbientPass:=False;
   FDiffusePass:=False;

   if FLightIDs.Count>0 then begin

      glDepthFunc(GL_LEQUAL);
      glDisable(GL_BLEND);
      DoLightPass(FLightIDs[0]);
      FLightIDs.Delete(0);

   end else begin

      glDisable(GL_LIGHTING);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE2_ARB);
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
end;

// DoUnApply
//
function TGLBumpShader.DoUnApply(var rci: TRenderContextInfo) : Boolean;
var
   ambient, materialAmbient : TVector;
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

   end else if not FDiffusePass and (FLightsEnabled <> 0)
   and (boDiffuseTexture2 in BumpOptions)
   and (BumpMethod = bmDot3TexCombiner) then begin

      glEnable(GL_BLEND);
      glBlendFunc(GL_DST_COLOR, GL_ZERO);

      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glEnable(GL_TEXTURE_2D);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
      glActiveTextureARB(GL_TEXTURE2_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);

      FDiffusePass:=True;
      Result:=True;
      Exit;

   end else if not FAmbientPass then begin

      glDisable(GL_VERTEX_PROGRAM_ARB);
      if BumpMethod = bmBasicARBFP then
         glDisable(GL_FRAGMENT_PROGRAM_ARB);

      glDisable(GL_LIGHTING);
      glActiveTextureARB(GL_TEXTURE0_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE2_ARB);
      glDisable(GL_TEXTURE_2D);
      glActiveTextureARB(GL_TEXTURE0_ARB);

      glDepthFunc(GL_LEQUAL);
      glEnable(GL_BLEND);
      glBlendFunc(GL_ONE, GL_ONE);

      glGetFloatv(GL_LIGHT_MODEL_AMBIENT, @ambient);
      glGetMaterialfv(GL_FRONT, GL_AMBIENT, @materialAmbient);
      ambient[0]:=ambient[0]*materialAmbient[0];
      ambient[1]:=ambient[1]*materialAmbient[1];
      ambient[2]:=ambient[2]*materialAmbient[2];
      glColor3fv(@ambient);

      FAmbientPass:=True;
      Result:=True;
      Exit;

   end;

   glDisable(GL_VERTEX_PROGRAM_ARB);
   if BumpMethod = bmBasicARBFP then
      glDisable(GL_FRAGMENT_PROGRAM_ARB);

   glPopAttrib;
end;

// DeleteVertexPrograms
//
procedure TGLBumpShader.DeleteVertexPrograms;
begin
   if Length(FVertexProgramHandles) > 0 then begin
      glDeleteProgramsARB(
        Length(FVertexProgramHandles), @FVertexProgramHandles[0]);
      SetLength(FVertexProgramHandles, 0);
   end;
   FVertexProgram.Clear;
end;

// DeleteFragmentPrograms
//
procedure TGLBumpShader.DeleteFragmentPrograms;
begin
   if Length(FFragmentProgramHandles) > 0 then begin
      glDeleteProgramsARB(
        Length(FFragmentProgramHandles), @FFragmentProgramHandles[0]);
      SetLength(FFragmentProgramHandles, 0);
   end;
   FFragmentProgram.Clear;
end;

// SetBumpMethod
//
procedure TGLBumpShader.SetBumpMethod(const Value: TBumpMethod);
begin
   if Value<>FBumpMethod then begin
      FBumpMethod:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetBumpSpace
//
procedure TGLBumpShader.SetBumpSpace(const Value: TBumpSpace);
begin
   if Value<>FBumpSpace then begin
      FBumpSpace:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetBumpOptions
//
procedure TGLBumpShader.SetBumpOptions(const Value: TBumpOptions);
begin
   if Value<>FBumpOptions then begin
      FBumpOptions:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetSpecularMode
//
procedure TGLBumpShader.SetSpecularMode(const Value: TSpecularMode);
begin
   if Value<>FSpecularMode then begin
      FSpecularMode:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

// SetDesignTimeEnabled
//
procedure TGLBumpShader.SetDesignTimeEnabled(const Value: Boolean);
begin
   if Value<>FDesignTimeEnabled then begin
      FDesignTimeEnabled:=Value;
      NotifyChange(Self);
   end;
end;

// SetParallaxOffset
//
procedure TGLBumpShader.SetParallaxOffset(const Value: Single);
begin
   if Value<>FParallaxOffset then begin
      FParallaxOffset:=Value;
      DeleteVertexPrograms;
      DeleteFragmentPrograms;
      NotifyChange(Self);
   end;
end;

end.
