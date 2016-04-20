//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLTextureCombiners<p>

   Texture combiners setup utility functions.<p>

   <b>History : </b><font size=-1><ul>
      <li>17/12/03 - EG - Alpha and RGB channels separate combination now supported
      <li>23/05/03 - EG - All tex units now accepted as target
      <li>22/05/03 - EG - Fixed GL_ADD_SIGNED_ARB parsing, better error reporting
      <li>16/05/03 - EG - Creation
   </ul></font>
}
unit GLTextureCombiners;

interface

uses SysUtils;

type

   // ETextureCombinerError
   //
   ETextureCombinerError = class (Exception)
   ;

{: Parses a TC text description and setups combiners accordingly.<p>
   *experimental*<br>
   Knowledge of texture combiners is a requirement<br>
   Syntax: pascal-like, one instruction per line, use '//' for comment.<p>

   Examples:<ul>
   <li>Tex1:=Tex0;   // replace texture 1 with texture 0
   <li>Tex1:=Tex0+Tex1; // additive blending between textures 0 and 1
   <li>Tex1:=Tex0-Tex1; // subtractive blending between textures 0 and 1
   <li>Tex1:=Tex0*Tex1; // modulation between textures 0 and 1
   <li>Tex1:=Tex0+Tex1-0.5; // signed additive blending between textures 0 and 1
   <li>Tex1:=Interpolate(Tex0, Tex1, PrimaryColor); // interpolation between textures 0 and 1 using primary color as factor
   <li>Tex1:=Dot3(Tex0, Tex1); // dot3 product between textures 0 and 1
   </ul><p>

   Accepted tokens:<ul>
   <li>Tex0, Tex1, etc. : texture unit
   <li>PrimaryColor, Col : the primary color
   <li>ConstantColor, EnvCol : texture environment constant color
   </ul><br>
   Tokens can be qualified with '.a' or '.alpha' to specify the alpha channel
   explicitly, and '.rgb' to specify color channels (default). You cannot mix
   alpha and rgb tokens in the same line.
}
procedure SetupTextureCombiners(const tcCode : String);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Classes, GLContext, OpenGL1x;

// TCAssertCheck
//
procedure TCAssertCheck(const b : Boolean; const errMsg : String);
begin
   if not b then
      raise ETextureCombinerError.Create(errMsg);
end;

// RemoveSpaces
//
function RemoveSpaces(const str : String) : String;
var
   c : Char;
   i, p, n : Integer;
begin
   n:=Length(str);
   SetLength(Result, n);
   p:=1;
   for i:=1 to n do begin
      c:=str[i];
      if c<>' ' then begin
         Result[p]:=c;
         Inc(p);
      end;
   end;
   SetLength(Result, p-1);
end;

// ProcessTextureCombinerArgument
//
procedure ProcessTextureCombinerArgument(arg : String; sourceEnum, operandEnum : Integer;
                                         const dest : String);
var
   sourceValue, operandValue, n, p : Integer;
   origArg, qualifier : String;
begin
   origArg:=arg;
   p:=Pos('.', arg);
   if p>0 then begin
      qualifier:=Copy(arg, p+1, MaxInt);
      arg:=Copy(arg, 1, p-1);
   end else qualifier:='rgb';
   if qualifier='rgb' then begin
      if Copy(arg, 1, 1)='~' then begin
         operandValue:=GL_ONE_MINUS_SRC_COLOR;
         arg:=Copy(arg, 2, MaxInt);
      end else if Copy(arg, 1, 2)='1-' then begin
         operandValue:=GL_ONE_MINUS_SRC_COLOR;
         arg:=Copy(arg, 3, MaxInt);
      end else operandValue:=GL_SRC_COLOR;
   end else if Copy(qualifier, 1, 1)='a' then begin
      if Copy(arg, 1, 1)='~' then begin
         operandValue:=GL_ONE_MINUS_SRC_ALPHA;
         arg:=Copy(arg, 2, MaxInt);
      end else if Copy(arg, 1, 2)='1-' then begin
         operandValue:=GL_ONE_MINUS_SRC_ALPHA;
         arg:=Copy(arg, 3, MaxInt);
      end else operandValue:=GL_SRC_ALPHA;
   end else operandValue:=0;
   sourceValue:=0;
   if (arg='tex') or (arg=dest) then
      sourceValue:=GL_TEXTURE
   else if ((arg='tex0') and (dest='tex1')) or ((arg='tex1') and (dest='tex2'))
        or ((arg='tex2') and (dest='tex3')) then
      sourceValue:=GL_PREVIOUS_ARB
   else if (arg='col') or (arg='col0') or (arg='primarycolor') then
      sourceValue:=GL_PRIMARY_COLOR_ARB
   else if (arg='envcol') or (arg='constcol') or (arg='constantcolor') then
      sourceValue:=GL_CONSTANT_COLOR_ARB
   else if Copy(arg, 1, 3)='tex' then begin
      TCAssertCheck(GL_ARB_texture_env_crossbar or GL_NV_texture_env_combine4,
             'Requires GL_ARB_texture_env_crossbar or NV_texture_env_combine4');
      n:=StrToIntDef(Copy(arg, 4, MaxInt), -1);
      if n in [0..7] then
         sourceValue:=GL_TEXTURE0_ARB+n;
   end;
   TCAssertCheck((operandValue>0) and (sourceValue>0),
                 'invalid argument : "'+origArg+'"');
   glTexEnvf(GL_TEXTURE_ENV, sourceEnum , sourceValue);
   glTexEnvf(GL_TEXTURE_ENV, operandEnum, operandValue);
   CheckOpenGLError;
end;

// ProcessTextureCombinerLine
//
procedure ProcessTextureCombinerLine(const tcLine : String);
var
   line, dest, arg1, arg2, arg3, funcname : String;
   p : Integer;
   destEnum, operEnum : Integer;
   sourceBaseEnum, operandBaseEnum : Integer;
   sl : TStrings;
begin
   // initial filtering
   line:=LowerCase(RemoveSpaces(Trim(tcLine)));
   if Copy(line, 1, 2)='//' then Exit;
   if line='' then Exit;
   if line[Length(line)]=';' then begin
      line:=Trim(Copy(line, 1, Length(line)-1));
      if line='' then Exit;
   end;
   // Parse destination
   p:=Pos(':=', line);
   dest:=Copy(line, 1, p-1);
   line:=Copy(line, p+2, MaxInt);
   p:=Pos('.', dest);
   destEnum:=GL_COMBINE_RGB_ARB;
   sourceBaseEnum:=GL_SOURCE0_RGB_ARB;
   operandBaseEnum:=GL_OPERAND0_RGB_ARB;
   if p>0 then begin
      if Copy(dest, p+1, 1)='a' then begin
         destEnum:=GL_COMBINE_ALPHA_ARB;
         sourceBaseEnum:=GL_SOURCE0_ALPHA_ARB;
         operandBaseEnum:=GL_OPERAND0_ALPHA_ARB;
      end;
      dest:=Copy(dest, 1, p-1);
   end;
   if Copy(dest, 1, 3)='tex' then begin
      p:=StrToIntDef(Copy(dest, 4, MaxInt), -1);
      TCAssertCheck(p>=0, 'Invalid destination texture unit "'+dest+'"');
      glActiveTextureARB(GL_TEXTURE0_ARB+p)
   end else TCAssertCheck(False, 'Invalid destination "'+dest+'"');
   // parse combiner operator
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
   CheckOpenGLError;
   operEnum:=0;
   arg1:=''; arg2:=''; arg3:='';
   p:=Pos('+', line);
   if p>0 then begin
      // ADD & ADD_SIGNED operators
      if Copy(line, Length(line)-3, 4)='-0.5' then begin
         operEnum:=GL_ADD_SIGNED_ARB;
         SetLength(line, Length(line)-4);
      end else operEnum:=GL_ADD;
      arg1:=Copy(line, 1, p-1);
      arg2:=Copy(line, p+1, MaxInt);
   end;
   p:=Pos('*', line);
   if p>0 then begin
      // MODULATE operator
      operEnum:=GL_MODULATE;
      arg1:=Copy(line, 1, p-1);
      arg2:=Copy(line, p+1, MaxInt);
      line:='';
   end;
   p:=Pos('(', line);
   if p>0 then begin
      // function
      sl:=TStringList.Create;
      try
         funcName:=Copy(line, 1, p-1);
         p:=Pos('(', line);
         line:=Copy(line, p+1, MaxInt);
         p:=Pos(')', line);
         sl.CommaText:=Copy(line, 1, p-1);
         if funcName='interpolate' then begin
            // INTERPOLATE operator
            TCAssertCheck(sl.Count=3, 'Invalid parameter count');
            operEnum:=GL_INTERPOLATE_ARB;
            arg1:=sl[0];
            arg2:=sl[1];
            arg3:=sl[2];
         end else if funcName='dot3' then begin
            // DOT3 operator
            TCAssertCheck(sl.Count=2, 'Invalid parameter count');
            TCAssertCheck(GL_ARB_texture_env_dot3, 'Requires GL_ARB_texture_env_dot3');
            operEnum:=GL_DOT3_RGB_ARB;
            arg1:=sl[0];
            arg2:=sl[1];
         end else TCAssertCheck(False, 'Invalid function "'+funcName+'"');
      finally
         sl.Free;
      end;
      line:='';
   end;
   p:=Pos('-', line);
   if p>0 then begin
      // SUBTRACT operator
      operEnum:=GL_SUBTRACT_ARB;
      arg1:=Copy(line, 1, p-1);
      arg2:=Copy(line, p+1, MaxInt);
      line:='';
   end;
   if operEnum=0 then begin
      // REPLACE by default
      operEnum:=GL_REPLACE;
      arg1:=line;
   end;

   glTexEnvi(GL_TEXTURE_ENV, destEnum, operEnum);
   CheckOpenGLError;
   // parse arguments
   if arg1<>'' then
      ProcessTextureCombinerArgument(arg1, sourceBaseEnum, operandBaseEnum, dest);
   if arg2<>'' then
      ProcessTextureCombinerArgument(arg2, sourceBaseEnum+1, operandBaseEnum+1, dest);
   if arg3<>'' then
      ProcessTextureCombinerArgument(arg3, sourceBaseEnum+2, operandBaseEnum+2, dest);

   glActiveTextureARB(GL_TEXTURE0_ARB);
end;

// SetupTextureCombiners
//
procedure SetupTextureCombiners(const tcCode : String);
var
   i : Integer;
   sl : TStringList;
begin
   TCAssertCheck(GL_ARB_texture_env_combine, 'Requires GL_ARB_texture_env_combine support');
   sl:=TStringList.Create;
   try
      sl.Text:=tcCode;
      for i:=0 to sl.Count-1 do
         ProcessTextureCombinerLine(sl[i]);
   finally
      sl.Free;
   end;
end;

end.
