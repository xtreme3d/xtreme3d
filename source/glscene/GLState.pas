{: GLState<p>

   Miscellaneous support routines & classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/10/04 - NC - Added stTextureRect (GL_TEXTURE_RECTANGLE_NV)
      <li>07/01/04 - EG - Introduced TGLStateCache
      <li>05/09/03 - EG - Creation from GLMisc split
   </ul></font>
}
unit GLState;

interface

uses Classes, VectorGeometry, SysUtils, OpenGL1x;

{$i GLScene.inc}

type

   // TGLState
   //
	//: Reflects all relevant (binary) states of OpenGL subsystem
	TGLState = (stAlphaTest, stAutoNormal,
					stBlend, stColorMaterial, stCullFace, stDepthTest, stDither,
					stFog, stLighting, stLineSmooth, stLineStipple,
					stLogicOp, stNormalize, stPointSmooth, stPolygonSmooth,
					stPolygonStipple, stScissorTest, stStencilTest,
					stTexture1D, stTexture2D, stTextureCubeMap, stTextureRect);
	TGLStates = set of TGLState;

   // TFaceWinding
   //
	//: Describe what kind of winding has a front face
	TFaceWinding = (fwCounterClockWise, fwClockWise);

   // TGLStateCache
   //
   {: Manages an application-side cache of OpenGL states and parameters.<p>
      Purpose of this class is to eliminate redundant state and parameter
      changes, and there will typically be no more than one state cache per
      OpenGL context. }
   TGLStateCache = class
 		private
         { Private Declarations }
         FFrontBackColors : array [0..1, 0..3] of TVector;
         FFrontBackShininess : array [0..1] of Integer;
         FStates : TGLStates;
         FTextureHandle : array [0..7] of Integer;
         FLastFrontMode, FLastBackMode : TGLEnum;
         FFrontFaceCCW : Boolean;
         FTextureMatrixIsIdenty : Boolean;

		protected
         { Protected Declarations }

		public
         { Public Declarations }
         constructor Create; virtual;
         destructor Destroy; override;

         procedure SetGLState(const aState : TGLState);
         procedure UnSetGLState(const aState : TGLState);

         {: Adjusts PolygonMode for a face }
         procedure SetGLPolygonMode(const aFace, mode : TGLEnum);
         //: Reset GLPolygonMode, next calls to SetGLPolygonMode WILL do something
         procedure ResetGLPolygonMode;

         {: Adjusts material colors for a face. }
         procedure SetGLMaterialColors(const aFace : TGLEnum;
                                       const emission, ambient, diffuse, specular : TVector;
                                       const shininess : Integer);
         {: Adjusts material alpha channel for a face. }
         procedure SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
         //: Reset GLMaterial colors, next calls to SetGLMaterial WILL do something
         procedure ResetGLMaterialColors;

         {: Specify a new texture handle for the target of textureUnit.<p>
            Does NOT perform glActiveTextureARB calls. } 
         procedure SetGLCurrentTexture(const textureUnit, target, handle : Integer);
         procedure ResetGLCurrentTexture;

         {: Defines the OpenGL texture matrix.<p>
            Assumed texture mode is GL_MODELVIEW. }
         procedure SetGLTextureMatrix(const matrix : TMatrix);
         {: Resets the OpenGL texture matrix to Identity.<p>
            Assumed texture mode is GL_MODELVIEW. }
         procedure ResetGLTextureMatrix;

         {: Inverts front face winding (CCW/CW). }
         procedure InvertGLFrontFace;
         {: Reset to default front face winding (CCW). }
         procedure ResetGLFrontFace;
         {: Set front face winding to ClockWise. }
         procedure SetGLFrontFaceCW;
         {: Set front face winding to Counter-ClockWise. }
         procedure SetGLFrontFaceCCW;

         {: Invokes all Reset methods. }
         procedure ResetAll;

         // read only properties

         property States : TGLStates read FStates;
   end;

//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------
implementation
//------------------------------------------------------
//------------------------------------------------------
//------------------------------------------------------

const
	cGLStateToGLEnum : array [stAlphaTest..stTextureRect] of TGLEnum =
		(GL_ALPHA_TEST, GL_AUTO_NORMAL, GL_BLEND, GL_COLOR_MATERIAL, GL_CULL_FACE,
		 GL_DEPTH_TEST, GL_DITHER, GL_FOG, GL_LIGHTING, GL_LINE_SMOOTH,
		 GL_LINE_STIPPLE, GL_LOGIC_OP, GL_NORMALIZE, GL_POINT_SMOOTH,
		 GL_POLYGON_SMOOTH, GL_POLYGON_STIPPLE, GL_SCISSOR_TEST, GL_STENCIL_TEST,
		 GL_TEXTURE_1D, GL_TEXTURE_2D, GL_TEXTURE_CUBE_MAP_ARB, GL_TEXTURE_RECTANGLE_NV );

// ------------------
// ------------------ TGLStateCache ------------------
// ------------------

// Create
//
constructor TGLStateCache.Create;
begin
   inherited;
   FTextureMatrixIsIdenty:=True;
   FFrontFaceCCW:=True;
end;

// Destroy
//
destructor TGLStateCache.Destroy;
begin
   inherited;
end;

// SetGLState
//
procedure TGLStateCache.SetGLState(const aState : TGLState);
begin
	if not (aState in FStates) then begin
		Include(FStates, aState);
		glEnable(cGLStateToGLEnum[aState]);
	end;
end;

// UnSetGLState
//
procedure TGLStateCache.UnSetGLState(const aState : TGLState);
begin
	if (aState in FStates) then begin
		Exclude(FStates, aState);
		glDisable(cGLStateToGLEnum[aState]);
	end;
end;

// SetGLPolygonMode
//
procedure TGLStateCache.SetGLPolygonMode(const aFace, mode : TGLEnum);
begin
   case aFace of
      GL_FRONT :
         if mode<>FLastFrontMode then begin
            FLastFrontMode:=mode;
            glPolygonMode(aFace, mode);
         end;
      GL_BACK :
         if mode<>FLastBackMode then begin
            FLastBackMode:=mode;
            glPolygonMode(aFace, mode);
         end;
      GL_FRONT_AND_BACK :
         if (mode<>FLastFrontMode) or (mode<>FLastBackMode) then begin
            FLastFrontMode:=mode;
            FLastBackMode:=mode;
            glPolygonMode(aFace, mode);
         end;
   end;
end;

// ResetGLPolygonMode
//
procedure TGLStateCache.ResetGLPolygonMode ;
begin
   FLastFrontMode:=0;
   FLastBackMode:=0;
end;

// SetGLMaterialColors
//
procedure TGLStateCache.SetGLMaterialColors(const aFace : TGLEnum;
                              const emission, ambient, diffuse, specular : TVector;
                              const shininess : Integer);
var
   i : Integer;
begin
   i:=aFace-GL_FRONT;
   if FFrontBackShininess[i]<>shininess then begin
    	glMateriali(AFace, GL_SHININESS, shininess);
      FFrontBackShininess[i]:=shininess;
   end;
   if not AffineVectorEquals(FFrontBackColors[i][0], emission) then begin
     	glMaterialfv(aFace, GL_EMISSION, @emission);
      SetVector(FFrontBackColors[i][0], emission);
   end;
   if not AffineVectorEquals(FFrontBackColors[i][1], ambient) then begin
     	glMaterialfv(aFace, GL_AMBIENT, @ambient);
      SetVector(FFrontBackColors[i][1], ambient);
   end;
   if not VectorEquals(FFrontBackColors[i][2], diffuse) then begin
     	glMaterialfv(aFace, GL_DIFFUSE, @diffuse);
      SetVector(FFrontBackColors[i][2], diffuse);
   end;
   if not AffineVectorEquals(FFrontBackColors[i][3], specular) then begin
     	glMaterialfv(aFace, GL_SPECULAR, @specular);
      SetVector(FFrontBackColors[i][3], specular);
   end;
end;

// SetGLMaterialAlphaChannel
//
procedure TGLStateCache.SetGLMaterialAlphaChannel(const aFace : TGLEnum; const alpha : TGLFloat);
var
   i : Integer;
begin
   i:=aFace-GL_FRONT;
   if FFrontBackColors[i][2][3]<>alpha then begin
      FFrontBackColors[i][2][3]:=alpha;
     	glMaterialfv(aFace, GL_DIFFUSE, @FFrontBackColors[i][2]);
   end;
end;

// ResetGLMaterialColors
//
procedure TGLStateCache.ResetGLMaterialColors;
const
   clrBlack  : TVector = (0,    0,    0,    1);
   clrGray20 : TVector = (0.20, 0.20, 0.20, 1);
   clrGray80 : TVector = (0.80, 0.80, 0.80, 1);
begin
  	glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @clrGray20);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @clrGray80);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @clrBlack);
  	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @clrBlack);
 	glMateriali(GL_FRONT_AND_BACK,  GL_SHININESS, 0);
   FillChar(FFrontBackColors, SizeOf(FFrontBackColors), 127);
   FFrontBackShininess[0]:=0;
   FFrontBackShininess[1]:=0;
end;

// SetGLCurrentTexture
//
procedure TGLStateCache.SetGLCurrentTexture(const textureUnit, target, handle : Integer);
begin
   if handle<>FTextureHandle[textureUnit] then begin
      glBindTexture(target, handle);
      FTextureHandle[textureUnit]:=handle;
   end;
end;

// ResetGLCurrentTexture
//
procedure TGLStateCache.ResetGLCurrentTexture;
var
   i : Integer;
begin
   for i:=0 to 7 do
      FTextureHandle[i]:=-1;
end;

// SetGLTextureMatrix
//
procedure TGLStateCache.SetGLTextureMatrix(const matrix : TMatrix);
begin
   FTextureMatrixIsIdenty:=False;
   glMatrixMode(GL_TEXTURE);
   glLoadMatrixf(PGLFloat(@matrix[0][0]));
   glMatrixMode(GL_MODELVIEW);
end;

// ResetGLTextureMatrix
//
procedure TGLStateCache.ResetGLTextureMatrix;
begin
   if not FTextureMatrixIsIdenty then begin
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
      FTextureMatrixIsIdenty:=True;
   end;
end;

// InvertGLFrontFace
//
procedure TGLStateCache.InvertGLFrontFace;
begin
   FFrontFaceCCW:=not FFrontFaceCCW;
   if FFrontFaceCCW then
      glFrontFace(GL_CCW)
   else glFrontFace(GL_CW);
end;

// ResetGLFrontFace
//
procedure TGLStateCache.ResetGLFrontFace;
begin
   glFrontFace(GL_CCW);
   FFrontFaceCCW:=True;
end;

// SetGLFrontFaceCW
//
procedure TGLStateCache.SetGLFrontFaceCW;
begin
   if FFrontFaceCCW then begin
      glFrontFace(GL_CW);
      FFrontFaceCCW:=False;
   end;
end;

// SetGLFrontFaceCCW
//
procedure TGLStateCache.SetGLFrontFaceCCW;
begin
   if not FFrontFaceCCW then begin
      glFrontFace(GL_CCW);
      FFrontFaceCCW:=True;
   end;
end;

// ResetAll
//
procedure TGLStateCache.ResetAll;
begin
   ResetGLPolygonMode;
   ResetGLMaterialColors;
   ResetGLCurrentTexture;
   ResetGLFrontFace;
end;

end.
