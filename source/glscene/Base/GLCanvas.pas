//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLCanvas<p>

	Implements a basic Canvas-like interface over for OpenGL.<p>
   This class can be used for generic OpenGL applications and has no dependencies
   to the GLScene core units (only to base units).<p>

	<b>History : </b><font size=-1><ul>
      <li>08/07/04 - LR - Replace Graphics and TPoint by GLCrossPlatform for Linux
      <li>13/01/04 - EG - Polyline/Polygon fix
      <li>07/05/03 - EG - SetPenWidth now correctly stops the primitive
      <li>08/01/03 - EG - StopPrimitive now public
      <li>09/12/02 - EG - Now properly disables fog
      <li>20/11/02 - EG - Now uses Types/Windows TPoint (D5 & D6 tested only) 
      <li>01/10/02 - EG - Added Polygon & Polyline
      <li>04/03/02 - EG - Added FrameRect and FillRect
      <li>31/01/02 - EG - Texture3D/CubeMap only disabled if supported
      <li>24/01/02 - EG - Added PenAlpha
      <li>19/01/02 - EG - Creation
	</ul></font>
}
unit GLCanvas;

interface

{$i GLScene.inc}

uses Classes, VectorGeometry, GLCrossPlatform;

type

   TColor = Integer;

	// TGLCanvas
	//
   {: A simple Canvas-like interface for OpenGL.<p>
      This class implements a small "shell" for 2D operations in OpenGL,
      it operates over the current OpenGL context and provides methods
      for drawing lines, ellipses and points.<br>
      This class is typically used by creating an instance, using it for drawing,
      and freeing the instance. When drawing (0, 0) is the top left corner.<br>
      All coordinates are internally maintained with floating point precisoion.<p>
      Several states are cached and it is of primary importance not to invoke
      OpenGL directly throughout the life of an instance (at the cost of
      unespected behaviour). }
	TGLCanvas = class
	   private
	      { Private Declarations }
         FBufferSizeX, FBufferSizeY : Integer;
         FColorBackup : TVector;
         FPointSizeBackup, FLineWidthBackup : Single;

         FLastPrimitive : Integer;
         FCurrentPos : TAffineVector;
         FPenColor : TColor;
         FPenWidth : Integer;
         FCurrentPenColorVector : TVector;

	   protected
	      { Protected Declarations }
	      procedure BackupOpenGLStates;
	      procedure RestoreOpenGLStates;

	      procedure StartPrimitive(const primitiveType : Integer);

         procedure EllipseVertices(x, y, xRadius, yRadius : Single);

         procedure SetPenColor(const val : TColor);
         procedure SetPenAlpha(const val : Single);
         procedure SetPenWidth(const val : Integer);

      public
	      { Public Declarations }
	      constructor Create(bufferSizeX, bufferSizeY : Integer;
                            const baseTransform : TMatrix); overload;
	      constructor Create(bufferSizeX, bufferSizeY : Integer); overload;
	      destructor Destroy; override;

         {: Stops the current internal primitive.<p>
            This function is invoked automatically by TGLCanvas when changeing
            primitives, you should directly call if you want to render your
            own stuff intertwined with TGLCanvas drawings. In that case, call
            it before your own OpenGL calls. }
         procedure StopPrimitive;

         {: Inverts the orientation of the Y Axis.<p>
            If (0, 0) was in the top left corner, it will move to the bottom
            left corner or vice-versa. }
         procedure InvertYAxis;

         property CanvasSizeX : Integer read FBufferSizeX;
         property CanvasSizeY : Integer read FBufferSizeY;

         {: Current Pen Color. }
         property PenColor : TColor read FPenColor write SetPenColor;
         {: Current Pen Alpha channel (from 0.0 to 1.0) }
         property PenAlpha : Single read FCurrentPenColorVector[3] write SetPenAlpha;
         {: Current Pen Width. }
         property PenWidth : Integer read FPenWidth write SetPenWidth;

         {: Updates the current position (absolute coords). }
	      procedure MoveTo(const x, y : Integer); overload;
	      procedure MoveTo(const x, y : Single); overload;
         {: Updates the current position (relative coords). }
	      procedure MoveToRel(const x, y : Integer); overload;
	      procedure MoveToRel(const x, y : Single); overload;

         {: Draws a line from current position to given coordinate.<p>
            Current position is updated. }
	      procedure LineTo(const x, y : Integer); overload;
	      procedure LineTo(const x, y : Single); overload;
	      procedure LineToRel(const x, y : Integer); overload;
	      procedure LineToRel(const x, y : Single); overload;
         {: Draws a line from (x1, y1) to (x2, y2).<p>
            The current position is NOT updated. }
	      procedure Line(const x1, y1, x2, y2 : Integer); overload;
	      procedure Line(const x1, y1, x2, y2 : Single); overload;

         {: Draws the set of lines defined by connecting the points.<p>
            Similar to invoking MoveTo on the first point, then LineTo
            on all the following points. }
         procedure Polyline(const points : array of TGLPoint);
         {: Similar to Polyline but also connects the last point to the first. }
         procedure Polygon(const points : array of TGLPoint);

         {: Plots a pixel at given coordinate.<p>
            PenWidth affects pixel size.<br>
            The current position is NOT updated. }
  	      procedure PlotPixel(const x, y : Integer); overload;
	      procedure PlotPixel(const x, y : Single); overload;

         {: Draw the (x1,y1)-(x2, y2) rectangle's frame (border). }
         procedure FrameRect(const x1, y1, x2, y2 : Integer); overload;
         procedure FrameRect(const x1, y1, x2, y2 : Single); overload;
         {: Draw the (x1,y1)-(x2, y2) rectangle (filled with PenColor). }
         procedure FillRect(const x1, y1, x2, y2 : Integer); overload;
         procedure FillRect(const x1, y1, x2, y2 : Single); overload;

         {: Draws an ellipse with (x1,y1)-(x2, y2) bounding rectangle. }
	      procedure Ellipse(const x1, y1, x2, y2 : Integer); overload;
         {: Draws and ellipse centered at (x, y) with given radiuses. }
	      procedure Ellipse(const x, y : Integer; const xRadius, yRadius : Single); overload;
	      procedure Ellipse(x, y, xRadius, yRadius : Single); overload;
         {: Draw a filled ellipse. }
	      procedure FillEllipse(const x, y : Integer; const xRadius, yRadius : Single); overload;
	end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses OpenGL1x;

const
   cNoPrimitive = MaxInt;

// ConvertColorVector
//
function ConvertColorVector(const aColor: TVector): TColor;
begin
  Result:=   (Round(255 * AColor[2]) shl 16)
          or (Round(255 * AColor[1]) shl 8)
          or  Round(255 * AColor[0]);
end;

// ConvertWinColor
//
function ConvertWinColor(aColor : TColor; alpha : Single = 1) : TVector;
var
   winColor : Integer;
begin
	// Delphi color to Windows color
   winColor:=ColorToRGB(AColor);
   // convert 0..255 range into 0..1 range
   Result[0]:=(winColor and $FF)*(1/255);
   Result[1]:=((winColor shr 8) and $FF)*(1/255);
   Result[2]:=((winColor shr 16) and $FF)*(1/255);
   Result[3]:=alpha;
end;

// ------------------
// ------------------ TGLCanvas ------------------
// ------------------

// Create
//
constructor TGLCanvas.Create(bufferSizeX, bufferSizeY : Integer;
                             const baseTransform : TMatrix);
begin
   FBufferSizeX:=bufferSizeX;
   FBufferSizeY:=bufferSizeY;
   
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   gluOrtho2D(0, bufferSizeX, bufferSizeY, 0);

   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@baseTransform);

   BackupOpenGLStates;

   FLastPrimitive:=cNoPrimitive;
end;

// Create
//
constructor TGLCanvas.Create(bufferSizeX, bufferSizeY : Integer);
begin
   Create(bufferSizeX, bufferSizeY, IdentityHmgMatrix);
end;

// Destroy
//
destructor TGLCanvas.Destroy;
begin
   StopPrimitive;

   RestoreOpenGLStates;

   glMatrixMode(GL_PROJECTION);
   glPopMatrix;

   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
end;

// BackupOpenGLStates
//
procedure TGLCanvas.BackupOpenGLStates;
begin
   glPushAttrib(GL_ENABLE_BIT);

   glDisable(GL_LIGHTING);
   glDisable(GL_FOG);
   glDisable(GL_CULL_FACE);
   glDisable(GL_COLOR_MATERIAL);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_TEXTURE_1D);
   glDisable(GL_TEXTURE_2D);
   if GL_EXT_texture3D then
      glDisable(GL_TEXTURE_3D);
   if GL_ARB_texture_cube_map then
      glDisable(GL_TEXTURE_CUBE_MAP_ARB);
   glDisable(GL_LINE_SMOOTH);
   glDisable(GL_POINT_SMOOTH);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

   // Setup and backup pen stuff
   glGetFloatv(GL_CURRENT_COLOR, @FColorBackup);
   FPenColor:=clBlack;
   SetVector(FCurrentPenColorVector, NullHmgPoint);
   glColor4fv(@FCurrentPenColorVector);
   glGetFloatv(GL_LINE_WIDTH, @FLineWidthBackup);
   glGetFloatv(GL_POINT_SIZE, @FPointSizeBackup);
   FPenWidth:=1;
   glLineWidth(1);
   glPointSize(1);
end;

// RestoreOpenGLStates
//
procedure TGLCanvas.RestoreOpenGLStates;
begin
   glColor4fv(@FColorBackup);
   glLineWidth(FLineWidthBackup);
   glPointSize(FPointSizeBackup);

   glPopAttrib;
end;

// StartPrimitive
//
procedure TGLCanvas.StartPrimitive(const primitiveType : Integer);
begin
   if primitiveType<>FLastPrimitive then begin
      if FLastPrimitive<>cNoPrimitive then
         glEnd;
      if primitiveType<>cNoPrimitive then
         glBegin(primitiveType);
      FLastPrimitive:=primitiveType;
   end;
end;

// StopPrimitive
//
procedure TGLCanvas.StopPrimitive;
begin
   StartPrimitive(cNoPrimitive);
end;

// InvertYAxis
//
procedure TGLCanvas.InvertYAxis;
var
   mat : TMatrix;
begin
   mat:=IdentityHmgMatrix;
   mat[1][1]:=-1;
   mat[3][1]:=FBufferSizeY;
   glMultMatrixf(@mat);
//   glTranslatef(0, FBufferSizeY, 0);
//   glScalef(0, -1, 0);
end;

// SetPenColor
//
procedure TGLCanvas.SetPenColor(const val : TColor);
begin
   if val<>FPenColor then begin
      SetVector(FCurrentPenColorVector, ConvertWinColor(val, FCurrentPenColorVector[3]));
      FPenColor:=val;
      glColor4fv(@FCurrentPenColorVector);
   end;
end;

// SetPenAlpha
//
procedure TGLCanvas.SetPenAlpha(const val : Single);
begin
   if val<>FCurrentPenColorVector[3] then begin
      FCurrentPenColorVector[3]:=val;
      glColor4fv(@FCurrentPenColorVector);
   end;
end;

// SetPenWidth
//
procedure TGLCanvas.SetPenWidth(const val : Integer);
begin
   if val<1 then Exit;
   if val<>FPenWidth then begin
      FPenWidth:=val;
      StopPrimitive;
      glLineWidth(val);
      glPointSize(val);
   end;
end;

// MoveTo
//
procedure TGLCanvas.MoveTo(const x, y : Integer);
begin
   FCurrentPos[0]:=x;
   FCurrentPos[1]:=y;
end;

// MoveTo
//
procedure TGLCanvas.MoveTo(const x, y : Single);
begin
   FCurrentPos[0]:=x;
   FCurrentPos[1]:=y;
end;

// MoveToRel
//
procedure TGLCanvas.MoveToRel(const x, y : Integer);
begin
   FCurrentPos[0]:=FCurrentPos[0]+x;
   FCurrentPos[1]:=FCurrentPos[1]+y;
end;

// MoveToRel
//
procedure TGLCanvas.MoveToRel(const x, y : Single);
begin
   FCurrentPos[0]:=FCurrentPos[0]+x;
   FCurrentPos[1]:=FCurrentPos[1]+y;
end;

// LineTo
//
procedure TGLCanvas.LineTo(const x, y : Integer);
begin
   StartPrimitive(GL_LINES);
   glVertex2fv(@FCurrentPos);
   MoveTo(x, y);
   glVertex2fv(@FCurrentPos);
end;

// LineTo
//
procedure TGLCanvas.LineTo(const x, y : Single);
begin
   StartPrimitive(GL_LINES);
   glVertex2fv(@FCurrentPos);
   MoveTo(x, y);
   glVertex2fv(@FCurrentPos);
end;

// LineToRel
//
procedure TGLCanvas.LineToRel(const x, y : Integer);
begin
   LineTo(FCurrentPos[0]+x, FCurrentPos[1]+y);
end;

// LineToRel
//
procedure TGLCanvas.LineToRel(const x, y : Single);
begin
   LineTo(FCurrentPos[0]+x, FCurrentPos[1]+y);
end;

// Line
//
procedure TGLCanvas.Line(const x1, y1, x2, y2 : Integer);
begin
   StartPrimitive(GL_LINES);
   glVertex2i(x1, y1);
   glVertex2i(x2, y2);
end;

// Line
//
procedure TGLCanvas.Line(const x1, y1, x2, y2 : Single);
begin
   StartPrimitive(GL_LINES);
   glVertex2f(x1, y1);
   glVertex2f(x2, y2);
end;

// Polyline
//
procedure TGLCanvas.Polyline(const points : array of TGLPoint);
var
   i, n : Integer;
begin
   n:=Length(Points);
   if n>1 then begin
      StartPrimitive(GL_LINE_STRIP);
      glVertex2iv(@points[Low(points)]);
      for i:=Low(points)+1 to High(points) do
         glVertex2iv(@points[i]);
      StopPrimitive;
   end;
end;

// Polygon
//
procedure TGLCanvas.Polygon(const points : array of TGLPoint);
var
   i, n : Integer;
begin
   n:=Length(Points);
   if n>1 then begin
      StartPrimitive(GL_LINE_LOOP);
      glVertex2iv(@points[Low(points)]);
      for i:=Low(points)+1 to High(points) do
         glVertex2iv(@points[i]);
      StopPrimitive;
   end;
end;

// PlotPixel
//
procedure TGLCanvas.PlotPixel(const x, y : Integer);
begin
   StartPrimitive(GL_POINTS);
   glVertex2i(x, y);
end;

// PlotPixel
//
procedure TGLCanvas.PlotPixel(const x, y : Single);
begin
   StartPrimitive(GL_POINTS);
   glVertex2f(x, y);
end;

// FrameRect (integer)
//
procedure TGLCanvas.FrameRect(const x1, y1, x2, y2 : Integer);
begin
   StartPrimitive(GL_LINE_LOOP);
   glVertex2i(x1, y1);  glVertex2i(x2, y1);
   glVertex2i(x2, y2);  glVertex2i(x1, y2);
   StopPrimitive;
end;

// FrameRect (single)
//
procedure TGLCanvas.FrameRect(const x1, y1, x2, y2 : Single);
begin
   StartPrimitive(GL_LINE_LOOP);
   glVertex2f(x1, y1);  glVertex2f(x2, y1);
   glVertex2f(x2, y2);  glVertex2f(x1, y2);
   StopPrimitive;
end;

// FillRect (integer)
//
procedure TGLCanvas.FillRect(const x1, y1, x2, y2 : Integer);
begin
   StartPrimitive(GL_QUADS);
   glVertex2i(x1, y1);  glVertex2i(x2, y1);
   glVertex2i(x2, y2);  glVertex2i(x1, y2);
end;

// FillRect (single)
//
procedure TGLCanvas.FillRect(const x1, y1, x2, y2 : Single);
begin
   StartPrimitive(GL_QUADS);
   glVertex2f(x1, y1);  glVertex2f(x2, y1);
   glVertex2f(x2, y2);  glVertex2f(x1, y2);
end;

// EllipseVertices
//
procedure TGLCanvas.EllipseVertices(x, y, xRadius, yRadius : Single);
var
   i, n : Integer;
   s, c : TSingleArray;
begin
   n:=Round(MaxFloat(xRadius, yRadius)*0.1)+5;
   SetLength(s, n);
   SetLength(c, n);
   Dec(n);
   PrepareSinCosCache(s, c, 0, 90);
   ScaleFloatArray(s, yRadius);
   ScaleFloatArray(c, xRadius);
   // first quadrant (top right)
   for i:=0 to n do
      glVertex2f(x+c[i], y-s[i]);
   // second quadrant (top left)
   for i:=n-1 downto 0 do
      glVertex2f(x-c[i], y-s[i]);
   // third quadrant (bottom left)
   for i:=1 to n do
      glVertex2f(x-c[i], y+s[i]);
   // fourth quadrant (bottom right)
   for i:=n-1 downto 0 do
      glVertex2f(x+c[i], y+s[i]);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(const x1, y1, x2, y2 : Integer);
begin
   Ellipse((x1+x2)*0.5, (y1+y2)*0.5, Abs(x2-x1)*0.5, Abs(y2-y1)*0.5);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(const x, y : Integer; const xRadius, yRadius : Single);
var
   sx, sy : Single;
begin
   sx:=x; sy:=y;
   Ellipse(sx, sy, xRadius, yRadius);
end;

// Ellipse
//
procedure TGLCanvas.Ellipse(x, y, xRadius, yRadius : Single);
begin
   StartPrimitive(GL_LINE_STRIP);
   EllipseVertices(x, y, xRadius, yRadius);
   StopPrimitive;
end;

// FillEllipse
//
procedure TGLCanvas.FillEllipse(const x, y : Integer; const xRadius, yRadius : Single);
begin
   StartPrimitive(GL_TRIANGLE_FAN);
   glVertex2f(x, y); // not really necessary, but may help with memory stride
   EllipseVertices(x, y, xRadius, yRadius);
   StopPrimitive;
end;

end.
