{: GLFileNurbs<p>

	Nurbs surfaces vector file loading.<p>

	<b>History :</b><font size=-1><ul>
      <li>11/08/03 - SG - Some minor changes
      <li>05/08/03 - SG - Initial, adapted LoadFromStream from earlier tests
                          with GLNurbsSurface (depricated), originally coded
                          by Eric Grange.
	</ul></font>
}
unit GLFileNurbs;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, VectorGeometry, VectorLists, ApplicationFileIO,
  GLParametricSurfaces, GLUtils;

type

   // TGLNurbsSurface
   //
   TGLNurbsVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;
         procedure LoadFromStream(stream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLNurbsVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLNurbsVectorFile.Capabilities : TDataFileCapabilities;
begin
  Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLNurbsVectorFile.LoadFromStream(stream : TStream);

   function CleanupLine(const line : String) : String;
   var
      p : Integer;
   begin
      p:=Pos('#', line);
      if p>0 then
         Result:=LowerCase(Trim(Copy(line, 1, p-1)))
      else Result:=LowerCase(Trim(line));
   end;

   function ReadSingleArray(sl : TStrings; idx : Integer; list : TSingleList) : Integer;
   var
      k : Integer;
      buf : String;
      vals : TStringList;
   begin
      vals:=TStringList.Create;
      try
         while idx<sl.Count do begin
            buf:=CleanupLine(sl[idx]);
            if buf=']' then Break;
            vals.CommaText:=buf;
            for k:=0 to vals.Count-1 do if vals[k]<>'' then
               list.Add(StrToFloatDef(vals[k],0));
            Inc(idx);
         end;
         Result:=idx;
      finally
         vals.Free;
      end;
   end;

   function ReadVectorArray(sl : TStrings; idx : Integer; list : TAffineVectorList) : Integer;
   var
      buf : String;
      vals : TStringList;
   begin
      vals:=TStringList.Create;
      try
         while idx<sl.Count do begin
            buf:=CleanupLine(sl[idx]);
            if buf=']' then Break;
            vals.CommaText:=buf;
            if vals.Count>=3 then
               list.Add(StrToFloatDef(vals[0],0), StrToFloatDef(vals[1],0), StrToFloatDef(vals[2],0));
            Inc(idx);
         end;
         Result:=idx;
      finally
         vals.Free;
      end;
   end;

var
   sl, buf : TStringList;
   ss : TStringStream;
   i,j : Integer;
   surface : TMOParametricSurface;
   invert : Boolean;
   invControlPoints : TAffineVectorList;
begin
   ss:=TStringStream.Create('');
   sl:=TStringList.Create;
   buf:=TStringList.Create;

   surface:=TMOParametricSurface.CreateOwned(Owner.MeshObjects);
   with surface do begin
      Name:='Nurbs'+IntToStr(Owner.IndexOf(surface));
      Basis:=psbBSpline;
      Renderer:=psrOpenGL;
      AutoKnots:=False;
   end;
   
   invert:=False;

   try
      ss.CopyFrom(stream, stream.Size-stream.Position);
      sl.Text:=ss.DataString;

      i:=0; while i<sl.Count do begin
         buf.CommaText:=CleanupLine(sl[i]);
         if buf.Count>1 then begin
            if buf[0]='uorder' then
               surface.OrderU:=StrToIntDef(buf[1], 2)
            else if buf[0]='vorder' then
               surface.OrderV:=StrToIntDef(buf[1], 2)
            else if buf[0]='uknot' then
               i:=ReadSingleArray(sl, i+1, surface.KnotsU)
            else if buf[0]='vknot' then
               i:=ReadSingleArray(sl, i+1, surface.KnotsV)
            else if buf[0]='weight' then
               i:=ReadSingleArray(sl, i+1, surface.Weights)
            else if buf[0]='udimension' then
               surface.CountU:=StrToIntDef(buf[1], 0)
            else if buf[0]='vdimension' then
               surface.CountV:=StrToIntDef(buf[1], 0)
            else if buf[0]='controlpoint' then
               i:=ReadVectorArray(sl, i+1, Surface.ControlPoints)
            else if buf[0]='ccw' then
               invert:=(buf[1]='false');
         end;
         Inc(i);
      end;
      
      if invert then begin
         invControlPoints:=TAffineVectorList.Create;
         for i:=surface.CountV-1 downto 0 do
           for j:=0 to surface.CountU-1 do
             invControlPoints.Add(surface.ControlPoints[i*surface.CountU+j]);
         surface.ControlPoints.Assign(invControlPoints);
         invControlPoints.Free;
      end;
      
   finally
      buf.Free;
      sl.Free;
      ss.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('nurbs', 'Nurbs model files', TGLNurbsVectorFile);

end.
