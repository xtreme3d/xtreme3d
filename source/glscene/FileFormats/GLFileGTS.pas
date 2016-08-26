//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileGTS<p>

	GTS (GNU Triangulated Surface) vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>05/06/03 - SG - Separated from GLVectorFileObjects.pas
	</ul></font>
}
unit GLFileGTS;

interface

uses
  Classes, GLVectorFileObjects, ApplicationFileIO, GLMisc;

type
   // TGLGTSVectorFile
   //
   {: The GTS vector file (GNU Triangulated Surface library).<p>
      It is a simple text format, with indexed vertices. The first line contains
      the number of vertices, the number of edges and the number of faces separated
      by spaces.<br>
      Following lines contain the x/y/z coordinates of vertices, then the edges
      (two indices) and the faces (three indices).<br>
      http://gts.sourceforge.net/ }
   TGLGTSVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         class function Capabilities : TDataFileCapabilities; override;
         procedure LoadFromStream(aStream : TStream); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLUtils;

// ------------------
// ------------------ TGLGTSVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLGTSVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLGTSVectorFile.LoadFromStream(aStream : TStream);
var
   i, nv, ne, nf, k, ei : Integer;
   sl : TStringList;
   mesh : TMeshObject;
   fg : TFGVertexIndexList;
   buf : String;
   vertIndices : array [0..5] of Integer;
   pEdge, pTri, p : PChar;
begin
   sl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Mode:=momFaceGroups;
      if sl.Count>0 then begin
         p:=PChar(sl[0]);
         nv:=ParseInteger(p);
         ne:=ParseInteger(p);
         nf:=ParseInteger(p);
         if (nv or nf or ne)=0 then Exit;
         for i:=1 to nv do begin
            p:=PChar(sl[i]);
            mesh.Vertices.Add(ParseFloat(p), ParseFloat(p), ParseFloat(p));
         end;
         fg:=TFGVertexIndexList.CreateOwned(mesh.FaceGroups);
         for i:=1+nv+ne to nv+ne+nf do begin
            pTri:=PChar(sl[i]);
            for k:=0 to 2 do begin
               ei:=ParseInteger(pTri);
               buf:=sl[nv+ei];
               pEdge:=PChar(sl[nv+ei]);
               vertIndices[k*2+0]:=ParseInteger(pEdge);
               vertIndices[k*2+1]:=ParseInteger(pEdge);
            end;
            if (vertIndices[0]=vertIndices[2]) or (vertIndices[0]=vertIndices[3]) then
               fg.VertexIndices.Add(vertIndices[0]-1)
            else fg.VertexIndices.Add(vertIndices[1]-1);
            if (vertIndices[2]=vertIndices[4]) or (vertIndices[2]=vertIndices[5]) then
               fg.VertexIndices.Add(vertIndices[2]-1)
            else fg.VertexIndices.Add(vertIndices[3]-1);
            if (vertIndices[4]=vertIndices[0]) or (vertIndices[4]=vertIndices[1]) then
               fg.VertexIndices.Add(vertIndices[4]-1)
            else fg.VertexIndices.Add(vertIndices[5]-1);
         end;
         mesh.BuildNormals(fg.VertexIndices, momTriangles);
      end;
   finally
      sl.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('gts', 'GNU Triangulated Surface', TGLGTSVectorFile);

end.