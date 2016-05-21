//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileTIN<p>

	TIN (Triangular Irregular Network) vector file format implementation.<p>

	<b>History :</b><font size=-1><ul>
      <li>05/06/03 - SG - Separated from GLVectorFileObjects.pas
	</ul></font>
}
unit GLFileTIN;

interface

uses
  Classes, SysUtils, GLVectorFileObjects, ApplicationFileIO, VectorGeometry;

type
   // TGLTINVectorFile
   //
   {: The TIN vector file (triangle irregular network).<p>
      It is a simple text format, with one triangle record per line, no materials,
      no texturing (there may be more, but I never saw anything in this files).<p>
      This format is encountered in the DEM/DTED world and used in place of grids. }
   TGLTINVectorFile = class(TVectorFile)
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

uses GLMisc, GLUtils;

// ------------------
// ------------------ TGLTINVectorFile ------------------
// ------------------

// Capabilities
//
class function TGLTINVectorFile.Capabilities : TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

// LoadFromStream
//
procedure TGLTINVectorFile.LoadFromStream(aStream : TStream);
var
   i : Integer;
   sl, tl : TStringList;
   mesh : TMeshObject;
   v1, v2, v3, n : TAffineVector;
begin
   sl:=TStringList.Create;
   tl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      mesh:=TMeshObject.CreateOwned(Owner.MeshObjects);
      mesh.Mode:=momTriangles;
      for i:=0 to sl.Count-1 do if Copy(sl[i], 1, 2)='t ' then begin
         tl.CommaText:=Trim(Copy(sl[i], 3, MaxInt));
         if tl.Count=9 then begin
            SetVector(v1, StrToFloatDef(tl[0]), StrToFloatDef(tl[1]), StrToFloatDef(tl[2]));
            SetVector(v2, StrToFloatDef(tl[3]), StrToFloatDef(tl[4]), StrToFloatDef(tl[5]));
            SetVector(v3, StrToFloatDef(tl[6]), StrToFloatDef(tl[7]), StrToFloatDef(tl[8]));
            mesh.Vertices.Add(v1, v2, v3);
            n:=CalcPlaneNormal(v1, v2, v3);
            mesh.Normals.Add(n, n, n);
         end;
      end;
   finally
      tl.Free;
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

   RegisterVectorFileFormat('tin', 'Triangular Irregular Network', TGLTINVectorFile);

end.