// GLPolyhedron
{: Standard polyhedrons.<p>

	<b>History : </b><font size=-1><ul>
      <li>20/01/04 - SG - Added TGLIcosahedron
      <li>21/07/03 - EG - Creation from GLObjects split
   </ul></font>
}
unit GLPolyhedron;

interface

uses Classes, GLScene, GLTexture, VectorGeometry, OpenGL1x, GLMisc;

type

   // TGLDodecahedron
   //
   {: A Dodecahedron.<p>
      The dodecahedron has no texture coordinates defined, ie. without using
      a texture generation mode, no texture will be mapped. }
   TGLDodecahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TGLIcosahedron
   //
   {: A Icosahedron.<p>
      The icosahedron has no texture coordinates defined, ie. without using
      a texture generation mode, no texture will be mapped. }
   TGLIcosahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses GLObjects;

// ------------------
// ------------------ TGLDodecahedron ------------------
// ------------------

// BuildList
//
procedure TGLDodecahedron.BuildList(var rci : TRenderContextInfo);
begin
   DodecahedronBuildList;
end;

// ------------------
// ------------------ TGLIcosahedron ------------------
// ------------------

// BuildList
//
procedure TGLIcosahedron.BuildList(var rci : TRenderContextInfo);
begin
   IcosahedronBuildList;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLDodecahedron, TGLIcosahedron]);

end.

