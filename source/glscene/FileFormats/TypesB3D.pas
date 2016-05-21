{: TypesB3D<p>

	Types used on the B3D file loader<p>

	<b>History :</b><font size=-1><ul>
	   <li>22/12/05 - Mathx - Added to the GLScene Project.
	</ul></font>
}
unit TypesB3D;

interface

uses
  VectorTypes, VectorGeometry;

type
  TB3DChunkType = (bctUnknown, bctHeader, bctTexture, bctBrush, bctNode, bctVertex, bctTriangle,
    bctMesh, bctBone, bctKeyFrame, bctAnimation);

  PB3DChunk = ^TB3DChunk;
  TB3DChunk = record
    chunk: array[0..3] of char;
    length: Integer;
  end;

  PBB3DChunk = ^TBB3DChunk;
  TBB3DChunk = record
    Version: Integer;
  end;

  PTEXSChunk = ^TTEXSChunk;
  TTEXSChunk = record
    fileName: array[0..255] of char; //texture file name this is the filename of the texture, ie "wall.bmp"  Has to be in the local Directory
    flags, blend: Integer;  //blitz3D TextureFLags and TextureBlend: default=1,2
  		            //these are the same as far as I know as the flags for a texture in Blitz3D
    x_pos, y_pos: Single;   //x and y position of texture: default=0,0
    x_scale, y_scale: Single; //x and y scale of texture: default=1,1
    rotation: Single;         //rotation of texture (in radians): default=0 radian = 180/pi degrees
  end;

  PBRUSChunk = ^TBRUSChunk;
  TBRUSChunk = record
    n_texs: Integer;
    name: array[0..255] of Char; //eg "WATER" - just use texture name by default
    red, green, blue, alpha: Single;  //Blitz3D Brushcolor and Brushalpha: default=1,1,1,1
    shininess: Single; //Blitz3D BrushShininess: default=0
    blend, fx: Integer; //Blitz3D Brushblend and BrushFX: default=1,0
    texture_id: array of Integer; //textures used in brush, ie if there is more then one texture used, ie Alphamaps, colour maps etc,
                                  //you put all ID's here as ints.
  end;

  PVertexData = ^TVertexData;
  TVertexData = record
    next: PVertexData;
    x, y, z: Single; //always present
    nx, ny, nz: Single; //vertex normal: present if (flags&1)
    red, green, blue, alpha: Single; //vertex color: present if (flags&2)
    tex_coords: array of Single; //tex coords
  end;

  PVRTSChunk = ^TVRTSChunk;
  TVRTSChunk = record
    flags: Integer; //1=normal values present, 2=rgba values present
    tex_coord_sets: Integer; //texture coords per vertex (eg: 1 for simple U/V) max=8
    tex_coord_set_size: Integer; //components per set (eg: 2 for simple U/V) max=4
    vertices: PVertexData;
  end;

  PTRISChunk = ^TTRISChunk;
  TTRISChunk = record
    next: PTRISChunk;
    brush_id: Integer; //brush applied to these TRIs: default=-1
    vertex_id: array of Integer; //vertex indices
  end;

  PMESHChunk = ^TMESHChunk;
  TMESHChunk = record
    brush_id: Integer; //'master' brush: default=-1
    vertices: TVRTSChunk;  //vertices
    triangles: PTRISChunk;  //1 or more sets of triangles
  end;

  PBONEChunk = ^TBONEChunk;
  TBONEChunk = record
    vertex_id: Integer; //vertex affected by this bone
    weight: Single; //;how much the vertex is affected
  end;

  PKEYSChunk = ^TKEYSChunk;
  TKEYSChunk = record
    next: PKEYSChunk;
    flags: Integer; //1=position, 2=scale, 4=rotation
    frame: Integer; //where key occurs
    position: TAffineVector; //present if (flags&1)
    scale: TAffineVector; //present if (flags&2)
    rotation: TVector; //present if (flags&4)
  end;

  PANIMChunk = ^TANIMChunk;
  TANIMChunk = record
    flags: Integer; //unused: default=0
    frames: Integer; //how many frames in anim
    fps: Single; //default=60
  end;

  PNODEChunk = ^TNODEChunk;
  TNODEChunk = record
    name: array[0..255] of char; //name of node
    position: TAffineVector;  //local...
    scale: TAffineVector; //coord...
    rotation: TVector; //system...
    //array of node elements
    //should be one of meshes or bones, support meshes only for now
    meshes: PMESHChunk; //what 'kind' of node this is - if unrecognized, just use a Blitz3D pivot.
    {
    not supprot yet
    bones: PBONEChunk;
    }
    keys: PKEYSChunk; //optional animation keys
    nodes: PNODEChunk; //optional child nodes
    animation: TANIMChunk; //optional animation
    next: PNODEChunk; //point to the next node
    level: Integer;
  end;

implementation

end.
