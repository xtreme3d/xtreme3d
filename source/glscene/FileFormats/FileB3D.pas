{: FileB3D<p>

	File streaming class for the B3D loader<p>

	<b>History :</b><font size=-1><ul>
	   <li>22/12/05 - Mathx - Added to the GLScene Project.
	</ul></font>
}
unit FileB3D;

interface

{$R-}

uses
  Classes, TypesB3D, VectorGeometry, VectorTypes, VectorLists;

type
  TB3DMaterial = class
    MaterialData: TBRUSChunk;
    constructor Create;
    destructor Destroy; override;
    function GetMaterialName: string;
  end;

  TB3DTexture = class
    TextureData: TTEXSChunk;
    constructor Create;
    destructor Destroy; override;
    function GetTextureName: string;
  end;

  TB3DNode = class
    NodeData: PNODEChunk;
    constructor Create;
    destructor Destroy; override;
    function GetNodeName: string;
    procedure DestroyNodeData(Node: PNODEChunk);
  end;

  // TFileB3D
  //
  TFileB3D = class
  private
    fTextures: TStringList;
    fMaterials: TStringList;
    fNodes: TB3DNode;
    procedure FreeLists;
    function GetChunkType(aChunk: TB3DChunk): TB3DChunkType;
    function SkipChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
    function ReadTextureChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
    function ReadMaterialChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
    function ReadNodeChunk(aStream: TStream; aChunk: TB3DChunk; Node: PNODEChunk; level: Integer): Integer;
    function ReadMeshChunk(aStream: TStream; aChunk: TB3DChunk; Mesh: PMESHChunk): Integer;
    function ReadVerticesChunk(aStream: TStream; aChunk: TB3DChunk; Vertices: PVRTSChunk): Integer;
    function ReadTrianglesChunk(aStream: TStream; aChunk: TB3DChunk; Triangle: PTRISChunk): Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(aStream : TStream);
    //for test only
    procedure Check;
    
    property Textures: TStringList read fTextures;
    property Materials: TStringList read fMaterials;
    property Nodes: TB3DNode read fNodes;
  end;

implementation

uses
  SysUtils, Windows;

constructor TB3DMaterial.Create;
begin
  inherited Create;
  fillChar(MaterialData, sizeof(TBRUSChunk), 0);
end;

destructor TB3DMaterial.Destroy;
begin
  SetLength(MaterialData.texture_id, 0);
  inherited Destroy;
end;

function TB3DMaterial.GetMaterialName: string;
begin
  SetString(Result, MaterialData.name, strlen(MaterialData.name));
end;

constructor TB3DTexture.Create;
begin
  inherited Create;
  fillChar(TextureData, sizeof(TTEXSChunk), 0);
end;

destructor TB3DTexture.Destroy;
begin
  inherited Destroy;
end;

function TB3DTexture.GetTextureName: string;
begin
  SetString(Result, TextureData.fileName, strlen(TextureData.fileName));
end;

constructor TB3DNode.Create;
begin
  inherited Create;
  NodeData := nil;
end;

destructor TB3DNode.Destroy;
begin
  DestroyNodeData(NodeData);
  inherited Destroy;
end;

function TB3DNode.GetNodeName: string;
begin
  SetString(Result, NodeData^.name, strlen(NodeData^.name));
end;

procedure DeleteVertices(var aVertex: PVertexData);
var
  V: PVertexData;
begin
  while aVertex<>nil do
  begin
    SetLength(aVertex^.tex_coords, 0);
    V := aVertex^.next;
    freeMem(aVertex);
    aVertex := nil;
    DeleteVertices(V);
  end;
end;

procedure DeleteTriangles(var aTriangle: PTRISChunk);
var
  T: PTRISChunk;
begin
  while aTriangle<>nil do
  begin
    SetLength(aTriangle^.vertex_id, 0);
    T := aTriangle^.next;
    freeMem(aTriangle);
    aTriangle := nil;
    DeleteTriangles(T);
  end;
end;

procedure TB3DNode.DestroyNodeData(Node: PNODEChunk);
var
  oldNode, PNode: PNODEChunk;
begin
  PNode := Node;
  while PNode<>nil do
  begin
    if PNode^.meshes<>nil then
    begin
      DeleteTriangles(PNode^.meshes^.triangles);
      DeleteVertices(PNode^.meshes^.vertices.vertices);
      freeMem(PNode^.meshes);
      PNode^.meshes := nil;
    end;
    if PNode^.keys<>nil then
      freeMem(PNode^.keys);
    DestroyNodeData(PNode^.nodes);
    oldNode := PNode;
    PNode := PNode^.next;
    freeMem(oldNode);
  end;
end;

//------------------------------------------------------------------------------
constructor TFileB3D.Create;
begin
  inherited Create;
  fTextures := TStringList.Create;
  fMaterials := TStringList.Create;
  fNodes := TB3DNode.Create;
end;

destructor TFileB3D.Destroy;
begin
  FreeLists;
  fTextures.free;
  fMaterials.free;
  fNodes.free;
  inherited Destroy;
end;

function TFileB3D.GetChunkType(aChunk: TB3DChunk): TB3DChunkType;
begin
  Result := bctUnKnown;
  if StrLIComp(aChunk.chunk, 'BB3D', 4)=0 then
    Result := bctHeader;
  if StrLIComp(aChunk.chunk, 'TEXS', 4)=0 then
    Result := bctTexture;
  if StrLIComp(aChunk.chunk, 'BRUS', 4)=0 then
    Result := bctBrush;
  if StrLIComp(aChunk.chunk, 'NODE', 4)=0 then
    Result := bctNode;
  if StrLIComp(aChunk.chunk, 'VRTS', 4)=0 then
    Result := bctVertex;
  if StrLIComp(aChunk.chunk, 'BONE', 4)=0 then
    Result := bctBone;
  if StrLIComp(aChunk.chunk, 'KEYS', 4)=0 then
    Result := bctKeyFrame;
  if StrLIComp(aChunk.chunk, 'ANIM', 4)=0 then
    Result := bctAnimation;
  if StrLIComp(aChunk.chunk, 'MESH', 4)=0 then
    Result := bctMesh;
  if StrLIComp(aChunk.chunk, 'TRIS', 4)=0 then
    Result := bctTriangle;
end;

function TFileB3D.SkipChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
begin
  aStream.Seek(aChunk.length, soFromCurrent);
  Result := aChunk.length;
end;

function ReadString(aStream: TStream; buffer: PChar; MaxCount: Integer): Integer;
begin
  Result := 0;
  while Result<MaxCount do
  begin
    aStream.Read(buffer[Result], sizeof(char));
    Inc(result);
    if buffer[result-1]=#0 then
      break;
  end;
end;

function TFileB3D.ReadTextureChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
var
  Texture: TB3DTexture;
  Count: Integer;
begin
  Result := 0;
  if aChunk.length<5 then
    exit;
  Count := 0;
  while Count<aChunk.length do
  begin
    Texture := TB3DTexture.Create;
    Inc(Count, ReadString(aStream, Texture.TextureData.fileName, 255));
    Inc(Count, aStream.Read(Texture.TextureData.flags, sizeof(integer)));
    Inc(Count, aStream.Read(Texture.TextureData.blend, sizeof(integer)));
    Inc(Count, aStream.Read(Texture.TextureData.x_pos, sizeof(single)));
    Inc(Count, aStream.Read(Texture.TextureData.y_pos, sizeof(single)));
    Inc(Count, aStream.Read(Texture.TextureData.x_scale, sizeof(single)));
    Inc(Count, aStream.Read(Texture.TextureData.y_scale, sizeof(single)));
    Inc(Count, aStream.Read(Texture.TextureData.rotation, sizeof(single)));
    fTextures.AddObject(Texture.GetTextureName, Texture);
  end;
  Result := fTextures.Count;
end;

function TFileB3D.ReadMaterialChunk(aStream: TStream; aChunk: TB3DChunk): Integer;
var
  Material: TB3DMaterial;
  Count, I: Integer;
  TextureCount: Integer;
begin
  Result := 0;
  if aChunk.length<5 then
    exit;
  Count := 0;
  TextureCount := 0;
  while Count<aChunk.length do
  begin
    Material := TB3DMaterial.Create;
    if Count=0 then
    begin
      Inc(Count, aStream.Read(Material.MaterialData.n_texs, sizeof(integer)));
      TextureCount := Material.MaterialData.n_texs;
    end else
      Material.MaterialData.n_texs := TextureCount;
    Inc(Count, ReadString(aStream, Material.MaterialData.name, 255));
    Inc(Count, aStream.Read(Material.MaterialData.red, sizeof(single)));
    Inc(Count, aStream.Read(Material.MaterialData.green, sizeof(single)));
    Inc(Count, aStream.Read(Material.MaterialData.blue, sizeof(single)));
    Inc(Count, aStream.Read(Material.MaterialData.alpha, sizeof(single)));
    Inc(Count, aStream.Read(Material.MaterialData.shininess, sizeof(single)));
    Inc(Count, aStream.Read(Material.MaterialData.blend, sizeof(integer)));
    Inc(Count, aStream.Read(Material.MaterialData.fx, sizeof(integer)));
    SetLength(Material.MaterialData.texture_id, TextureCount);
    for I:=0 to TextureCount-1 do
      Inc(Count, aStream.Read(Material.MaterialData.texture_id[I], sizeof(Integer)));
    fMaterials.AddObject(Material.GetMaterialName, Material);
  end;
  Result := fMaterials.Count;
end;

function TFileB3D.ReadMeshChunk(aStream: TStream; aChunk: TB3DChunk; Mesh: PMESHChunk): Integer;
var
  C: TB3DChunk;
  T: PTRISChunk;
begin
  Result := 0;
  fillChar(Mesh^, sizeof(TMESHChunk), 0);
  Mesh^.brush_id := -1;
  Inc(Result, aStream.Read(Mesh^.brush_id, sizeof(Integer)));
  T := nil;
  while Result<aChunk.length do
  begin
    Inc(Result, aStream.Read(C, sizeof(TB3DChunk)));
    case GetChunkType(C) of
      bctVertex:
      begin
        Inc(Result, ReadVerticesChunk(aStream, C, @(Mesh^.vertices)));
      end;
      bctTriangle:
      begin
        if Mesh^.triangles=nil then
        begin
          GetMem(Mesh^.triangles, sizeof(TTRISChunk));
          fillChar(Mesh^.triangles^, sizeof(TTRISChunk), 0);
          Inc(Result, ReadTrianglesChunk(aStream, C, Mesh^.triangles));
        end else
        begin
          if T=nil then
          begin
            GetMem(T, sizeof(TTRISChunk));
            fillChar(T^, sizeof(TTRISChunk), 0);
            Inc(Result, ReadTrianglesChunk(aStream, C, T));
            Mesh^.triangles^.next := T;
          end else
          begin
            GetMem(T^.next, sizeof(TTRISChunk));
            fillChar(T^.next^, sizeof(TTRISChunk), 0);
            Inc(Result, ReadTrianglesChunk(aStream, C, T^.next));
            T := T^.next;
          end;
        end;
      end;
      else
        inc(Result, SkipChunk(aStream, C));
    end;
  end;
end;

function TFileB3D.ReadVerticesChunk(aStream: TStream; aChunk: TB3DChunk; Vertices: PVRTSChunk): Integer;
var
  v: PVertexData;
  v1: PVertexData;
  size: Integer;
begin
  Result := 0;
  fillChar(Vertices^, sizeof(TVRTSChunk), 0);
  Inc(Result, aStream.Read(Vertices^.flags, sizeof(Integer)));
  Inc(Result, aStream.Read(Vertices^.tex_coord_sets, sizeof(Integer)));
  Inc(Result, aStream.Read(Vertices^.tex_coord_set_size, sizeof(Integer)));
  size := Vertices^.tex_coord_set_size*Vertices^.tex_coord_sets;
  v := nil;
  while Result<aChunk.length do
  begin
    if Vertices^.vertices=nil then
    begin
      GetMem(Vertices^.vertices, sizeof(TVertexData));
      fillChar(vertices^.vertices^, sizeof(TVertexData), 0);
    end else
    begin
      if v=nil then
      begin
        GetMem(v, sizeof(TVertexData));
        fillChar(v^, sizeof(TVertexData), 0);
        vertices^.vertices^.next := v;
      end else
      begin
        GetMem(v^.next, sizeof(TVertexData));
        fillChar(v^.next^, sizeof(TVertexData), 0);
        v := v^.next;
      end;
    end;
    if v=nil then
      v1 := vertices^.vertices
    else
      v1 := v;
    Inc(Result, aStream.Read(v1^.x, sizeof(single)));
    Inc(Result, aStream.Read(v1^.y, sizeof(single)));
    Inc(Result, aStream.Read(v1^.z, sizeof(single)));
    //W3D Begin
    if (Vertices^.flags and 1)>0 then begin
      Inc(Result, aStream.Read(v1^.nx, sizeof(single)));
      Inc(Result, aStream.Read(v1^.ny, sizeof(single)));
      Inc(Result, aStream.Read(v1^.nz, sizeof(single)));
    end;
    if (Vertices^.flags and 2)>0 then begin
      Inc(Result, aStream.Read(v1^.red, sizeof(single)));
      Inc(Result, aStream.Read(v1^.green, sizeof(single)));
      Inc(Result, aStream.Read(v1^.blue, sizeof(single)));
      Inc(Result, aStream.Read(v1^.alpha, sizeof(single)));
    end;
    //W3D END
    SetLength(v1^.tex_coords, size);
    Inc(Result, aStream.Read(v1^.tex_coords[0], size*sizeof(single)));
  end;
end;

function TFileB3D.ReadTrianglesChunk(aStream: TStream; aChunk: TB3DChunk; Triangle: PTRISChunk): Integer;
begin
  Result := 0;
  if Triangle=nil then
  begin
    GetMem(Triangle, sizeof(TTRISChunk));
    fillChar(Triangle^, sizeof(TTRISChunk), 0);
    Triangle^.brush_id := -1;
  end;
  Inc(Result, aStream.Read(Triangle^.brush_id, sizeof(Integer)));
  SetLength(Triangle^.vertex_id, (aChunk.length-Result) div sizeof(Integer));
  Inc(Result, aStream.Read(Triangle^.vertex_id[0], (aChunk.length-Result)));
end;

//read in only the mesh data, the keyframes and animation had been dropped
function TFileB3D.ReadNodeChunk(aStream: TStream; aChunk: TB3DChunk; Node: PNODEChunk; level: Integer): Integer;
var
  Count: Integer;
  C: TB3DChunk;
  N: PNODEChunk;
begin
  N := nil;
  fillChar(Node^, sizeof(TNODEChunk), 0);
  Node^.level := level;
  Count := 0;
  Inc(Count, ReadString(aStream, Node^.name, 255));
  Inc(Count, aStream.Read(Node^.position[0], sizeof(TAffineVector)));
  Inc(Count, aStream.Read(Node^.scale[0], sizeof(TAffineVector)));
  Inc(Count, aStream.Read(Node^.rotation[0], sizeof(TVector)));
  while Count<aChunk.length do
  begin
    Inc(Count, aStream.Read(C, sizeof(TB3DChunk)));
    case GetChunkType(C) of
      bctMesh:
      begin
        GetMem(Node^.meshes, sizeof(TMESHChunk));
        Inc(Count, ReadMeshChunk(aStream, C, Node^.meshes));
      end;
      bctKeyframe:
      begin
        Inc(Count, SkipChunk(aStream, C));
      end;
      bctNode:
      begin
        if N=nil then
        begin
          GetMem(N, sizeof(TNODEChunk));
          fillChar(N^, sizeof(TNODEChunk), 0);
          Inc(Count, ReadNodeChunk(aStream, C, N, level + 1));
          Node^.next := N;
        end else
        begin
          GetMem(N^.next, sizeof(TNODEChunk));
          fillChar(N^.next^, sizeof(TNODEChunk), 0);
          Inc(Count, ReadNodeChunk(aStream, C, N^.next, level + 1));
          N := N^.next;
        end;
      end;
      bctAnimation:
      begin
        Inc(Count, SkipChunk(aStream, C));
      end;
      else
        Inc(Count, SkipChunk(aStream, C));
    end;
  end;
  Result := Count;
end;

procedure TFileB3D.LoadFromStream(aStream : TStream);
var
  aChunk: TB3DChunk;
  FileSize: Integer;
  Node: PNODEChunk;
begin
  FileSize := aStream.Size;
  Node := nil;
  while aStream.Position<FileSize do
  begin
    aStream.Read(aChunk, sizeof(TB3DChunk));
    case GetChunkType(aChunk) of
      bctHeader:
      begin
        FileSize := aChunk.length - sizeof(TB3DChunk) - sizeof(TBB3DChunk);
        aStream.Seek(sizeof(TBB3DChunk), soFromCurrent);
      end;
      bctTexture:
      begin
        ReadTextureChunk(aStream, aChunk);
      end;
      bctBrush:
      begin
        ReadMaterialChunk(aStream, aChunk);
      end;
      bctNode:
      begin
        if fNodes.NodeData=nil then
        begin
          GetMem(fNodes.NodeData, sizeof(TNODEChunk));
          ReadNodeChunk(aStream, aChunk, fNodes.NodeData, 0);
        end;
      end;
      else
        SkipChunk(aStream, aChunk);
    end;
  end;
end;

procedure TFileB3D.FreeLists;
begin
  while fTextures.Count>0 do
  begin
    fTextures.Objects[0].free;
    ftextures.Delete(0);
  end;
  while fMaterials.Count>0 do
  begin
    fMaterials.Objects[0].free;
    fMaterials.Delete(0);
  end;
end;

//for test only
procedure TFileB3D.Check;
var
  NodeLevel: Integer;
  NodeCount: Integer;
  Node: PNODEChunk;
  VerticesCount: Integer;
  FaceCount: Integer;
  Face: PTRISChunk;
  Vertex: PVertexData;
begin
  NodeLevel := 0;
  NodeCount := 0;
  VerticesCount := 0;
  FaceCount := 0;
  Node := fNodes.NodeData;
  while Node<>nil do
  begin
    if Node^.meshes<>nil then
      Inc(NodeCount);
    if Node^.level>NodeLevel then
      NodeLevel := Node^.level;
    if Node^.meshes<>nil then
    begin
      Vertex := Node^.meshes.vertices.vertices;
      while Vertex<>nil do
      begin
        Inc(VerticesCount);
        Vertex := Vertex.next;
      end;
      Face := Node^.meshes.triangles;
      while Face<>nil do
      begin
        Inc(FaceCount);
        Face := Face.next;
      end;
    end;
    Node := Node^.next;
  end;
  {
  MessageBeep(FaceCount);
  MessageBeep(VerticesCount);
  MessageBeep(NodeLevel);
  MessageBeep(NodeCount);
  }
end;

end.
