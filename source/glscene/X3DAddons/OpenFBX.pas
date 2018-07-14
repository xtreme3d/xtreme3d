Unit OpenFBX;

interface

uses VectorTypes;

type
  PtrReal = ^real;

  FBXMatrix = record
    m: Array [0..15] of real;
  end;

var
  IsOpenFBXInitialized: boolean = False;

  fbxSceneLoad: function(const buffer: pchar; size: Integer): Pointer; cdecl;
  fbxSceneGetMeshCount: function(scene: Pointer): Integer; cdecl;
  fbxSceneGetMesh: function(scene: Pointer; index: Integer): Pointer; cdecl;

  fbxObjectGetLocalPosition: function(mesh: Pointer; index: Integer): real; cdecl;
  fbxObjectGetLocalRotation: function(mesh: Pointer; index: Integer): real; cdecl;
  fbxObjectGetLocalScaling: function(mesh: Pointer; index: Integer): real; cdecl;

  fbxObjectGetGlobalTransform: function(mesh: Pointer): FBXMatrix; cdecl;
  
  fbxMeshGetVertexCount: function(mesh: Pointer): Integer; cdecl;
  fbxMeshGetVertices: function(mesh: Pointer): PtrReal; cdecl;
  fbxMeshGetNormals: function(mesh: Pointer): PtrReal; cdecl;
  fbxMeshGetUVs: function(mesh: Pointer): PtrReal; cdecl;
  
  function InitOpenFBX(ADllName: pchar): boolean;
  procedure CloseOpenFBX;

implementation

uses
  moduleloader;
var
  vOpenFBXHandle: TModuleHandle;

function InitOpenFBX(ADllName: pchar): boolean;
begin
  IsOpenFBXInitialized := LoadModule(vOpenFBXHandle, ADllName);
  result := IsOpenFBXInitialized;

  if IsOpenFBXInitialized then
  begin
    fbxSceneLoad := GetModuleSymbol(vOpenFBXHandle, 'fbxSceneLoad');
    fbxSceneGetMeshCount := GetModuleSymbol(vOpenFBXHandle, 'fbxSceneGetMeshCount');
    fbxSceneGetMesh := GetModuleSymbol(vOpenFBXHandle, 'fbxSceneGetMesh');

    fbxObjectGetLocalPosition := GetModuleSymbol(vOpenFBXHandle, 'fbxObjectGetLocalPosition');
    fbxObjectGetLocalRotation := GetModuleSymbol(vOpenFBXHandle, 'fbxObjectGetLocalRotation');
    fbxObjectGetLocalScaling := GetModuleSymbol(vOpenFBXHandle, 'fbxObjectGetLocalScaling');

    fbxObjectGetGlobalTransform := GetModuleSymbol(vOpenFBXHandle, 'fbxObjectGetGlobalTransform');
    
    fbxMeshGetVertexCount := GetModuleSymbol(vOpenFBXHandle, 'fbxMeshGetVertexCount');
    fbxMeshGetVertices := GetModuleSymbol(vOpenFBXHandle, 'fbxMeshGetVertices');
    fbxMeshGetNormals := GetModuleSymbol(vOpenFBXHandle, 'fbxMeshGetNormals');
    fbxMeshGetUVs := GetModuleSymbol(vOpenFBXHandle, 'fbxMeshGetUVs');
  end;
end;

procedure CloseOpenFBX;
begin
  IsOpenFBXInitialized := false;
  UnLoadModule(vOpenFBXHandle);
end;

end.