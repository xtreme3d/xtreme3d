//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLFileLMTS<p>

 <b>History : </b><font size=-1><ul>
	<li>22/01/10 - Yar - Added GLTextureFormat to uses
        <li>25/07/07 - DaStr - Replaced some types to get rid of compiler warnings
        <li>08/10/08 - DanB - fix for different Char size in Delphi 2009+
        <li>22/06/08 - DaStr - Fixups after converting TMeshObject.LightMapTexCoords
                               to TAffineVectorList (thanks Ast) (Bugtracker ID = 2000089)
        <li>29/05/08 - DaStr - Replaced GLUtils with GLGraphics (BugTracker ID = 1923844)
                               Added $I GLScene.inc
        <li>13/08/07 - fig -  Added checks for DDS textures in LoadFromStream()
        <li>23/03/07 - fig -  Fixed exception when material properties were loaded without a material library being assigned.
        <li>15/01/07 - fig -  If available, material data is now imported/exported.
        <li>15/01/07 - fig -  Added checks in the loader for invalid material indices.  LMTools can return meshes like this for some reason.
        <li>14/01/07 - fig -  Material/facegroup name is now changed to the available filename instead of stripping the extention.
        <li>12/01/07 - fig -  Fixed LoadFromStream() to handle duplicate and null textures correctly.
        <li>07/01/07 - fig -  Fixed the file extention stripping. extra periods in the filenames were causing conflicts.
        <li>06/01/07 - fig -  Strip all texture file extentions on load/save
        <li>03/01/07 - fig -  can now use different texture types from the ones stated in the file,
                            missing texture exception handling, normals are built on load,
                            support for more facegroup types added.
        <li>02/01/07 - fig - Added SavetoStream() and Capabilities function.
        <li>02/01/07 - PvD - Dealing with non empty material libraries.
        <li>02/01/07 - PvD - Mirrored mesh in X to original orientation.
        <li>01/01/07 - Dave Gravel - Modification to make it work.
        <li>10/09/03 - Domin - Creation
   </ul><p>
}
unit GLFileLMTS;

interface

{$I GLScene.inc}

uses Graphics, Classes, SysUtils,
     GLVectorFileObjects, ApplicationFileIO, VectorLists, VectorGeometry,
     GLTexture, GLUtils, PersistentClasses, GLGraphics;

const
    C_LMTS_ID = $53544D4C;
    C_LMTS_VER = 4;
    C_LMTS_SUBS = $53425553;
    C_LMTS_TEXT = $54584554;
    C_LMTS_TRIS = $53495254;

    C_LMTS_TEXFNLEN = 255; // max texture filename length

type
    PLMTS_Header = ^TLMTS_Header;
    TLMTS_Header = record //packed
        ID: cardinal;
        Ver: cardinal;
        headerSize: cardinal;
        nTexts: word; // # of textures
        nSubsets: word;
        nTris: cardinal;
        subSize: word;
        vtxSize: word;
    end;

    PLMTS_TexData = ^TLMTS_TexData;
    TLMTS_TexData = record //packed
        fName: array[0..C_LMTS_TEXFNLEN] of ansichar;
        Flags: word;
    end;

    PLMTS_Subset = ^TLMTS_Subset;
    TLMTS_Subset = record //packed
        Offset: longint;
        Count: longint;
        TextID1: word;
        TextID2: word;
    end;

    PLMTS_Vertex = ^TLMTS_Vertex;
    TLMTS_Vertex = record //packed
        x, y, z: single;
        u1, v1, u2, v2: single;
    end;

    PLMTS = ^TLMTS;
    TLMTS = record
        header: TLMTS_Header;
        usrData: pointer;
        usrSize: cardinal;
        texData: pointer;
        subsets: pointer;
        tris: pointer;
        ok: boolean;
    end;

    TMaterialInfo = record
        FShininess, BShininess: TShininess;
        FAmbient, FDiffuse, FEmission, FSpecular,
            BAmbient, BDiffuse, BEmission, BSpecular: TVector;
        ImageAlpha: TGLTextureImageAlpha;
        magFilter: TGLMagFilter;
        minFilter: TGLMinFilter;
        TextureMode: TGLTextureMode;
        TextureWrap: TGLTextureWrap;
        Blendingmode: TBlendingMode;
        FaceCulling: TFaceCulling;
        mathash: integer;
    end;

    TGLLMTSVectorFile = class(TVectorFile)
    public
        class function Capabilities: TDataFileCapabilities; override;

        procedure LoadFromStream(aStream: TStream); override;
        procedure SaveToStream(aStream: TStream); override;

    end;

implementation

//uses
//  GLTextureFormat;

// ------------------
// ------------------ TGLLMTSVectorFile ------------------
// ------------------

// Capabilities
//

class function TGLLMTSVectorFile.Capabilities: TDataFileCapabilities;
begin
    Result := [dfcRead, dfcWrite];
end;

// LoadFromStream
//

procedure TGLLMTSVectorFile.LoadFromStream(aStream: TStream);
var
    MO: TMeshObject;
    FG: TFGVertexIndexList;
    LL: TGLMaterialLibrary;
    ML: TGLMaterialLibrary;
    LMTS: TLMTS;
    T: TLMTS_TexData;
    V: array[0..2] of TLMTS_Vertex;
    S: TLMTS_Subset;
    _4cc: cardinal;
    C: integer;
    fName: string;
    vi: Tintegerlist;
    libmat: TGLLibmaterial;
    lmnames, matnames: TStringlist;
    MatInfoHeader: array[0..3] of ansichar;
    MatInfoCount: Cardinal;
    Matinfo: array of TMaterialInfo;
    i, j: integer;
begin
    owner.MeshObjects.Clear;

    MO := TMeshObject.CreateOwned(Owner.MeshObjects);
    MO.Mode := momFaceGroups;

    vi := TIntegerlist.create;

    LL := Owner.LightmapLibrary;
    ML := Owner.MaterialLibrary;

    lmnames := TStringlist.create;
    matnames := TStringlist.create;
    MatInfoCount := 0;
    try
        // read header...
        aStream.Read(LMTS.header, SizeOf(TLMTS_Header));
        // verify...
        if (LMTS.header.ID <> C_LMTS_ID) or
            (LMTS.header.Ver <> C_LMTS_VER) or
            (LMTS.header.headerSize < SizeOf(TLMTS_Header)) or
            (LMTS.header.subSize < SizeOf(TLMTS_Subset)) or
            (LMTS.header.vtxSize < SizeOf(TLMTS_Vertex)) then
            raise Exception.Create('Error in header');

        // read "user" data - actually skip this data...
        LMTS.usrSize := LMTS.header.headerSize - SizeOf(TLMTS_Header);
        if (LMTS.usrSize > 7) then
        begin
            aStream.Read(MatInfoHeader, 4);
            if MatInfoheader = 'MATI' then
            begin
                astream.read(MatInfoCount, 4);
                if MatInfoCount > 0 then
                begin
                    setlength(matinfo, MatInfoCount);
                    for i := 0 to MatInfoCount - 1 do
                    begin
                        astream.Read(matinfo[i], sizeof(matinfo[i]));
                    end;
                end;
                if LMTS.usrSize > ((MatInfoCount * sizeof(TMaterialInfo)) + 8) then
                begin
                    LMTS.usrSize := LMTS.usrSize - ((MatInfoCount * sizeof(TMaterialInfo)) + 8);
                    aStream.Seek(LMTS.usrSize, soFromCurrent);
                end;
            end
            else
                aStream.Seek(LMTS.usrSize - 4, soFromCurrent);
        end
        else
            if (LMTS.usrSize > 0) then
                aStream.Seek(LMTS.usrSize, soFromCurrent);

        // read texture filenames data...
        aStream.Read(_4cc, SizeOf(_4cc));
        if (_4cc <> C_LMTS_TEXT) then
            raise Exception.Create('Texture data not found');

        for C := 0 to LMTS.header.nTexts - 1 do
        begin
            aStream.Read(T, SizeOf(TLMTS_TexData));
            if T.Flags = 0 then
            begin
                if Assigned(ML) and (trim(String(T.fName)) <> '') then
                begin
                    fName := String(T.fName);
                    try
                        if lastdelimiter('.', fName) <> length(fName) - 3 then
                            fName := fName + '.tga'
                        else
                            fName := changefileext(fName, '.tga');
                        if not fileexists(fName) then
                        begin
                            fName := changefileext(fName, '.jpg');
                            if not fileexists(fName) then
                            begin
                                fName := changefileext(fName, '.png');
                                if not fileexists(fName) then
                                begin
                                    fName := changefileext(fName, '.bmp');
                                    if not fileexists(fName) then
                                    begin
                                        fName := changefileext(fName, '.dds');
                                        if not fileexists(fName) then
                                        begin
                                            fName := String(T.fName);
                                        end;
                                        //  fName:=fName+' (missing)';
                                    end;

                                end;
                            end;
                        end;
                        libmat := ML.Materials.GetLibMaterialByName(fName);
                        if not assigned(libmat) then
                        begin
                            with ML.AddTextureMaterial(fName, fName) do
                                Material.Texture.TextureMode := tmModulate;
                        end;
                        matnames.add(fName);
                    except
                        matnames.add(fName);
                        {    on E: ETexture do
                            begin
                                if not Owner.IgnoreMissingTextures then
                                    raise;
                            end; }
                    end;
                end
                else
                    matnames.add(String(T.fName));
            end
            else
            begin
                if Assigned(LL) and (trim(String(T.fName)) <> '') then
                begin
                    fName := String(T.fName);
                    try
                        if lastdelimiter('.', fName) <> length(fName) - 3 then
                            fName := fName + '.tga'
                        else
                            fName := changefileext(fName, '.tga');
                        if not fileexists(fName) then
                        begin
                            fName := changefileext(fName, '.jpg');
                            if not fileexists(fName) then
                            begin
                                fName := changefileext(fName, '.png');
                                if not fileexists(fName) then
                                begin
                                    fName := changefileext(fName, '.bmp');
                                    if not fileexists(fName) then
                                    begin
                                        fName := changefileext(fName, '.dds');
                                        if not fileexists(fName) then
                                        begin
                                            fName := String(T.fName);
                                        end;
                                        //fName:=fName+' (missing)';
                                    end;
                                end;

                            end;
                        end;
                        libmat := LL.Materials.GetLibMaterialByName(fName);
                        if not assigned(libmat) then
                        begin
                            with LL.AddTextureMaterial(fName, fName).Material.Texture do
                            begin
                                MinFilter := miLinear;
                                TextureWrap := twNone;
                                TextureFormat := tfRGB;
                                TextureMode := tmModulate;
                            end;
                        end;
                        lmnames.add(fName);
                    except
                        lmnames.add(fName);
                        {  on E: ETexture do
                          begin
                              if not Owner.IgnoreMissingTextures then
                                  raise;
                          end;  }
                    end;
                end
                else
                    lmnames.add(String(T.fName));
            end;
        end;

        if assigned(owner.MaterialLibrary) then
            for i := 0 to MatInfoCount - 1 do
            begin
                libmat := nil;
                for j := 0 to owner.MaterialLibrary.Materials.count - 1 do
                    if owner.MaterialLibrary.Materials[j].NameHashKey = matinfo[i].mathash then
                    begin
                        libmat := owner.MaterialLibrary.Materials[j];
                        break;
                    end;

                if assigned(libmat) then
                begin
                    with matinfo[i] do
                    begin
                        libmat.Material.FrontProperties.Shininess := FShininess;
                        libmat.Material.BackProperties.Shininess := BShininess;
                        libmat.Material.FrontProperties.Ambient.Color := FAmbient;
                        libmat.Material.FrontProperties.Diffuse.Color := FDiffuse;
                        libmat.Material.FrontProperties.Emission.Color := FEmission;
                        libmat.Material.FrontProperties.Specular.Color := FSpecular;

                        libmat.Material.BackProperties.Ambient.Color := BAmbient;
                        libmat.Material.BackProperties.Diffuse.Color := BDiffuse;
                        libmat.Material.BackProperties.Emission.Color := BEmission;
                        libmat.Material.BackProperties.Specular.Color := BSpecular;

                        libmat.Material.Texture.ImageAlpha := ImageAlpha;
                        libmat.Material.Texture.MagFilter := magFilter;
                        libmat.Material.Texture.MinFilter := minFilter;
                        libmat.Material.Texture.TextureMode := TextureMode;
                        libmat.Material.Texture.TextureWrap := TextureWrap;
                        libmat.Material.BlendingMode := Blendingmode;
                        libmat.Material.FaceCulling := faceculling;
                    end;
                end;
            end;

        // read subset data...
        aStream.Read(_4cc, SizeOf(_4cc));
        if (_4cc <> C_LMTS_SUBS) then
            raise Exception.Create('Subset data not found');
        for C := LMTS.header.nSubsets - 1 downto 0 do
        begin
            aStream.Read(S, LMTS.header.subSize);
            FG := TFGVertexIndexList.CreateOwned(MO.FaceGroups);
            FG.Mode := fgmmTriangles;
            fg.vertexindices.AddSerie(s.Offset * 3, 1, s.Count * 3);
            vi.AddSerie(s.Offset * 3, 1, s.Count * 3);

            if Assigned(ML) and (S.TextID1 <> $FFFF) then
            begin
                if (S.TextID1 < matnames.count) then
                begin
                    libmat := ml.Materials.GetLibMaterialByName(matnames[S.TextID1]);
                    if assigned(libmat) then
                        fg.MaterialName := libmat.Name;
                end;
            end;

            if Assigned(LL) and (S.TextID2 <> $FFFF) then
            begin
                if (S.TextID2 - matnames.count < lmnames.count) and (S.TextID2 - matnames.count > -1) then
                begin
                    libmat := ll.Materials.GetLibMaterialByName(lmnames[S.TextID2 - matnames.count]);
                    if assigned(libmat) then
                        fg.lightmapindex := libmat.Index;
                end;
            end;
        end;
        // read vertex data...
        aStream.Read(_4cc, SizeOf(_4cc));
        if (_4cc <> C_LMTS_TRIS) then
            raise Exception.Create('Vertex data not found');
        for c := 0 to Integer(LMTS.header.nTris) - 1 do
        begin
            aStream.Read(V[0], LMTS.header.vtxSize);
            aStream.Read(V[1], LMTS.header.vtxSize);
            aStream.Read(V[2], LMTS.header.vtxSize);

            MO.Vertices.Add(-V[0].x, V[0].y, V[0].z);
            MO.TexCoords.Add(V[0].u1, -V[0].v1);
            MO.LightmapTexCoords.Add(V[0].u2, 1 - V[0].v2);

            MO.Vertices.Add(-V[2].x, V[2].y, V[2].z);
            MO.TexCoords.Add(V[2].u1, -V[2].v1);
            MO.LightmapTexCoords.Add(V[2].u2, 1 - V[2].v2);

            MO.Vertices.Add(-V[1].x, V[1].y, V[1].z);
            MO.TexCoords.Add(V[1].u1, -V[1].v1);
            MO.LightmapTexCoords.Add(V[1].u2, 1 - V[1].v2);
        end;
        mo.BuildNormals(vi, momtriangles);
        vi.free;
        matnames.free;
        lmnames.free;
        setlength(Matinfo, 0);
    except
        matnames.free;
        lmnames.free;
        MO.Free;
    end;
end;

// SaveToStream
//

procedure TGLLMTSVectorFile.SaveToStream(aStream: TStream);
var
    MO: TMeshObject;
    FG: TFGVertexIndexList;
    i, j, k, l, lmstartindex, c, matindex: integer;
    h: TLMTS_Header;
    V: array[0..2] of TLMTS_Vertex;
    texdata: array of TLMTS_TexData;
    subsets: array of TLMTS_Subset;
    tris: array of TLMTS_Vertex;
    _4cc: cardinal;
    matname: AnsiString;
    ss: integer;
    matinfo: array of TMaterialInfo;
    MatInfoCount: integer;
    libmat: TGLLibMaterial;
begin
    c := 0;
    lmstartindex := maxint;
    for i := 0 to Owner.MeshObjects.count - 1 do
    begin
        mo := Owner.MeshObjects[i];
        for j := 0 to mo.facegroups.count - 1 do
        begin
            fg := TfgVertexIndexList(mo.facegroups[j]);

            matname := AnsiString(fg.materialname);

            //no duplicate textures please
            matindex := -1;
            for k := 0 to high(texdata) do
                if texdata[k].fName = matname then
                begin
                    matindex := k;
                    break;
                end;

            if matindex = -1 then //not a duplicate, so add it
            begin
                setlength(texdata, length(texdata) + 1);
                with texdata[high(texdata)] do
                begin
                    matindex := high(texdata);

                    strpcopy(pansichar(@fName), matname);
                    Flags := 0;
                end;

                inc(c); //used to offest the lightmap index
            end;

            //set some of the facegroup (subsets) info here.
            setlength(subsets, length(subsets) + 1);
            with subsets[high(subsets)] do
            begin
                if (matname <> '') then
                    TextID1 := matindex
                else
                    TextID1 := $FFFF;
            end;

            if (fg.LightMapIndex > -1) and (lmstartindex > fg.LightMapIndex) then
                lmstartindex := fg.lightmapindex; //used to offest the lightmap index
        end;
    end;

    if lmstartindex = maxint then
        lmstartindex := 0; //cool, lightmaps start from the first index
    ss := 0;
    for i := 0 to Owner.MeshObjects.count - 1 do
    begin
        mo := owner.meshobjects[i];
        for j := 0 to mo.facegroups.count - 1 do
        begin
            fg := TfgVertexIndexList(mo.facegroups[j]);

            //subset already created earlier, just finish filling the data.
            //we needed the "c" and "lmstartindex" to be able to do this
            with subsets[ss] do
            begin
                Offset := length(tris) div 3;
                Count := fg.vertexindices.count div 3;

                if (fg.lightmapindex > -1) and assigned(owner.LightmapLibrary) then
                    TextID2 := c + fg.LightMapIndex - lmstartindex
                else
                    TextID2 := $FFFF;
            end;

            //fill the vertex data
            k := 0;
            while k < fg.vertexindices.count do
            begin
                for l := 0 to 2 do
                begin
                    with v[l] do
                    begin
                        //vertex
                        x := -mo.Vertices[fg.VertexIndices[k + l]][0];
                        y := mo.Vertices[fg.VertexIndices[k + l]][1];
                        z := mo.Vertices[fg.VertexIndices[k + l]][2];

                        //texcoords
                        u1 := 0;
                        v1 := 0;
                        if fg is TFGVertexNormalTexIndexList then
                        begin
                            if mo.texcoords.count > TFGVertexNormalTexIndexList(fg).texcoordIndices[k + l] then
                            begin
                                u1 := mo.Texcoords[TFGVertexNormalTexIndexList(fg).texcoordIndices[k + l]][0];
                                v1 := -mo.Texcoords[TFGVertexNormalTexIndexList(fg).texcoordIndices[k + l]][1];
                            end;
                        end
                        else
                            if fg is TFGIndexTexCoordList then
                            begin
                                u1 := TFGIndexTexCoordList(fg).Texcoords[k + l][0];
                                v1 := -TFGIndexTexCoordList(fg).Texcoords[k + l][1];
                            end
                            else
                                if mo.texcoords.count > fg.VertexIndices[k + l] then
                                begin
                                    u1 := mo.Texcoords[fg.VertexIndices[k + l]][0];
                                    v1 := -mo.Texcoords[fg.VertexIndices[k + l]][1];
                                end;

                        //lightmap texcoords
                        u2 := 0;
                        v2 := 0;
                        if mo.LightmapTexcoords.count > fg.VertexIndices[k + l] then
                        begin
                            u2 := mo.LightmapTexcoords[fg.VertexIndices[k + l]].S;
                            v2 := 1 - mo.LightmapTexcoords[fg.VertexIndices[k + l]].T;
                        end;
                    end;
                end;
                setlength(tris, length(tris) + 3);

                tris[high(tris) - 2] := v[0];
                tris[high(tris) - 1] := v[2];
                tris[high(tris)] := v[1];

                inc(k, 3);
            end;
            inc(ss);
        end;
    end;

    //store the material properties..
    if assigned(owner.MaterialLibrary) then
    begin
        for i := 0 to high(texdata) do
        begin
            libmat := owner.MaterialLibrary.Materials.GetLibMaterialByName(String(texdata[i].fName));
            if assigned(libmat) then
            begin
                setlength(matinfo, length(matinfo) + 1);
                with matinfo[high(matinfo)] do
                begin
                    FShininess := libmat.Material.FrontProperties.Shininess;
                    BShininess := libmat.Material.BackProperties.Shininess;
                    FAmbient := libmat.Material.FrontProperties.Ambient.Color;
                    FDiffuse := libmat.Material.FrontProperties.Diffuse.Color;
                    FEmission := libmat.Material.FrontProperties.Emission.Color;
                    FSpecular := libmat.Material.FrontProperties.Specular.Color;

                    BAmbient := libmat.Material.BackProperties.Ambient.Color;
                    BDiffuse := libmat.Material.BackProperties.Diffuse.Color;
                    BEmission := libmat.Material.BackProperties.Emission.Color;
                    BSpecular := libmat.Material.BackProperties.Specular.Color;

                    ImageAlpha := libmat.Material.Texture.ImageAlpha;
                    magFilter := libmat.Material.Texture.MagFilter;
                    minFilter := libmat.Material.Texture.MinFilter;
                    TextureMode := libmat.Material.Texture.TextureMode;
                    TextureWrap := libmat.Material.Texture.TextureWrap;
                    Blendingmode := libmat.Material.BlendingMode;
                    faceculling := libmat.Material.FaceCulling;
                    mathash := libmat.NameHashKey;
                end;
            end;
        end;
    end;

    //add the lightmap texture names to the texdata list
    c := length(texdata);
    if assigned(owner.LightmapLibrary) then
        for i := 0 to Owner.MeshObjects.count - 1 do
        begin
            mo := owner.meshobjects[i];
            for j := 0 to mo.facegroups.count - 1 do
            begin
                fg := TfgVertexIndexList(mo.facegroups[j]);
                if fg.lightmapindex > -1 then
                begin
                    matname := AnsiString(owner.LightmapLibrary.materials[fg.lightmapindex].name);
                    //no duplicate textures please
                    matindex := -1;
                    for k := c to high(texdata) do
                        if texdata[k].fName = matname then
                        begin
                            matindex := k;
                            break;
                        end;
                    if matindex = -1 then //not a duplicate, so add it
                    begin
                        setlength(texdata, length(texdata) + 1);
                        with texdata[high(texdata)] do
                        begin
                            strpcopy(pansichar(@fName), matname);
                            Flags := 1;
                        end;
                    end;
                end;
            end;
        end;

    //fill and write the file header
    with h do
    begin
        ID := C_LMTS_ID;
        Ver := C_LMTS_VER;
        headerSize := 24 + 8 + (length(matinfo) * sizeof(TMaterialinfo));
        nTexts := length(texdata);
        nSubsets := length(subsets);
        nTris := length(tris) div 3;
        subSize := sizeof(TLMTS_Subset);
        vtxSize := sizeof(TLMTS_Vertex);
    end;
    astream.Write(h, sizeof(h));

    astream.Write('MATI', 4);
    MatInfoCount := length(matinfo);
    astream.Write(MatInfoCount, sizeof(MatInfoCount));
    //write the materials info
    for i := 0 to high(matinfo) do
        astream.Write(matinfo[i], sizeof(matinfo[i]));

    //write the texture names
    _4cc := C_LMTS_TEXT;
    astream.Write(_4cc, sizeof(_4cc));
    for i := 0 to high(texdata) do
        astream.Write(texdata[i], sizeof(texdata[i]));

    //fagegroups
    _4cc := C_LMTS_SUBS;
    astream.Write(_4cc, sizeof(_4cc));
    for i := 0 to high(subsets) do
        astream.Write(subsets[i], sizeof(subsets[i]));

    //vertex data
    _4cc := C_LMTS_TRIS;
    astream.Write(_4cc, sizeof(_4cc));
    for i := 0 to high(tris) do
        astream.Write(tris[i], sizeof(tris[i]));

    //free up used memory
    setlength(tris, 0);
    setlength(subsets, 0);
    setlength(texdata, 0);
    setlength(Matinfo, 0);
end;

initialization

    RegisterVectorFileFormat('lmts', 'Pulsar Studio LMTS File Format', TGLLMTSVectorFile);

end.

