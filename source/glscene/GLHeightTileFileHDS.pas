// GLHeightTileFileHDS
{: HeightDataSource for the HTF (HeightTileFile) format.<p>

	<b>History : </b><font size=-1><ul>
      <li>29/01/03 - EG - Creation
	</ul></font>
}
unit GLHeightTileFileHDS;

interface

uses Classes, GLHeightData, HeightTileFile;

type

	// TGLHeightTileFileHDS
	//
   {: An Height Data Source for the HTF format.<p> }
	TGLHeightTileFileHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FInfiniteWrap : Boolean;
         FHTFFileName : String;
         FHTF : THeightTileFile;
         FMinElevation : Integer;

	   protected
	      { Protected Declarations }
         procedure SetHTFFileName(const val : String);
         procedure SetInfiniteWrap(val : Boolean);
         procedure SetMinElevation(val : Integer);

         procedure StartPreparingData(heightData : THeightData); override;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }

         {: FileName of the HTF file.<p>
            Note that it is accessed via the services of ApplicationFileIO,
            so this may not necessarily be a regular file on a disk... }
         property HTFFileName : String read FHTFFileName write SetHTFFileName;
         {: If true the height field is wrapped indefinetely. }
         property InfiniteWrap : Boolean read FInfiniteWrap write SetInfiniteWrap default True;
         {: Minimum elevation of the tiles that are considered to exist.<p>
            This property can typically be used to hide underwater tiles. }
         property MinElevation : Integer read FMinElevation write SetMinElevation default -32768;

         property MaxPoolSize;
         property DefaultHeight;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TGLHeightTileFileHDS ------------------
// ------------------

// Create
//
constructor TGLHeightTileFileHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FInfiniteWrap:=True;
   FMinElevation:=-32768;
end;

// Destroy
//
destructor TGLHeightTileFileHDS.Destroy;
begin
   FHTF.Free;
	inherited Destroy;
end;

// SetHTFFileName
//
procedure TGLHeightTileFileHDS.SetHTFFileName(const val : String);
begin
   if FHTFFileName<>val then begin
      MarkDirty;
      FreeAndNil(FHTF);
      FHTFFileName:=val;
   end;
end;

// SetInfiniteWrap
//
procedure TGLHeightTileFileHDS.SetInfiniteWrap(val : Boolean);
begin
   if FInfiniteWrap<>val then begin
      FInfiniteWrap:=val;
      MarkDirty;
   end;
end;

// SetMinElevation
//
procedure TGLHeightTileFileHDS.SetMinElevation(val : Integer);
begin
   if FMinElevation<>val then begin
      FMinElevation:=val;
      MarkDirty;
   end;
end;

// StartPreparingData
//
procedure TGLHeightTileFileHDS.StartPreparingData(heightData : THeightData);
var
   oldType : THeightDataType;
   htfTile : PHeightTile;
   htfTileInfo : PHeightTileInfo;
   x, y : Integer;
begin
   // access htf data
   if not Assigned(FHTF) then begin
      if FHTFFileName='' then begin
         heightData.DataState:=hdsNone;
         Exit;
      end;
      FHTF:=THeightTileFile.Create(FHTFFileName);
      Assert(FHTF.TileSize=heightData.Size,
             'HTF TileSize and HeightData size don''t match');
   end;

   // retrieve data and place it in the heightData
   with heightData do begin
      if InfiniteWrap then begin
         x:=XLeft mod FHTF.SizeX;
         if x<0 then x:=x+FHTF.SizeX;
         y:=YTop mod FHTF.SizeY;
         if y<0 then y:=y+FHTF.SizeY;
         htfTile:=FHTF.GetTile(x, y, @htfTileInfo);
      end else begin
         htfTile:=FHTF.GetTile(XLeft, YTop, @htfTileInfo);
      end;
      if (htfTile=nil) or (htfTileInfo.max<=FMinElevation) then begin
         // non-aligned tiles aren't handled (would be slow anyway)
         DataState:=hdsNone;
      end else begin
         oldType:=DataType;
         Allocate(hdtSmallInt);
         Move(htfTile.data[0], SmallIntData^, DataSize);
         if oldType<>hdtSmallInt then
            DataType:=oldType;
         DataState:=hdsReady;
         HeightMin:=htfTileInfo.min;
         HeightMax:=htfTileInfo.max;
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   Classes.RegisterClasses([TGLHeightTileFileHDS]);

end.
