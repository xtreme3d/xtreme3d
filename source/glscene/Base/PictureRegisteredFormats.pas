// PictureRegisteredFormats
{: Egg<p>

   Hacks into the VCL to access the list of TPicture registered TGraphic formats<p>

   <b>History : </b><font size=-1><ul>
      <li>08/03/06 - ur - added Delphi 2006 support
      <li>28/02/05 - EG - Added BPL support
      <li>24/02/05 - EG - Creation
   </ul></font>
}
unit PictureRegisteredFormats;

interface

uses Classes, Graphics;

{$ifdef VER130} {$define PRF_HACK_PASSES} {$endif} // Delphi 5
{$ifdef VER140} {$define PRF_HACK_PASSES} {$endif} // Delphi 6
{$ifdef VER150} {$define PRF_HACK_PASSES} {$endif} // Delphi 7
{$ifdef VER170} {$define PRF_HACK_PASSES} {$endif} // Delphi 2005
{$ifdef VER180} {$define PRF_HACK_PASSES} {$endif} // Delphi 2006

{$ifndef PRF_HACK_PASSES} Error: hack not tested for this Delphi version! {$endif}

{: Returns the TGraphicClass associated to the extension, if any.<p>
   Accepts anExtension with or without the '.' }
function GraphicClassForExtension(const anExtension : String) : TGraphicClass;

{: Adds to the passed TStrings the list of registered formats.<p>
   Convention is "extension=description" for the string, the Objects hold
   the corresponding TGraphicClass (extensions do not include the '.'). }
procedure HackTPictureRegisteredFormats(destList : TStrings);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   PInteger = ^Integer;

// GraphicClassForExtension
//
function GraphicClassForExtension(const anExtension : String) : TGraphicClass;
var
   i : Integer;
   sl : TStringList;
   buf : String;
begin
   Result:=nil;
   if anExtension='' then Exit;
   if anExtension[1]='.' then
      buf:=Copy(anExtension, 2, MaxInt)
   else buf:=anExtension;
   sl:=TStringList.Create;
   try
      HackTPictureRegisteredFormats(sl);
      i:=sl.IndexOfName(buf);
      if i>=0 then
         Result:=TGraphicClass(sl.Objects[i]);
   finally
      sl.Free;
   end;
end;

type
   PFileFormat = ^TFileFormat;
   TFileFormat = record
      GraphicClass: TGraphicClass;
      Extension: string;
      Description: string;
      DescResID: Integer;
   end;

// HackTPictureRegisteredFormats
//
procedure HackTPictureRegisteredFormats(destList : TStrings);
var
   pRegisterFileFormat, pCallGetFileFormat, pGetFileFormats, pFileFormats : PChar;
   iCall : Integer;
   i : Integer;
   list : TList;
   fileFormat : PFileFormat;
begin
   pRegisterFileFormat:=PChar(@TPicture.RegisterFileFormat);
   if pRegisterFileFormat[0]=#$FF then // in case of BPL redirector
      pRegisterFileFormat:=PChar(PInteger(PInteger(@pRegisterFileFormat[2])^)^);
   pCallGetFileFormat:=@pRegisterFileFormat[16];
   iCall:=PInteger(pCallGetFileFormat)^;
   pGetFileFormats:=@pCallGetFileFormat[iCall+4];
   pFileFormats:=PChar(PInteger(@pGetFileFormats[2])^);
   list:=TList(PInteger(pFileFormats)^);
   if list<>nil then begin
      for i:=0 to list.Count-1 do begin
         fileFormat:=PFileFormat(list[i]);
         destList.AddObject(fileFormat.Extension+'='+fileFormat.Description, TObject(fileFormat.GraphicClass));
      end;
   end;
end;

end.
