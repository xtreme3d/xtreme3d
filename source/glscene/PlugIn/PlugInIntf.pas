unit PlugInIntf;

// PlugInIntf - the interface unit to GLScene plug-ins
//
// Last Change - 29. November 1997
// for more information see help file for writing plug-ins

interface

type TPIServiceType = (stRaw,stObject,stBitmap,stTexture,stImport,stExport);
     TPIServices    = set of TPIServiceType;

     TEnumCallBack = procedure(Name: PChar); stdcall;

     TEnumResourceNames = procedure(Service: TPIServiceType; Callback: TEnumCallback); stdcall;
     TGetServices    = function : TPIServices; stdcall;
     TGetVendor      = function : PChar; stdcall;
     TGetDescription = function : PChar; stdcall;
     TGetVersion     = function : PChar; stdcall;

implementation

end.

