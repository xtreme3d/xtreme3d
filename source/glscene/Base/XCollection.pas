//
// This unit is part of the GLScene Project, http://glscene.org
//
{: XCollection<p>

	A polymorphism-enabled TCollection-like set of classes<p>

	<b>History : </b><font size=-1><ul>
      <li>08/12/04 - SG - Added TXCollectionItem.CanAddTo class function
      <li>03/07/04 - LR - Removed ..\ from the GLScene.inc
      <li>12/07/03 - DanB - Added (De)RegisterXCollectionDestroyEvent
      <li>19/06/03 - DanB - Added TXCollection.GetOrCreate
      <li>18/02/01 - EG - Fixed TXCollectionItem.Destroy (count decrementation)
      <li>02/02/01 - EG - CanAdd now virtual
      <li>09/06/00 - EG - Added GetByClass
      <li>23/05/00 - EG - Added "Loaded" mechanism
      <li>17/04/00 - EG - Optimized TXCollection.Assign
	   <li>16/04/00 - EG - Creation from GLScene split
	</ul></font>
}
unit XCollection;

interface

uses Classes, SysUtils;

{$i GLScene.inc}

type

   TXCollection = class;

  	EFilerException = class(Exception)
	end;

	// TXCollectionItem
	//
	{: Base class for implementing a XCollection item.<p>
		NOTES :<ul>
		<li>Don't forget to override the ReadFromFiler/WriteToFiler persistence
			methods if you add data in a subclass !
		<li>Subclasses must be registered using the RegisterXCollectionItemClass
         function for proper operation
		</ul> }
	TXCollectionItem = class (TPersistent)
		private
			{ Private Declarations }
			FOwner : TXCollection;
			FName : String;

		protected
			{ Protected Declarations }
         procedure SetName(const val : String); virtual; 
         function GetOwner : TPersistent; override;

         {: Override this function to write subclass data. }
         procedure WriteToFiler(writer : TWriter); virtual;
         {: Override this function to read subclass data. }
         procedure ReadFromFiler(reader : TReader); virtual;
         {: Override to perform things when owner form has been loaded. }
         procedure Loaded; dynamic;

			{: Triggers an EFilerException with appropriate version message. }
			procedure RaiseFilerException(const archiveVersion : Integer);

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); virtual;
			destructor Destroy; override;

         function GetNamePath : String; override;
			property Owner : TXCollection read FOwner;

			{: Default implementation uses WriteToFiler/ReadFromFiler.<p> }
			procedure Assign(Source: TPersistent); override;

			procedure MoveUp;
			procedure MoveDown;
         function Index : Integer;

			{: Returns a user-friendly denomination for the class.<p>
				This denomination is used for picking a texture image class
				in the IDE expert. }
			class function FriendlyName : String; virtual; abstract;
			{: Returns a user-friendly description for the class.<p>
				This denomination is used for helping the user when picking a
				texture image class in the IDE expert. If it's not overriden,
				takes its value from FriendlyName. }
			class function FriendlyDescription : String; virtual;
         {: Category of the item class.<p>
            This is a free string, it will used by the XCollectionEditor to
            regroup collectionitems and menu items }
         class function ItemCategory : String; virtual;
			{: If true only one such XCollectionItem is allowed per BaseSceneObject.<p>
				Inheritance is accounted for UniqueXCollectionItem resolution, ie.
				if TClassA is unique, and TClassB is a subclass of TClassA,
				whatever the unicity of TClassB, TClassA and TClassB won't be allowed
				to mix (since TClassB is a TClassA, and TClassA is unique).<br>
				Attempting to break the unicity rules will not be possible at
				design-time (in Delphi IDE) and will trigger an exception at run-time. }
			class function UniqueItem : Boolean; virtual;
			{: Allows the XCollectionItem class to determine if it should be allowed
         to be added to the given collection. }
      class function CanAddTo(collection : TXCollection) : Boolean; virtual;

		published
			{ Published Declarations }
			property Name : String read FName write SetName;
	end;

	TXCollectionItemClass = class of TXCollectionItem;

	// TXCollection
	//
	{: Holds a list of TXCollectionItem objects.<p>
		This class looks a lot like a polymorphic-enabled TCollection, it is
		a much stripped down version of a proprietary TObjectList and persistence
		classes (XClasses & XLists), if the copyrights are ever partially lifted
		on the originals, I'll base this code on them since they are way faster
		than Borland's lists and persistence mechanisms (and unlike Borland's,
      with polymorphism-support and full backward compatibility). }
	TXCollection = class (TPersistent)
		private
			{ Private Declarations }
			FOwner : TPersistent;
			FList : TList;
         FCount : Integer;

		protected
			{ Protected Declarations }
			function GetItems(index : Integer) : TXCollectionItem;
         function GetOwner : TPersistent; override;

			procedure ReadFromFiler(reader : TReader);
			procedure WriteToFiler(writer : TWriter);

		public
			{ Public Declarations }
			constructor Create(aOwner : TPersistent); virtual;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
         procedure Loaded;

			property Owner : TPersistent read FOwner write FOwner;
         function GetNamePath : String; override;

         {: Class of the items.<p>
            Unlike TCollection, items can be of ItemsClass OR ANY of its
            subclasses, ie. this function is used only for asserting your adding
            objects of the right class, and not for persistence. }
         class function ItemsClass : TXCollectionItemClass; virtual;

			property Items[index : Integer] : TXCollectionItem read GetItems; default;
         property Count : Integer read FCount;
			function Add(anItem : TXCollectionItem) : Integer;
                        function GetOrCreate(anItem:TXCollectionItemClass) : TXCollectionItem;
			procedure Delete(index : Integer);
			procedure Remove(anItem : TXCollectionItem);
			procedure Clear;
			function IndexOf(anItem : TXCollectionItem) : Integer;
			//: Returns the index of the first XCollectionItem of the given class (or -1)
			function IndexOfClass(aClass : TXCollectionItemClass) : Integer;
         //: Returns the first XCollection of the given class (or nil)
         function GetByClass(aClass : TXCollectionItemClass) : TXCollectionItem;
			//: Returns the index of the first XCollectionItem of the given name (or -1)
			function IndexOfName(const aName : String) : Integer;
			{: Indicates if an object of the given class can be added.<p>
          	This function is used to enforce Unique XCollection. }
			function CanAdd(aClass : TXCollectionItemClass) : Boolean; virtual;
	end;

resourcestring
   cUnknownArchiveVersion = 'Unknown archive version : ';

{: Registers an event to be called when an XCollection is destroyed. }
procedure RegisterXCollectionDestroyEvent(notifyEvent : TNotifyEvent);
{: DeRegisters event. }
procedure DeRegisterXCollectionDestroyEvent(notifyEvent : TNotifyEvent);

{: Registers a TXCollectionItem subclass for persistence requirements. }
procedure RegisterXCollectionItemClass(aClass : TXCollectionItemClass);
{: Removes a TXCollectionItem subclass from the list. }
procedure UnregisterXCollectionItemClass(aClass : TXCollectionItemClass);
{: Retrieves a registered TXCollectionItemClass from its classname. }
function FindXCollectionItemClass(const className : String) : TXCollectionItemClass;
{: Creates and returns a copy of internal list of TXCollectionItem classes.<p>
	Returned list should be freed by caller, the parameter defines an ancestor
   class filter. If baseClass is left nil, TXCollectionItem is used as ancestor. }
function GetXCollectionItemClassesList(baseClass : TXCollectionItemClass = nil) : TList;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
  vXCollectionItemClasses : TList;
  vXCollectionDestroyEvent : TNotifyEvent;

//---------- internal global routines (used by xcollection editor) -------------

// RegisterXCollectionDestroyEvent
//
procedure RegisterXCollectionDestroyEvent(notifyEvent : TNotifyEvent);
begin
   vXCollectionDestroyEvent:=notifyEvent;
end;

// DeRegisterXCollectionDestroyEvent
//
procedure DeRegisterXCollectionDestroyEvent(notifyEvent : TNotifyEvent);
begin
   vXCollectionDestroyEvent:=nil;
end;

//------------------------------------------------------------------------------

// RegisterXCollectionItemClass
//
procedure RegisterXCollectionItemClass(aClass : TXCollectionItemClass);
begin
	if not Assigned(vXCollectionItemClasses) then
		vXCollectionItemClasses:=TList.Create;
	if vXCollectionItemClasses.IndexOf(aClass)<0 then
		vXCollectionItemClasses.Add(aClass);
end;

// UnregisterXCollectionItemClass
//
procedure UnregisterXCollectionItemClass(aClass : TXCollectionItemClass);
begin
	if not Assigned(vXCollectionItemClasses) then
		exit;
	if vXCollectionItemClasses.IndexOf(aClass)>=0 then
		vXCollectionItemClasses.Remove(aClass);
end;

// FindXCollectionItemClass
//
function FindXCollectionItemClass(const className : String) : TXCollectionItemClass;
var
	i : Integer;
begin
	Result:=nil;
	if Assigned(vXCollectionItemClasses) then
		for i:=0 to vXCollectionItemClasses.Count-1 do
			if TXCollectionItemClass(vXCollectionItemClasses[i]).ClassName=className then begin
				Result:=TXCollectionItemClass(vXCollectionItemClasses[i]);
				Break;
			end;
end;

// GetXCollectionItemClassesList
//
function GetXCollectionItemClassesList(baseClass : TXCollectionItemClass = nil) : TList;
var
	i : Integer;
begin
	Result:=TList.Create;
   if not Assigned(baseClass) then
      baseClass:=TXCollectionItem;
	if Assigned(vXCollectionItemClasses) then
		for i:=0 to vXCollectionItemClasses.Count-1 do
         if TXCollectionItemClass(vXCollectionItemClasses[i]).InheritsFrom(baseClass) then
   			Result.Add(vXCollectionItemClasses[i]);
end;

// ------------------
// ------------------ TXCollectionItem ------------------
// ------------------

// Create
//
constructor TXCollectionItem.Create(aOwner : TXCollection);
begin
	inherited Create;
	FOwner:=aOwner;
	if Assigned(aOwner) then begin
      Assert(aOwner.CanAdd(TXCollectionItemClass(Self.ClassType)),
             'Addition of '+Self.ClassName+' to '+aOwner.ClassName+' rejected.');
		aOwner.FList.Add(Self);
      aOwner.FCount:=aOwner.FList.Count;
   end;
	FName:=FriendlyName;
end;

// Destroy
//
destructor TXCollectionItem.Destroy;
begin
	if Assigned(FOwner) then begin
		FOwner.FList.Remove(Self);
      FOwner.FCount:=FOwner.FList.Count;
   end;
	inherited Destroy;
end;

// Assign
//
procedure TXCollectionItem.Assign(Source: TPersistent);
begin
   if Source is TXCollectionItem then begin
      FName:=TXCollectionItem(Source).Name;
   end else inherited Assign(Source);
end;

// SetName
//
procedure TXCollectionItem.SetName(const val : String);
begin
   FName:=val;
end;

// GetOwner
//
function TXCollectionItem.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// WriteToFiler
//
procedure TXCollectionItem.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      WriteString(FName);
   end;
end;

// ReadFromFiler
//
procedure TXCollectionItem.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      Assert(ReadInteger=0);
      FName:=ReadString;
   end;
end;

// Loaded
//
procedure TXCollectionItem.Loaded;
begin
   // does nothing by default
end;

// GetNamePath
//
function TXCollectionItem.GetNamePath : String;
begin
   if FOwner<>nil then
      Result:=Format('%s[%d]', [FOwner.GetNamePath, Index])
   else Result:=inherited GetNamePath;
end;

// MoveUp
//
procedure TXCollectionItem.MoveUp;
var
	i : Integer;
begin
   if Assigned(Owner) then begin
   	i:=Owner.FList.IndexOf(Self);
	   if i>0 then
		   Owner.FList.Exchange(i, i-1);
   end;
end;

// MoveDown
//
procedure TXCollectionItem.MoveDown;
var
	i : Integer;
begin
   if Assigned(Owner) then begin
   	i:=Owner.FList.IndexOf(Self);
	   if Cardinal(i)<Cardinal(Owner.FList.Count-1) then
		   Owner.FList.Exchange(i, i+1);
   end;
end;

// Index
//
function TXCollectionItem.Index : Integer;
begin
   if Assigned(Owner) then
      Result:=Owner.FList.IndexOf(Self)
   else Result:=-1;
end;

// RaiseFilerException
//
procedure TXCollectionItem.RaiseFilerException(const archiveVersion : Integer);
begin
	raise EFilerException.Create(ClassName+cUnknownArchiveVersion
										  +IntToStr(archiveVersion));
end;

// FriendlyDescription
//
class function TXCollectionItem.FriendlyDescription : String;
begin
	Result:=FriendlyName;
end;

// ItemCategory
//
class function TXCollectionItem.ItemCategory : String;
begin
   Result:='';
end;

// UniqueXCollectionItem
//
class function TXCollectionItem.UniqueItem : Boolean;
begin
	Result:=False;
end;

// CanAddTo
//
class function TXCollectionItem.CanAddTo(collection : TXCollection) : Boolean;
begin
  Result:=True;
end;


// ------------------
// ------------------ TXCollection ------------------
// ------------------

// Create
//
constructor TXCollection.Create(aOwner : TPersistent);
begin
	inherited Create;
	FOwner:=aOwner;
	FList:=TList.Create;
end;

// Destroy
//
destructor TXCollection.Destroy;
begin
        if Assigned(vXCollectionDestroyEvent) then
                vXCollectionDestroyEvent(Self);
	Clear;
	FList.Free;
	inherited Destroy;
end;

// Assign
//
procedure TXCollection.Assign(Source: TPersistent);
var
   i : Integer;
   srcItem, newItem : TXCollectionItem;
begin
	if not Assigned(Source) then begin
		Clear;
	end else if Source.ClassType=Self.ClassType then begin
      Clear;
      FList.Capacity:=TXCollection(Source).FList.Count;
      for i:=0 to TXCollection(Source).Count-1 do begin
         srcItem:=TXCollectionItem(TXCollection(Source).FList[i]);
         newItem:=TXCollectionItemClass(srcItem.ClassType).Create(Self);
         newItem.Assign(srcItem);
      end;
	end else inherited Assign(Source);
   FCount:=FList.Count;
end;

// Loaded
//
procedure TXCollection.Loaded;
var
   i : Integer;
begin
   for i:=0 to FList.Count-1 do
      TXCollectionItem(FList[i]).Loaded;
end;

// WriteToFiler
//
procedure TXCollection.WriteToFiler(writer : TWriter);
var
	i, n : Integer;
	classList : TList;
	XCollectionItem : TXCollectionItem;
begin
	// Here, we write all listed XCollection through their WriteToFiler methods,
	// but to be able to restore them, we also write their classname, and to
	// avoid wasting space if the same class appears multiple times we build up
	// a lookup table while writing them, if the class is anew, the name is
	// written, otherwise, only the index in the table is written.
	// Using a global lookup table (instead of a "per-WriteData" one) could save
	// more space, but would also increase dependencies, and this I don't want 8)
	classList:=TList.Create;
	try
		with writer do begin
			WriteInteger(FList.Count);
			for i:=0 to FList.Count-1 do begin
				XCollectionItem:=TXCollectionItem(FList[i]);
				n:=classList.IndexOf(XCollectionItem.ClassType);
				if n<0 then begin
					WriteString(XCollectionItem.ClassName);
					classList.Add(XCollectionItem.ClassType);
				end else WriteInteger(n);
            XCollectionItem.WriteToFiler(writer);
			end;
		end;
	finally
		classList.Free;
	end;
end;

// ReadFromFiler
//
procedure TXCollection.ReadFromFiler(reader : TReader);
var
	n : Integer;
	classList : TList;
	cName : String;
	XCollectionItemClass : TXCollectionItemClass;
	XCollectionItem : TXCollectionItem;
begin
	// see WriteData for a description of what is going on here
	Clear;
	classList:=TList.Create;
	try
		with reader do begin
			for n:=1 to ReadInteger do begin
				if NextValue in [vaString, vaLString] then begin
					cName:=ReadString;
					XCollectionItemClass:=FindXCollectionItemClass(cName);
					Assert(Assigned(XCollectionItemClass),
                      'Class '+cName+' unknown. Add the relevant unit to your "uses".');
					classList.Add(XCollectionItemClass);
				end else XCollectionItemClass:=TXCollectionItemClass(classList[ReadInteger]);
				XCollectionItem:=XCollectionItemClass.Create(Self);
            XCollectionItem.ReadFromFiler(reader);
			end;
		end;
	finally
		classList.Free;
	end;
   FCount:=FList.Count;
end;

// ItemsClass
//
class function TXCollection.ItemsClass : TXCollectionItemClass;
begin
   Result:=TXCollectionItem;
end;

// GetItems
//
function TXCollection.GetItems(index : Integer) : TXCollectionItem;
begin
	Result:=TXCollectionItem(FList[index]);
end;

// GetOwner
//
function TXCollection.GetOwner : TPersistent;
begin
   Result:=FOwner;
end;

// GetNamePath
//
function TXCollection.GetNamePath : String;
var
   s : String;
begin
   Result:=ClassName;
   if GetOwner=nil then Exit;
   s:=GetOwner.GetNamePath;
   if s='' then Exit;
   Result:=s+'.XCollection';
end;

// Add
//
function TXCollection.Add(anItem : TXCollectionItem) : Integer;
begin
   Assert(anItem.InheritsFrom(ItemsClass));
	Assert(CanAdd(TXCollectionItemClass(anItem.ClassType)));
   if Assigned(anItem.FOwner) then begin
      anItem.FOwner.FList.Remove(anItem);
      anItem.FOwner.FCount:=anItem.FOwner.FList.Count;
   end;
   anItem.FOwner:=Self;
   Result:=FList.Add(anItem);
   FCount:=FList.Count;
end;

// GetOrCreate
//
function TXCollection.GetOrCreate(anItem:TXCollectionItemClass) : TXCollectionItem;
var
	i : Integer;
begin
   Assert(anItem.InheritsFrom(ItemsClass));
	i:=Self.IndexOfClass(anItem);
	if i>=0 then
		Result:=TXCollectionItem(Self[i])
	else Result:=anItem.Create(Self);
end;

// Delete
//
procedure TXCollection.Delete(index : Integer);
begin
	Assert(Cardinal(index)<Cardinal(FList.Count));
	// doin' it the fast way
	with TXCollectionItem(FList[index]) do begin
		FOwner:=nil;
		Free;
	end;
	FList.Delete(index);
   FCount:=FList.Count;
end;

// Remove
//
procedure TXCollection.Remove(anItem : TXCollectionItem);
var
	i : Integer;
begin
	i:=IndexOf(anItem);
	if i>=0 then Delete(i);
end;

// Clear
//
procedure TXCollection.Clear;
var
	i : Integer;
begin
	// Fast kill of owned XCollection
	for i:=0 to FList.Count-1 do
		with TXCollectionItem(FList[i]) do begin
			FOwner:=nil;
			Free;
		end;
	FList.Clear;
   FCount:=0;
end;

// IndexOf
//
function TXCollection.IndexOf(anItem : TXCollectionItem) : Integer;
begin
	Result:=FList.IndexOf(anItem);
end;

// IndexOfClass
//
function TXCollection.IndexOfClass(aClass : TXCollectionItemClass) : Integer;
var
	i : Integer;
begin
	Result:=-1;
	for i:=0 to FList.Count-1 do
		if TXCollectionItem(FList[i]) is aClass then begin
			Result:=i;
			Break;
		end;
end;

// GetByClass
//
function TXCollection.GetByClass(aClass : TXCollectionItemClass) : TXCollectionItem;
var
	i : Integer;
begin
	Result:=nil;
	for i:=0 to FList.Count-1 do
		if TXCollectionItem(FList[i]) is aClass then begin
			Result:=TXCollectionItem(FList[i]);
			Break;
		end;
end;

// IndexOfName
//
function TXCollection.IndexOfName(const aName : String) : Integer;
var
	i : Integer;
begin
	Result:=-1;
	for i:=0 to FList.Count-1 do
		if TXCollectionItem(FList[i]).Name=aName then begin
			Result:=i;
			Break;
		end;
end;

// CanAdd
//
function TXCollection.CanAdd(aClass : TXCollectionItemClass) : Boolean;
var
	i : Integer;
	XCollectionItemClass : TXCollectionItemClass;
begin
	Result:=True;

  // Test if the class allows itself to be added to this collection
  if not aClass.CanAddTo(Self) then begin
    Result:=False;
    Exit;
  end;  
  
	// is the given class compatible with owned ones ?
	if aClass.UniqueItem then for i:=0 to Count-1 do begin
		if Items[i] is aClass then begin
			Result:=False;
			Break;
		end;
	end;
	// are the owned classes compatible with the given one ?
	if Result then for i:=0 to Count-1 do begin
		XCollectionItemClass:=TXCollectionItemClass(Items[i].ClassType);
		if (XCollectionItemClass.UniqueItem) and aClass.InheritsFrom(XCollectionItemClass) then begin
			Result:=False;
			Break;
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

finalization

	vXCollectionItemClasses.Free;

end.

