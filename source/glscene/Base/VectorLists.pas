//
// This unit is part of the GLScene Project, http://glscene.org
//
{: VectorLists<p>

	Misc. lists of vectors and entities<p>

	<b>History : </b><font size=-1><ul>
      <li>28/06/04 - LR - Removed ..\ from the GLScene.inc
      <li>03/09/03 - EG - Added TBaseList.Move, faster TIntegerList.Offset
      <li>22/08/03 - EG - Faster FastQuickSortLists
      <li>13/08/03 - SG - Added TQuaternionList
      <li>05/06/03 - EG - Added MinInteger, some TIntegerList optimizations
      <li>03/06/03 - EG - Added TIntegerList.BinarySearch and AddSorted (Mattias Fagerlund)
      <li>22/01/03 - EG - Added AddIntegers
      <li>20/01/03 - EG - Added TIntegerList.SortAndRemoveDuplicates
      <li>22/10/02 - EG - Added TransformXxxx to TAffineVectorList
      <li>04/07/02 - EG - Fixed TIntegerList.Add( 2 at once )
      <li>15/06/02 - EG - Added TBaseListOption stuff
      <li>28/05/02 - EG - TBaseList.SetCount now properly resets new items
      <li>23/02/02 - EG - Added TBaseList.UseMemory
      <li>20/01/02 - EG - Now uses new funcs Add/ScaleVectorArray and VectorArrayAdd
      <li>06/12/01 - EG - Added Sort & MaxInteger to TIntegerList
      <li>04/12/01 - EG - Added TIntegerList.IndexOf
      <li>18/08/01 - EG - Fixed TAffineVectorList.Add (list)
      <li>03/08/01 - EG - Added TIntegerList.AddSerie
      <li>19/07/01 - EG - Added TAffineVectorList.Add (list variant)
      <li>18/03/01 - EG - Additions and enhancements
      <li>16/03/01 - EG - Introduced new PersistentClasses
      <li>04/03/01 - EG - Optimized TAffineVectorList.Normalize (x2 speed on K7)
      <li>26/02/01 - EG - VectorArrayLerp 3DNow optimized (x2 speed on K7)
      <li>08/08/00 - EG - Added TSingleList
	   <li>20/07/00 - EG - Creation
	</ul></font>
}
unit VectorLists;

interface

{$i GLScene.inc}

uses Classes, VectorTypes, VectorGeometry, PersistentClasses, SysUtils;

type

   // TBaseListOption
   //
   TBaseListOption = (bloExternalMemory, bloSetCountResetsMemory);
   TBaseListOptions = set of TBaseListOption;

   // TBaseList
   //
   {: Base class for lists, introduces common behaviours. }
   TBaseList = class (TPersistentObject)
		private
         { Private Declarations }
			FCount : Integer;
			FCapacity : Integer;
			FGrowthDelta : Integer;
         FBufferItem : PByteArray;
         FOptions : TBaseListOptions;

		protected
         { Protected Declarations }
         //: The base list pointer (untyped)
         FBaseList : VectorGeometry.PByteArray;
         //: Must be defined by all subclasses in their constructor(s)
         FItemSize : Integer;

         procedure SetCount(val : Integer);
         {: Only function where list may be alloc'ed & freed.<p>
            Resizes the array pointed by FBaseList, adjust the subclass's
            typed pointer accordingly if any. }
			procedure SetCapacity(NewCapacity: Integer); virtual;
         function BufferItem : PByteArray;
         function GetSetCountResetsMemory : Boolean;
         procedure SetSetCountResetsMemory(const val : Boolean);

		public
         { Public Declarations }
         constructor Create; override;
			destructor Destroy; override;
         procedure Assign(Src: TPersistent); override;

			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure AddNulls(nbVals : Cardinal);
			procedure InsertNulls(index : Integer; nbVals : Cardinal);

         procedure AdjustCapacityToAtLeast(const size : Integer);
         function DataSize : Integer;
         {: Tell the list to use the specified range instead of its own.<p>
            rangeCapacity should be expressed in bytes.<p>
            The allocated memory is NOT managed by the list, current content
            if copied to the location, if the capacity is later changed, regular
            memory will be allocated, and the specified range no longer used. }
         procedure UseMemory(rangeStart : Pointer; rangeCapacity : Integer);
         {: Empties the list without altering capacity. }
         procedure Flush;
         {: Empties the list and release. }
         procedure Clear;

         procedure Delete(index : Integer);
         procedure DeleteItems(index : Integer; nbVals : Cardinal);
         procedure Exchange(index1, index2 : Integer);
         procedure Move(curIndex, newIndex : Integer);
         procedure Reverse;

         {: Nb of items in the list.<p>
            When assigning a Count, added items are reset to zero. }
			property Count : Integer read FCount write SetCount;
         {: Current list capacity.<p>
            Not persistent. }
			property Capacity : Integer read FCapacity write SetCapacity;
			{: List growth granularity.<p>
            Not persistent. }
			property GrowthDelta : Integer read FGrowthDelta write FGrowthDelta;
         {: If true (default value) adjusting count will reset added values.<p>
            Switching this option to true will turn off this memory reset,
            which can improve performance is that having empty values isn't
            required. }
         property SetCountResetsMemory : Boolean read GetSetCountResetsMemory write SetSetCountResetsMemory;
   end;

   // TBaseVectorList
   //
   {: Base class for vector lists, introduces common behaviours. }
   TBaseVectorList = class (TBaseList)
		private
         { Private Declarations }

		protected
         { Protected Declarations }
         function GetItemAddress(index : Integer) : PFloatArray;

		public
         { Public Declarations }
			procedure WriteToFiler(writer : TVirtualWriter); override;
			procedure ReadFromFiler(reader : TVirtualReader); override;

         procedure GetExtents(var min, max : TAffineVector); dynamic;
         function Sum : TAffineVector; dynamic;
         procedure Normalize; dynamic;
         function MaxSpacing(list2 : TBaseVectorList) : Single; dynamic;
         procedure Translate(const delta : TAffineVector); overload; dynamic;
         procedure Translate(const delta : TBaseVectorList); overload; dynamic;
         procedure TranslateInv(const delta : TBaseVectorList); overload; dynamic;

         {: Replace content of the list with lerp results between the two given lists.<p>
            Note: you can't Lerp with Self!!! }
         procedure Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single); dynamic; abstract;
         {: Replace content of the list with angle lerp between the two given lists.<p>
            Note: you can't Lerp with Self!!! }
         procedure AngleLerp(const list1, list2 : TBaseVectorList; lerpFactor : Single);
         procedure AngleCombine(const list1 : TBaseVectorList; intensity : Single);
         {: Linear combination of Self with another list.<p>
            Self[i]:=Self[i]+list2[i]*factor }
         procedure Combine(const list2 : TBaseVectorList; factor : Single); dynamic;

         property ItemAddress[index : Integer] : PFloatArray read GetItemAddress;
   end;

  	// TAffineVectorList
	//
	{: A list of TAffineVector.<p>
		Similar to TList, but using TAffineVector as items.<p>
      The list has stack-like push/pop methods. }
	TAffineVectorList = class (TBaseVectorList)
		private
         { Private Declarations }
			FList: PAffineVectorArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TAffineVector;
			procedure Put(Index: Integer; const item : TAffineVector);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src: TPersistent); override;

			function  Add(const item : TAffineVector) : Integer; overload;
			function  Add(const item : TVector) : Integer; overload;
			procedure Add(const i1, i2 : TAffineVector); overload;
			procedure Add(const i1, i2, i3 : TAffineVector); overload;
			function  Add(const item : TVector2f) : Integer; overload;
			function  Add(const item : TTexPoint) : Integer; overload;
			function  Add(const x, y : Single) : Integer; overload;
			function  Add(const x, y, z : Single) : Integer; overload;
			function  Add(const x, y, z : Integer) : Integer; overload;
			function  AddNC(const x, y, z : Integer) : Integer; overload;
			function  Add(const xy : PIntegerArray; const z : Integer) : Integer; overload;
			function  AddNC(const xy : PIntegerArray; const z : Integer) : Integer; overload;
         procedure Add(const list : TAffineVectorList); overload;
			procedure Push(const val : TAffineVector);
			function  Pop : TAffineVector;
			procedure Insert(Index: Integer; const item : TAffineVector);
         function  IndexOf(const item : TAffineVector) : Integer;
         function  FindOrAdd(const item : TAffineVector) : Integer;

			property Items[Index: Integer] : TAffineVector read Get write Put; default;
			property List : PAffineVectorArray read FList;

         procedure Translate(const delta : TAffineVector); overload; override;
         procedure Translate(const delta : TAffineVector; base, nb : Integer); overload;

         //: Translates the given item
         procedure TranslateItem(index : Integer; const delta : TAffineVector);
         //: Translates given items
         procedure TranslateItems(index : Integer; const delta : TAffineVector;
                                  nb : Integer);
         //: Combines the given item
         procedure CombineItem(index : Integer; const vector : TAffineVector; const f : Single);

         {: Transforms all items by the matrix as if they were points.<p>
            ie. the translation component of the matrix is honoured. }
         procedure TransformAsPoints(const matrix : TMatrix);
         {: Transforms all items by the matrix as if they were vectors.<p>
            ie. the translation component of the matrix is not honoured. }
         procedure TransformAsVectors(const matrix : TMatrix); overload;
         procedure TransformAsVectors(const matrix : TAffineMatrix); overload;

         procedure Normalize; override;
         procedure Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single); override;

         procedure Scale(factor : Single); overload;
         procedure Scale(const factors : TAffineVector); overload;
	end;

  	// TVectorList
	//
	{: A list of TVector.<p>
		Similar to TList, but using TVector as items.<p>
      The list has stack-like push/pop methods. }
	TVectorList = class (TBaseVectorList)
		private
         { Private Declarations }
			FList: PVectorArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TVector;
			procedure Put(Index: Integer; const item : TVector);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TVector) : Integer; overload;
			function Add(const item : TAffineVector; w : Single) : Integer; overload;
			function Add(const x, y, z, w : Single) : Integer; overload;
			procedure Add(const i1, i2, i3 : TAffineVector; w : Single); overload;
			function AddVector(const item : TAffineVector) : Integer; overload;
			function AddPoint(const item : TAffineVector) : Integer; overload;
			function AddPoint(const x, y : Single; const z : Single = 0) : Integer; overload;
			procedure Push(const val : TVector);
			function  Pop : TVector;
         function  IndexOf(const item : TVector) : Integer;
         function  FindOrAdd(const item : TVector) : Integer;
         function  FindOrAddPoint(const item : TAffineVector) : Integer;
			procedure Insert(Index: Integer; const item : TVector);

			property Items[Index: Integer] : TVector read Get write Put; default;
			property List: PVectorArray read FList;

         procedure Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single); override;
	end;

  	// TTexPointList
	//
	{: A list of TTexPoint.<p>
		Similar to TList, but using TTexPoint as items.<p>
      The list has stack-like push/pop methods. }
	TTexPointList = class (TBaseList)
		private
         { Private Declarations }
			FList: PTexPointArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TTexPoint;
			procedure Put(Index: Integer; const item : TTexPoint);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TTexPoint) : Integer; overload;
			function Add(const item : TVector2f) : Integer; overload;
			function Add(const texS, texT : Single) : Integer; overload;
			function Add(const texS, texT : Integer) : Integer; overload;
			function AddNC(const texS, texT : Integer) : Integer; overload;
			function Add(const texST : PIntegerArray) : Integer; overload;
			function AddNC(const texST : PIntegerArray) : Integer; overload;
			procedure Push(const val : TTexPoint);
			function  Pop : TTexPoint;
			procedure Insert(Index: Integer; const item : TTexPoint);

			property Items[Index: Integer] : TTexPoint read Get write Put; default;
			property List: PTexPointArray read FList;

         procedure Translate(const delta : TTexPoint);
         procedure ScaleAndTranslate(const scale, delta : TTexPoint); overload;
         procedure ScaleAndTranslate(const scale, delta : TTexPoint;
                                     base, nb : Integer); overload;
	end;

  	// TIntegerList
	//
	{: A list of Integers.<p>
		Similar to TList, but using TTexPoint as items.<p>
      The list has stack-like push/pop methods. }
	TIntegerList = class (TBaseList)
		private
         { Private Declarations }
			FList : PIntegerArray;

		protected
         { Protected Declarations }
			function  Get(index : Integer) : Integer;
			procedure Put(index : Integer; const item : Integer);
			procedure SetCapacity(newCapacity : Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(src : TPersistent); override;

			function  Add(const item : Integer) : Integer; overload;
         function  AddNC(const item : Integer) : Integer; overload;
         procedure Add(const i1, i2 : Integer); overload;
         procedure Add(const i1, i2, i3 : Integer); overload;
         procedure Add(const list : TIntegerList); overload;
			procedure Push(const val : Integer);
			function  Pop : Integer;
			procedure Insert(index : Integer; const item : Integer);
         procedure Remove(const item : Integer);
         function  IndexOf(item : Integer): Integer;

			property Items[Index : Integer] : Integer read Get write Put; default;
			property List : PIntegerArray read FList;

         {: Adds count items in an arithmetic serie.<p>
            Items are (aBase), (aBase+aDelta) ... (aBase+(aCount-1)*aDelta) }
         procedure AddSerie(aBase, aDelta, aCount : Integer);
         {: Add n integers at the address starting at (and including) first. }
         procedure AddIntegers(const first : PInteger; n : Integer); overload;
         {: Add all integers from aList into the list. }
         procedure AddIntegers(const aList : TIntegerList); overload;
         {: Add all integers from anArray into the list. }
         procedure AddIntegers(const anArray : array of Integer); overload;

         {: Returns the minimum integer item, zero if list is empty. }
         function  MinInteger : Integer;
         {: Returns the maximum integer item, zero if list is empty. }
         function  MaxInteger : Integer;
         {: Sort items in ascending order. }
         procedure Sort;
         {: Sort items in ascending order and remove duplicated integers. }
         procedure SortAndRemoveDuplicates;

         {: Locate a value in a sorted list. }
         function BinarySearch(const value : Integer) : Integer; overload;
         {: Locate a value in a sorted list.<p>
            If ReturnBestFit is set to true, the routine will return the position
            of the largest value that's smaller than the sought value. Found will
            be set to True if the exact value was found, False if a "BestFit"
            was found. }
         function BinarySearch(const value : Integer; returnBestFit : Boolean; var found : Boolean) : Integer; overload;

         {: Add integer to a sorted list.<p>
            Maintains the list sorted. If you have to add "a lot" of integers
            at once, use the Add method then Sort the list for better performance. }
         function AddSorted(const value : Integer; const ignoreDuplicates : Boolean = False) : Integer;
         {: Removes an integer from a sorted list.<p> }
         procedure RemoveSorted(const value : Integer);

         {: Adds delta to all items in the list. }
         procedure Offset(delta : Integer); overload;
         procedure Offset(delta : Integer; const base, nb : Integer); overload;
	end;

   TSingleArrayList = array [0..MaxInt shr 4] of Single;
   PSingleArrayList = ^TSingleArrayList;

  	// TSingleList
	//
	{: A list of Single.<p>
		Similar to TList, but using Single as items.<p>
      The list has stack-like push/pop methods. }
	TSingleList = class (TBaseList)
		private
         { Private Declarations }
			FList : PSingleArrayList;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : Single;
			procedure Put(Index: Integer; const item : Single);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src : TPersistent); override;

			function Add(const item : Single) : Integer;
			procedure Push(const val : Single);
			function Pop : Single;
			procedure Insert(Index : Integer; const item : Single);

			property Items[Index: Integer] : Single read Get write Put; default;
			property List : PSingleArrayList read FList;

         procedure AddSerie(aBase, aDelta : Single; aCount : Integer);
         
         {: Adds delta to all items in the list. }
	      procedure Offset(delta : Single); overload;
         {: Adds to each item the corresponding item in the delta list.<p>
            Performs 'Items[i]:=Items[i]+delta[i]'.<br>
            If both lists don't have the same item count, an exception is raised. }
         procedure Offset(const delta : TSingleList); overload;
         {: Multiplies all items by factor. }
	      procedure Scale(factor : Single);
         {: Square all items. }
         procedure Sqr;
         {: SquareRoot all items. }
         procedure Sqrt;

         {: Computes the sum of all elements. }
         function  Sum : Single;
	end;

  	// TByteList
	//
	{: A list of bytes.<p>
		Similar to TList, but using Byte as items.<p> }
	TByteList = class (TBaseList)
		private
         { Private Declarations }
			FList: PByteArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : Byte;
			procedure Put(Index: Integer; const item : Byte);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src : TPersistent); override;

			function Add(const item : Byte) : Integer;
			procedure Insert(Index : Integer; const item : Byte);

			property Items[Index: Integer] : Byte read Get write Put; default;
			property List : PByteArray read FList;

	end;

  	// TQuaternionList
	//
	{: A list of TQuaternion.<p>
	   Similar to TList, but using TQuaternion as items.<p>
       The list has stack-like push/pop methods. }
	TQuaternionList = class (TBaseVectorList)
		private
         { Private Declarations }
			FList: PQuaternionArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TQuaternion;
			procedure Put(Index: Integer; const item : TQuaternion);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create; override;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TQuaternion) : Integer; overload;
			function Add(const item : TAffineVector; w : Single) : Integer; overload;
			function Add(const x, y, z, w : Single) : Integer; overload;
			procedure Push(const val : TQuaternion);
			function  Pop : TQuaternion;
         function  IndexOf(const item : TQuaternion) : Integer;
         function  FindOrAdd(const item : TQuaternion) : Integer;
			procedure Insert(Index: Integer; const item : TQuaternion);

			property Items[Index: Integer] : TQuaternion read Get write Put; default;
			property List: PQuaternionArray read FList;

         {: Lerps corresponding quaternions from both lists using QuaternionSlerp. }
         procedure Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single); override;
         {: Multiplies corresponding quaternions after the second quaternion is
            slerped with the IdentityQuaternion using factor. This allows for weighted
            combining of rotation transforms using quaternions. }
         procedure Combine(const list2 : TBaseVectorList; factor : Single); override;
	end;

{: Sort the refList in ascending order, ordering objList (TList) on the way. }
procedure QuickSortLists(startIndex, endIndex : Integer;
							    refList : TSingleList; objList : TList); overload;

{: Sort the refList in ascending order, ordering objList (TBaseList) on the way. }
procedure QuickSortLists(startIndex, endIndex : Integer;
								 refList : TSingleList; objList : TBaseList); overload;

{: Sort the refList in ascending order, ordering objList on the way.<p>
   Use if, and *ONLY* if refList contains only values superior or equal to 1. }
procedure FastQuickSortLists(startIndex, endIndex : Integer;
				      			  refList : TSingleList; objList : TPersistentObjectList);


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultListGrowthDelta = 16;

// QuickSortLists (TList)
//
procedure QuickSortLists(startIndex, endIndex : Integer;
								 refList : TSingleList; objList : TList);
var
	I, J : Integer;
	P : Single;
begin
	if endIndex-startIndex>1 then begin
		repeat
			I:=startIndex; J:=endIndex;
			P:=refList.List[(I + J) shr 1];
			repeat
				while Single(refList.List[I])<P do Inc(I);
				while Single(refList.List[J])>P do Dec(J);
				if I <= J then begin
					refList.Exchange(I, J);
					objList.Exchange(I, J);
					Inc(I); Dec(J);
				end;
			until I > J;
			if startIndex < J then
				QuickSortLists(startIndex, J, refList, objList);
			startIndex:=I;
		until I >= endIndex;
	end else if endIndex-startIndex>0 then begin
		p:=refList.List[startIndex];
		if refList.List[endIndex]<p then begin
			refList.Exchange(startIndex, endIndex);
			objList.Exchange(startIndex, endIndex);
		end;
	end;
end;

// QuickSortLists (TBaseList)
//
procedure QuickSortLists(startIndex, endIndex : Integer;
								 refList : TSingleList; objList : TBaseList);
var
	I, J : Integer;
	P : Single;
begin
	if endIndex-startIndex>1 then begin
		repeat
			I:=startIndex; J:=endIndex;
			P:=refList.List[(I + J) shr 1];
			repeat
				while Single(refList.List[I])<P do Inc(I);
				while Single(refList.List[J])>P do Dec(J);
				if I <= J then begin
					refList.Exchange(I, J);
					objList.Exchange(I, J);
					Inc(I); Dec(J);
				end;
			until I > J;
			if startIndex < J then
				QuickSortLists(startIndex, J, refList, objList);
			startIndex:=I;
		until I >= endIndex;
	end else if endIndex-startIndex>0 then begin
		p:=refList.List[startIndex];
		if refList.List[endIndex]<p then begin
			refList.Exchange(startIndex, endIndex);
			objList.Exchange(startIndex, endIndex);
		end;
	end;
end;

// FastQuickSortLists
//
procedure FastQuickSortLists(startIndex, endIndex : Integer;
								     refList : TSingleList; objList : TPersistentObjectList);
var
	i, j : Integer;
	p, temp : Integer;
   refInts : PIntegerArray;
   ppl : PIntegerArray;
begin
   // All singles are >=1, so IEEE format allows comparing them as if they were integers
   refInts:=PIntegerArray(@refList.List[0]);
	if endIndex>startIndex+1 then begin
		repeat
			i:=startIndex; j:=endIndex;
			p:=PInteger(@refList.List[(i+j) shr 1])^;
			repeat
            ppl:=refInts;
				while ppl[i]<p do Inc(i);
				while ppl[j]>p do Dec(j);
				if i<=j then begin
               temp:=ppl[i]; ppl[i]:=ppl[j]; ppl[j]:=temp;
               ppl:=PIntegerArray(objList.List);
               temp:=ppl[i]; ppl[i]:=ppl[j]; ppl[j]:=temp;
					Inc(i); Dec(j);
				end;
			until i>j;
			if startIndex<j then
				FastQuickSortLists(startIndex, j, refList, objList);
			startIndex:=i;
		until i>=endIndex;
	end else if endIndex>startIndex then begin
      ppl:=refInts;
		if ppl[endIndex]<ppl[startIndex] then begin
         i:=endIndex; j:=startIndex;
         temp:=ppl[i]; ppl[i]:=ppl[j]; ppl[j]:=temp;
         ppl:=PIntegerArray(objList.List);
         temp:=ppl[i]; ppl[i]:=ppl[j]; ppl[j]:=temp;
		end;
	end;
end;

// ------------------
// ------------------ TBaseList ------------------
// ------------------

// Create
//
constructor TBaseList.Create;
begin
	inherited Create;
   FOptions:=[bloSetCountResetsMemory];
end;

// Destroy
//
destructor TBaseList.Destroy;
begin
   Clear;
   if Assigned(FBufferItem) then
      FreeMem(FBufferItem);
   inherited;
end;

// Assign
//
procedure TBaseList.Assign(Src: TPersistent);
begin
   if (Src is TBaseList) then begin
      SetCapacity(TBaseList(Src).Count);
   	FGrowthDelta:=TAffineVectorList(Src).FGrowthDelta;
		FCount:=FCapacity;
   end else inherited;
end;

// WriteToFiler
//
procedure TBaseList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      WriteInteger(Count);
      WriteInteger(FItemSize);
      if Count>0 then
         Write(FBaseList[0], Count*FItemSize);
   end;
end;

// ReadFromFiler
//
procedure TBaseList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited;
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      FCount:=ReadInteger;
      FItemSize:=ReadInteger;
      SetCapacity(Count);
      if Count>0 then
         Read(FBaseList[0], Count*FItemSize);
   end else RaiseFilerException(archiveVersion);
end;

// SetCount
//
procedure TBaseList.SetCount(val : Integer);
begin
   Assert(val>=0);
   if val>FCapacity then
      SetCapacity(val);
   if (val>FCount) and (bloSetCountResetsMemory in FOptions) then
      FillChar(FBaseList[FItemSize*FCount], (val-FCount)*FItemSize, 0);
   FCount:=val;
end;

// SetCapacity
//
procedure TBaseList.SetCapacity(newCapacity: Integer);
begin
	if newCapacity<>FCapacity then begin
      if bloExternalMemory in FOptions then begin
         Exclude(FOptions, bloExternalMemory);
         FBaseList:=nil;
      end;
		ReallocMem(FBaseList, newCapacity*FItemSize);
		FCapacity:=newCapacity;
	end;
end;

// AddNulls
//
procedure TBaseList.AddNulls(nbVals : Cardinal);
begin
   if Integer(nbVals)+Count>Capacity then
      SetCapacity(Integer(nbVals)+Count);
   FillChar(FBaseList[FCount*FItemSize], Integer(nbVals)*FItemSize, 0);
   FCount:=FCount+Integer(nbVals);
end;

// InsertNulls
//
procedure TBaseList.InsertNulls(index : Integer; nbVals : Cardinal);
var
   nc : Integer;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
   if nbVals>0 then begin
      nc:=FCount+Integer(nbVals);
	   if nc>FCapacity then
         SetCapacity(nc);
   	if Index<FCount then
	   	System.Move(FBaseList[Index*FItemSize],
                     FBaseList[(Index+Integer(nbVals))*FItemSize],
                     (FCount-Index)*FItemSize);
      FillChar(FBaseList[Index*FItemSize], Integer(nbVals)*FItemSize, 0);
	   FCount:=nc;
   end;
end;

// AdjustCapacityToAtLeast
//
procedure TBaseList.AdjustCapacityToAtLeast(const size : Integer);
begin
   if Capacity<size then
      Capacity:=size;
end;

// DataSize
//
function TBaseList.DataSize : Integer;
begin
   Result:=FItemSize*FCount;
end;

// BufferItem
//
function TBaseList.BufferItem : PByteArray;
begin
   if not Assigned(FBufferItem) then
      GetMem(FBufferItem, FItemSize);
   Result:=FBufferItem;
end;

// GetSetCountResetsMemory
//
function TBaseList.GetSetCountResetsMemory : Boolean;
begin
   Result:=(bloSetCountResetsMemory in FOptions);
end;

// SetSetCountResetsMemory
//
procedure TBaseList.SetSetCountResetsMemory(const val : Boolean);
begin
   if val then
      Include(FOptions, bloSetCountResetsMemory)
   else Exclude(FOptions, bloSetCountResetsMemory);
end;

// UseMemory
//
procedure TBaseList.UseMemory(rangeStart : Pointer; rangeCapacity : Integer);
begin
   rangeCapacity:=rangeCapacity div FItemSize;
   if rangeCapacity<FCount then Exit;
   // transfer data
   System.Move(FBaseList^, rangeStart^, FCount*FItemSize);
   if not (bloExternalMemory in FOptions) then begin
      FreeMem(FBaseList);
      Include(FOptions, bloExternalMemory);
   end;
   FBaseList:=rangeStart;
   FCapacity:=rangeCapacity;
   SetCapacity(FCapacity); // notify subclasses
end;

// Flush
//
procedure TBaseList.Flush;
begin
	if Assigned(Self) then begin
      SetCount(0);
	end;
end;

// Clear
//
procedure TBaseList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;

// Delete
//
procedure TBaseList.Delete(index: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
	Dec(FCount);
	if index<FCount then
		System.Move(FBaseList[(index+1)*FItemSize],
                  FBaseList[index*FItemSize],
                  (FCount-index)*FItemSize);
end;

// DeleteItems
//
procedure TBaseList.DeleteItems(index : Integer; nbVals : Cardinal);
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
   if nbVals>0 then begin
      if index+Integer(nbVals)<FCount then begin
	   	System.Move(FBaseList[(index+Integer(nbVals))*FItemSize],
                     FBaseList[index*FItemSize],
                     (FCount-index-Integer(nbVals))*FItemSize);
      end;
	   Dec(FCount, nbVals);
   end;
end;

// Exchange
//
procedure TBaseList.Exchange(index1, index2: Integer);
var
   buf : Integer;
   p : PIntegerArray;
begin
{$IFOPT R+}
	Assert((Cardinal(index1)<Cardinal(FCount)) and (Cardinal(index2)<Cardinal(FCount)));
{$ENDIF}
   if FItemSize=4 then begin
      p:=PIntegerArray(FBaseList);
      buf:=p[index1];
      p[index1]:=p[index2];
      p[index2]:=buf;
   end else begin
      System.Move(FBaseList[index1*FItemSize], BufferItem[0], FItemSize);
      System.Move(FBaseList[index2*FItemSize], FBaseList[index1*FItemSize], FItemSize);
      System.Move(BufferItem[0], FBaseList[index2*FItemSize], FItemSize);
   end;
end;

// Move
//
procedure TBaseList.Move(curIndex, newIndex : Integer);
begin
   if curIndex<>newIndex then begin
{$IFOPT R+}
      Assert(Cardinal(newIndex)<Cardinal(Count));
      Assert(Cardinal(curIndex)<Cardinal(Count));
{$ENDIF}
      if FItemSize=4 then
         PInteger(BufferItem)^:=PInteger(@FBaseList[curIndex*FItemSize])^
      else System.Move(FBaseList[curIndex*FItemSize], BufferItem[0], FItemSize);
      if curIndex<newIndex then begin
         // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
         System.Move(FBaseList[(curIndex+1)*FItemSize], FBaseList[curIndex*FItemSize],
                     (newIndex-curIndex-1)*FItemSize);
      end else begin
         // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
         System.Move(FBaseList[newIndex*FItemSize], FBaseList[(newIndex+1)*FItemSize],
                     (curIndex-newIndex-1)*FItemSize);
      end;
      if FItemSize=4 then
         PInteger(@FBaseList[newIndex*FItemSize])^:=PInteger(BufferItem)^
      else System.Move(BufferItem[0], FBaseList[newIndex*FItemSize], FItemSize);
   end;
end;

// Reverse
//
procedure TBaseList.Reverse;
var
   s, e : Integer;
begin
   s:=0;
   e:=Count-1;
   while s<e do begin
      Exchange(s, e);
      Inc(s);
      Dec(e);
   end;
end;

// ------------------
// ------------------ TBaseVectorList ------------------
// ------------------

// WriteToFiler
//
procedure TBaseVectorList.WriteToFiler(writer : TVirtualWriter);
begin
   inherited;
   with writer do begin
      WriteInteger(0);  // Archive Version 0
      // nothing
   end;
end;

// ReadFromFiler
//
procedure TBaseVectorList.ReadFromFiler(reader : TVirtualReader);
var
   archiveVersion : Integer;
begin
   inherited;
   archiveVersion:=reader.ReadInteger;
   if archiveVersion=0 then with reader do begin
      // nothing
   end else RaiseFilerException(archiveVersion);
end;

// GetExtents
//
procedure TBaseVectorList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
   ref : PFloatArray;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      ref:=ItemAddress[i];
      for k:=0 to 2 do begin
         f:=ref[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// Sum
//
function TBaseVectorList.Sum : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   for i:=0 to Count-1 do
      AddVector(Result, PAffineVector(ItemAddress[i])^);
end;

// Normalize
//
procedure TBaseVectorList.Normalize;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      NormalizeVector(PAffineVector(ItemAddress[i])^);
end;

// MaxSpacing
//
function TBaseVectorList.MaxSpacing(list2 : TBaseVectorList) : Single;
var
   i : Integer;
   s : Single;
begin
   Assert(list2.Count=Count);
   Result:=0;
   for i:=0 to Count-1 do begin
      s:=VectorSpacing(PAffineVector(ItemAddress[i])^,
                       PAffineVector(list2.ItemAddress[i])^);
      if s>Result then Result:=s;
   end;
end;

// Translate (delta)
//
procedure TBaseVectorList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(PAffineVector(ItemAddress[i])^, delta);
end;

// Translate (TBaseVectorList)
//
procedure TBaseVectorList.Translate(const delta : TBaseVectorList);
var
   i : Integer;
begin
   Assert(Count<=delta.Count);
   for i:=0 to Count-1 do
      AddVector(PAffineVector(ItemAddress[i])^, PAffineVector(delta.ItemAddress[i])^);
end;

// TranslateInv (TBaseVectorList)
//
procedure TBaseVectorList.TranslateInv(const delta : TBaseVectorList);
var
   i : Integer;
begin
   Assert(Count<=delta.Count);
   for i:=0 to Count-1 do
      SubtractVector(PAffineVector(ItemAddress[i])^, PAffineVector(delta.ItemAddress[i])^);
end;

// AngleLerp
//
procedure TBaseVectorList.AngleLerp(const list1, list2 : TBaseVectorList; lerpFactor : Single);
var
   i : Integer;
begin
   Assert(list1.Count=list2.Count);
   if list1<>list2 then begin
      if lerpFactor=0 then
         Assign(list1)
      else if lerpFactor=1 then
         Assign(list2)
      else begin
         Capacity:=list1.Count;
         FCount:=list1.Count;
         for i:=0 to list1.Count-1 do
            PAffineVector(ItemAddress[i])^:=VectorAngleLerp(PAffineVector(list1.ItemAddress[i])^,
                                                            PAffineVector(list2.ItemAddress[i])^,
                                                            lerpFactor);
      end;
   end else Assign(list1);
end;

// AngleCombine
//
procedure TBaseVectorList.AngleCombine(const list1 : TBaseVectorList; intensity : Single);
var
   i : Integer;
begin
   Assert(list1.Count=Count);
   for i:=0 to Count-1 do
      PAffineVector(ItemAddress[i])^:=VectorAngleCombine(PAffineVector(ItemAddress[i])^,
                                                         PAffineVector(list1.ItemAddress[i])^,
                                                         intensity);
end;

// Combine
//
procedure TBaseVectorList.Combine(const list2 : TBaseVectorList; factor : Single);
var
   i : Integer;
begin
   Assert(list2.Count>=Count);
   for i:=0 to Count-1 do
      CombineVector(PAffineVector(ItemAddress[i])^,
                    PAffineVector(list2.ItemAddress[i])^,
                    factor);
end;

// GetItemAddress
//
function TBaseVectorList.GetItemAddress(index : Integer) : PFloatArray;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
   Result:=PFloatArray(@FBaseList[index*FItemSize]);
end;

// ------------------
// ------------------ TAffineVectorList ------------------
// ------------------

// Create
//
constructor TAffineVectorList.Create;
begin
   FItemSize:=SizeOf(TAffineVector);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TAffineVectorList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TAffineVectorList) then
			System.Move(TAffineVectorList(Src).FList^, FList^, FCount*SizeOf(TAffineVector));
	end else Clear;
end;

// Add (affine)
//
function TAffineVectorList.Add(const item : TAffineVector): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList[Result] := Item;
  	Inc(FCount);
end;

// Add (hmg)
//
function TAffineVectorList.Add(const item : TVector) : Integer;
begin
   Result:=Add(PAffineVector(@item)^);
end;

// Add (2 affine)
//
procedure TAffineVectorList.Add(const i1, i2 : TAffineVector);
begin
  	Inc(FCount, 2);
   while FCount>FCapacity do SetCapacity(FCapacity+FGrowthDelta);
	FList[FCount-2] := i1;
	FList[FCount-1] := i2;
end;

// Add (3 affine)
//
procedure TAffineVectorList.Add(const i1, i2, i3 : TAffineVector);
begin
  	Inc(FCount, 3);
   while FCount>FCapacity do SetCapacity(FCapacity+FGrowthDelta);
	FList[FCount-3] := i1;
	FList[FCount-2] := i2;
	FList[FCount-1] := i3;
end;

// Add (vector2f)
//
function TAffineVectorList.Add(const item : TVector2f) : Integer;
begin
   Result:=Add(AffineVectorMake(item[0], item[1], 0))
end;

// Add (texpoint)
//
function TAffineVectorList.Add(const item : TTexPoint) : Integer;
begin
   Result:=Add(AffineVectorMake(item.S, item.T, 0));
end;

// Add
//
function TAffineVectorList.Add(const x, y : Single) : Integer;
var
   v : PAffineVector;
begin
   Result:=FCount;
  	Inc(FCount);
   while FCount>FCapacity do SetCapacity(FCapacity+FGrowthDelta);
   v:=@List[Result];
   v[0]:=x;
	v[1]:=y;
	v[2]:=0;
end;

// Add
//
function TAffineVectorList.Add(const x, y, z : Single) : Integer;
var
   v : PAffineVector;
begin
   Result:=FCount;
  	Inc(FCount);
   while FCount>FCapacity do SetCapacity(FCapacity+FGrowthDelta);
   v:=@List[Result];
   v[0]:=x;
	v[1]:=y;
	v[2]:=z;
end;

// Add (3 ints)
//
function TAffineVectorList.Add(const x, y, z : Integer) : Integer;
var
   v : PAffineVector;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
   v:=@List[Result];
   v[0]:=x;
	v[1]:=y;
	v[2]:=z;
  	Inc(FCount);
end;

// Add (3 ints, no capacity check)
//
function TAffineVectorList.AddNC(const x, y, z : Integer) : Integer;
var
   v : PAffineVector;
begin
	Result:=FCount;
   v:=@List[Result];
   v[0]:=x;
	v[1]:=y;
	v[2]:=z;
  	Inc(FCount);
end;

// Add (2 ints in array + 1)
//
function TAffineVectorList.Add(const xy : PIntegerArray; const z : Integer) : Integer;
var
   v : PAffineVector;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
   v:=@List[Result];
   v[0]:=xy[0];
	v[1]:=xy[1];
	v[2]:=z;
  	Inc(FCount);
end;

// AddNC (2 ints in array + 1, no capacity check)
//
function TAffineVectorList.AddNC(const xy : PIntegerArray; const z : Integer) : Integer;
var
   v : PAffineVector;
begin
	Result:=FCount;
   v:=@List[Result];
   v[0]:=xy[0];
	v[1]:=xy[1];
	v[2]:=z;
  	Inc(FCount);
end;

// Add
//
procedure TAffineVectorList.Add(const list : TAffineVectorList);
begin
   if Assigned(list) and (list.Count>0) then begin
      if Count+list.Count>Capacity then
         Capacity:=Count+list.Count;
      System.Move(list.FList[0], FList[Count], list.Count*SizeOf(TAffineVector));
      Inc(FCount, list.Count);
   end;
end;

// Get
//
function TAffineVectorList.Get(Index: Integer): TAffineVector;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TAffineVectorList.Insert(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount - Index) * SizeOf(TAffineVector));
	FList[Index] := Item;
	Inc(FCount);
end;

// IndexOf
//
function TAffineVectorList.IndexOf(const item : TAffineVector) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Count-1 do if VectorEquals(item, FList[i]) then begin
      Result:=i;
      Break;
   end;
end;

// FindOrAdd
//
function TAffineVectorList.FindOrAdd(const item : TAffineVector) : Integer;
begin
   Result:=IndexOf(item);
   if Result<0 then Result:=Add(item);
end;

// Put
//
procedure TAffineVectorList.Put(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TAffineVectorList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PAffineVectorArray(FBaseList);
end;

// Push
//
procedure TAffineVectorList.Push(const val : TAffineVector);
begin
	Add(val);
end;

// Pop
//
function TAffineVectorList.Pop : TAffineVector;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullVector;
end;

// Translate (delta)
//
procedure TAffineVectorList.Translate(const delta : TAffineVector);
begin
   VectorArrayAdd(FList, delta, Count, FList);
end;

// Translate (delta, range)
//
procedure TAffineVectorList.Translate(const delta : TAffineVector; base, nb : Integer);
begin
   VectorArrayAdd(@FList[base], delta, nb, @FList[base]);
end;

// TranslateItem
//
procedure TAffineVectorList.TranslateItem(index : Integer; const delta : TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
   AddVector(FList[Index], delta);
end;

// TranslateItems
//
procedure TAffineVectorList.TranslateItems(index : Integer; const delta : TAffineVector;
                                           nb : Integer);
begin
   nb:=index+nb;
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
   if nb>FCount then
      nb:=FCount;
{$ENDIF}
   VectorArrayAdd(@FList[index], delta, nb-index, @FList[index]);
end;

// CombineItem
//
procedure TAffineVectorList.CombineItem(index : Integer; const vector : TAffineVector; const f : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
   CombineVector(FList[Index], vector, @f);
end;

// TransformAsPoints
//
procedure TAffineVectorList.TransformAsPoints(const matrix : TMatrix);
var
    i : Integer;
begin
   for i:=0 to FCount-1 do
      FList[i]:=VectorTransform(FList[i], matrix);
end;

// TransformAsVectors (hmg)
//
procedure TAffineVectorList.TransformAsVectors(const matrix : TMatrix);
var
   m : TAffineMatrix;
begin
   if FCount>0 then begin
      SetMatrix(m, matrix);
      TransformAsVectors(m);
   end;
end;

// TransformAsVectors (affine)
//
procedure TAffineVectorList.TransformAsVectors(const matrix : TAffineMatrix);
var
    i : Integer;
begin
   for i:=0 to FCount-1 do
      FList[i]:=VectorTransform(FList[i], matrix);
end;

// Normalize
//
procedure TAffineVectorList.Normalize;
begin
   NormalizeVectorArray(List, Count);
end;

// Lerp
//
procedure TAffineVectorList.Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single);
begin
   if (list1 is TAffineVectorList) and (list2 is TAffineVectorList) then begin
      Assert(list1.Count=list2.Count);
      Capacity:=list1.Count;
      FCount:=list1.Count;
      VectorArrayLerp(TAffineVectorList(list1).List, TAffineVectorList(list2).List,
                      lerpFactor, FCount, List);
   end;
end;

// Scale (scalar)
//
procedure TAffineVectorList.Scale(factor : Single);
begin
   if (Count>0) and (factor<>1) then
      ScaleFloatArray(@FList[0][0], Count*3, factor);
end;

// Scale (affine)
//
procedure TAffineVectorList.Scale(const factors : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      ScaleVector(FList[i], factors);
end;

// ------------------
// ------------------ TVectorList ------------------
// ------------------

// Create
//
constructor TVectorList.Create;
begin
   FItemSize:=SizeOf(TVector);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TVectorList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TVectorList) then
			System.Move(TVectorList(Src).FList^, FList^, FCount*SizeOf(TVector));
	end else Clear;
end;

// Add
//
function TVectorList.Add(const item : TVector): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList[Result] := Item;
  	Inc(FCount);
end;

// Add
//
function TVectorList.Add(const item : TAffineVector; w : Single): Integer;
begin
   Result:=Add(VectorMake(item, w));
end;

// Add
//
function TVectorList.Add(const x, y, z, w : Single): Integer;
begin
   Result:=Add(VectorMake(x, y, z, w));
end;

// Add (3 affine)
//
procedure TVectorList.Add(const i1, i2, i3 : TAffineVector; w : Single);
begin
  	Inc(FCount, 3);
   while FCount>FCapacity do SetCapacity(FCapacity + FGrowthDelta);
	PAffineVector(@FList[FCount-3])^:=i1;   FList[FCount-3][3]:=w;
	PAffineVector(@FList[FCount-2])^:=i2;   FList[FCount-2][3]:=w;
	PAffineVector(@FList[FCount-1])^:=i3;   FList[FCount-1][3]:=w;
end;

// AddVector
//
function TVectorList.AddVector(const item : TAffineVector) : Integer;
begin
   Result:=Add(VectorMake(item));
end;

// AddPoint
//
function TVectorList.AddPoint(const item : TAffineVector) : Integer;
begin
   Result:=Add(PointMake(item));
end;

// AddPoint
//
function TVectorList.AddPoint(const x, y : Single; const z : Single = 0) : Integer;
begin
   Result:=Add(PointMake(x, y, z));
end;

// Get
//
function TVectorList.Get(Index: Integer): TVector;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TVectorList.Insert(Index: Integer; const Item: TVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount - Index) * SizeOf(TVector));
	FList[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TVectorList.Put(Index: Integer; const Item: TVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TVectorList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PVectorArray(FBaseList);
end;

// Push
//
procedure TVectorList.Push(const val : TVector);
begin
	Add(val);
end;

// Pop
//
function TVectorList.Pop : TVector;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullHmgVector;
end;

// IndexOf
//
function TVectorList.IndexOf(const item : TVector) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Count-1 do if VectorEquals(item, FList[i]) then begin
      Result:=i;
      Break;
   end;
end;

// FindOrAdd
//
function TVectorList.FindOrAdd(const item : TVector) : Integer;
begin
   Result:=IndexOf(item);
   if Result<0 then Result:=Add(item);
end;

// FindOrAddPoint
//
function TVectorList.FindOrAddPoint(const item : TAffineVector) : Integer;
var
   ptItem : TVector;
begin
   MakePoint(ptItem, item);
   Result:=IndexOf(ptItem);
   if Result<0 then Result:=Add(ptItem);
end;

// Lerp
//
procedure TVectorList.Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single);
begin
   if (list1 is TVectorList) and (list2 is TVectorList) then begin
      Assert(list1.Count=list2.Count);
      Capacity:=list1.Count;
      FCount:=list1.Count;
      VectorArrayLerp(TVectorList(list1).List, TVectorList(list2).List,
                      lerpFactor, FCount, List);
   end;
end;

// ------------------
// ------------------ TTexPointList ------------------
// ------------------

// Create
//
constructor TTexPointList.Create;
begin
   FItemSize:=SizeOf(TTexPoint);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TTexPointList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TTexPointList) then
			System.Move(TTexPointList(Src).FList^, FList^, FCount*SizeOf(TTexPoint));
	end else Clear;
end;

// Add
//
function TTexPointList.Add(const item : TTexPoint): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList[Result] := Item;
  	Inc(FCount);
end;

// Add
//
function TTexPointList.Add(const item : TVector2f): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=PTexPoint(@Item)^;
  	Inc(FCount);
end;

// Add
//
function TTexPointList.Add(const texS, texT : Single) : Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
   with FList[Result] do begin
      s:=texS;
      t:=texT;
   end;
  	Inc(FCount);
end;

// Add
//
function TTexPointList.Add(const texS, texT : Integer) : Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
   with FList[Result] do begin
      s:=texS;
      t:=texT;
   end;
  	Inc(FCount);
end;

// AddNC
//
function TTexPointList.AddNC(const texS, texT : Integer) : Integer;
begin
	Result:=FCount;
   with FList[Result] do begin
      s:=texS;
      t:=texT;
   end;
 	Inc(FCount);
end;

// Add
//
function TTexPointList.Add(const texST : PIntegerArray) : Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
   with FList[Result] do begin
      s:=texST[0];
      t:=texST[1];
   end;
 	Inc(FCount);
end;

// AddNC
//
function TTexPointList.AddNC(const texST : PIntegerArray) : Integer;
begin
	Result:=FCount;
   with FList[Result] do begin
      s:=texST[0];
      t:=texST[1];
   end;
 	Inc(FCount);
end;

// Get
//
function TTexPointList.Get(Index: Integer): TTexPoint;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TTexPointList.Insert(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount - Index) * SizeOf(TTexPoint));
	FList[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TTexPointList.Put(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TTexPointList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PTexPointArray(FBaseList);
end;

// Push
//
procedure TTexPointList.Push(const val : TTexPoint);
begin
	Add(val);
end;

// Pop
//
function TTexPointList.Pop : TTexPoint;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullTexPoint;
end;

// Translate
//
procedure TTexPointList.Translate(const delta : TTexPoint);
begin
   TexPointArrayAdd(List, delta, FCount, FList);
end;

// ScaleAndTranslate
//
procedure TTexPointList.ScaleAndTranslate(const scale, delta : TTexPoint);
begin
   TexPointArrayScaleAndAdd(FList, delta, FCount, scale, FList);
end;

// ScaleAndTranslate
//
procedure TTexPointList.ScaleAndTranslate(const scale, delta : TTexPoint;
                                          base, nb : Integer);
var
   p : PTexPointArray;
begin
   p:=@FList[base];
   TexPointArrayScaleAndAdd(p, delta, nb, scale, p);
end;

// ------------------
// ------------------ TIntegerList ------------------
// ------------------

// Create
//
constructor TIntegerList.Create;
begin
   FItemSize:=SizeOf(Integer);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TIntegerList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TIntegerList) then
			System.Move(TIntegerList(Src).FList^, FList^, FCount*SizeOf(Integer));
	end else Clear;
end;

// Add (simple)
//
function TIntegerList.Add(const item : Integer) : Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
  	Inc(FCount);
end;

// AddNC (simple, no capacity check)
//
function TIntegerList.AddNC(const item : Integer) : Integer;
begin
	Result:=FCount;
	FList[Result]:=Item;
  	Inc(FCount);
end;

// Add (two at once)
//
procedure TIntegerList.Add(const i1, i2 : Integer);
var
   list : PIntegerArray;
begin
  	Inc(FCount, 2);
   while FCount>FCapacity do SetCapacity(FCapacity + FGrowthDelta);
   list:=@FList[FCount-2];
	list[0]:=i1;
	list[1]:=i2;
end;

// Add (three at once)
//
procedure TIntegerList.Add(const i1, i2, i3 : Integer);
var
   list : PIntegerArray;
begin
  	Inc(FCount, 3);
   while FCount>FCapacity do SetCapacity(FCapacity + FGrowthDelta);
   list:=@FList[FCount-3];
	list[0]:=i1;
	list[1]:=i2;
	list[2]:=i3;
end;

// Add (list)
//
procedure TIntegerList.Add(const list : TIntegerList);
begin
   if Assigned(list) and (list.Count>0) then begin
      if Count+list.Count>Capacity then
         Capacity:=Count+list.Count;
      System.Move(list.FList[0], FList[Count], list.Count*SizeOf(Integer));
      Inc(FCount, list.Count);
   end;
end;

// Get
//
function TIntegerList.Get(Index: Integer): Integer;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TIntegerList.Insert(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then
      SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[Index], FList[Index+1],	(FCount-Index)*SizeOf(Integer));
	FList[Index]:=Item;
	Inc(FCount);
end;

// Remove
//
procedure TIntegerList.Remove(const item : Integer);
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      if FList[i]=item then begin
         System.Move(FList[i+1], FList[i], (FCount-1-i)*SizeOf(Integer));
         Dec(FCount);
         Break;
      end;
   end;
end;

// Put
//
procedure TIntegerList.Put(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList[Index]:=Item;
end;

// SetCapacity
//
procedure TIntegerList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PIntegerArray(FBaseList);
end;

// Push
//
procedure TIntegerList.Push(const val : Integer);
begin
	Add(val);
end;

// Pop
//
function TIntegerList.Pop : Integer;
begin
	if FCount>0 then begin
   	Result:=FList[FCount-1];
		Delete(FCount-1);
	end else Result:=0;
end;

// AddSerie
//
procedure TIntegerList.AddSerie(aBase, aDelta, aCount : Integer);
var
   list : PInteger;
   i : Integer;
begin
   if aCount<=0 then Exit;
   AdjustCapacityToAtLeast(Count+aCount);
   list:=@FList[Count];
   for i:=Count to Count+aCount-1 do begin
      list^:=aBase;
      Inc(list);
      aBase:=aBase+aDelta;
   end;
   FCount:=Count+aCount;
end;

// AddIntegers (pointer & n)
//
procedure TIntegerList.AddIntegers(const first : PInteger; n : Integer);
begin
   if n<1 then Exit;
   AdjustCapacityToAtLeast(Count+n);
   System.Move(first^, FList[FCount], n*SizeOf(Integer));
   FCount:=FCount+n;
end;

// AddIntegers (TIntegerList)
//
procedure TIntegerList.AddIntegers(const aList : TIntegerList);
begin
   if not Assigned(aList) then Exit;
   AddIntegers(@aList.List[0], aList.Count)
end;

// AddIntegers (array)
//
procedure TIntegerList.AddIntegers(const anArray : array of Integer);
var
   n : Integer;
begin
   n:=Length(anArray);
   if n>0 then
      AddIntegers(@anArray[0], n);
end;

// IntegerSearch
//
function IntegerSearch(item : Integer; list : PIntegerVector; count : Integer) : Integer; register;
asm
      push edi;

      test ecx, ecx
      jz @@NotFound

      mov edi, edx;
      mov edx, ecx;
      repne scasd;
      je @@FoundIt

@@NotFound:
      xor eax, eax
      dec eax
      jmp @@End;

@@FoundIt:
      sub edx, ecx;
      dec edx;
      mov eax, edx;

@@End:
      pop edi;
end;

// IndexOf
//
function TIntegerList.IndexOf(item : Integer) : Integer; register;
begin
   Result:=IntegerSearch(item, FList, FCount);
end;

// MinInteger
//
function TIntegerList.MinInteger : Integer;
var
   i : Integer;
   locList : PIntegerVector;
begin
   if FCount>0 then begin
      locList:=FList;
      Result:=locList[0];
      for i:=1 to FCount-1 do
         if locList[i]<Result then Result:=locList[i];
   end else Result:=0;
end;

// MaxInteger
//
function TIntegerList.MaxInteger : Integer;
var
   i : Integer;
   locList : PIntegerVector;
begin
   if FCount>0 then begin
      locList:=FList;
      Result:=locList[0];
      for i:=1 to FCount-1 do
         if locList[i]>Result then Result:=locList[i];
   end else Result:=0;
end;

// IntegerQuickSort
//
procedure IntegerQuickSort(sortList : PIntegerArray; left, right : Integer);
var
	i, j : Integer;
	p, t : Integer;
begin
	repeat
		i:=left;
      j:=right;
		p:=sortList[(left+right) shr 1];
		repeat
			while sortList[i]<p do Inc(i);
			while sortList[j]>p do Dec(j);
			if i<=j then begin
				t:=sortList[i];
				sortList[i]:=sortList[j];
				sortList[j]:=t;
				Inc(i); Dec(j);
			end;
		until i>j;
		if left<j then
         IntegerQuickSort(sortList, left, j);
		left:=i;
	until i>=right;
end;

// Sort
//
procedure TIntegerList.Sort;
begin
	if (FList<>nil) and (Count>1) then
		IntegerQuickSort(FList, 0, Count-1);
end;

// SortAndRemoveDuplicates
//
procedure TIntegerList.SortAndRemoveDuplicates;
var
	i, j, lastVal : Integer;
   localList : PIntegerArray;
begin
	if (FList<>nil) and (Count>1) then begin
		IntegerQuickSort(FList, 0, Count-1);
      j:=0;
      localList:=FList;
      lastVal:=localList^[j];
      for i:=1 to Count-1 do begin
         if localList^[i]<>lastVal then begin
            lastVal:=localList^[i];
            Inc(j);
            localList^[j]:=lastVal;
         end;
      end;
      FCount:=j+1;
   end;
end;

// BinarySearch
//
function TIntegerList.BinarySearch(const value : Integer) : Integer;
var
   found : Boolean;
begin
   Result:=BinarySearch(value, False, found);
end;

// BinarySearch
//
function TIntegerList.BinarySearch(const value : Integer; returnBestFit : Boolean;
                                   var found : Boolean) : Integer;
var
   index : Integer;
   min, max, mid : Integer;
   intList : PIntegerArray;
begin
   // Assume we won't find it
   found:=False;
   // If the list is empty, we won't find the sought value!
   if Count=0 then begin
      Result:=-1;
      Exit;
   end;

   min:=-1; // ONE OFF!
   max:=Count; // ONE OFF!

   // We now know that Min and Max AREN'T the values!
   index:=-1;
   intList:=List;
   repeat
      // Find the middle of the current scope
      mid:=(min+max) shr 1;
      // Reduce the search scope by half
      if intList[mid]<=value then begin
         // Is this the one?
         if intList[mid]=value then begin
            index:=mid;
            found:=True;
            Break;
         end else min:=mid;
      end else max:=mid;
   until min+1=max;

   if returnBestFit then begin
      if index>=0 then
         Result:=index
      else Result:=min;
   end else Result:=index;
end;

// AddSorted
//
function TIntegerList.AddSorted(const value : Integer; const ignoreDuplicates : Boolean = False) : Integer;
var
   index : Integer;
   found : Boolean;
begin
   index:=BinarySearch(value, True, found);
   if ignoreDuplicates and Found then
      Result:=-1
   else begin
      Insert(index+1, Value);
      Result:=index+1;
   end;
end;

// RemoveSorted
//
procedure TIntegerList.RemoveSorted(const value : Integer);
var
   index : Integer;
begin
   index:=BinarySearch(value);
   if index>=0 then
      Delete(index);
end;

// Offset (all)
//
procedure TIntegerList.Offset(delta : Integer);
var
   i : Integer;
   locList : PIntegerArray;
begin
   locList:=FList;
   for i:=0 to FCount-1 do
      locList[i]:=locList[i]+delta;
end;

// Offset (range)
//
procedure TIntegerList.Offset(delta : Integer; const base, nb : Integer);
var
   i : Integer;
   locList : PIntegerArray;
begin
   locList:=FList;
   for i:=base to base+nb-1 do
      locList[i]:=locList[i]+delta;
end;

// ------------------
// ------------------ TSingleList ------------------
// ------------------

// Create
//
constructor TSingleList.Create;
begin
   FItemSize:=SizeOf(Single);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TSingleList.Assign(Src : TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TSingleList) then
			System.Move(TSingleList(Src).FList^, FList^, FCount*SizeOf(Single));
	end else Clear;
end;

// Add
//
function TSingleList.Add(const item : Single): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then
      SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
  	Inc(FCount);
end;

// Get
//
function TSingleList.Get(Index : Integer) : Single;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TSingleList.Insert(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount-Index)*SizeOf(Single));
	FList[Index]:=Item;
	Inc(FCount);
end;

// Put
//
procedure TSingleList.Put(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TSingleList.SetCapacity(NewCapacity : Integer);
begin
   inherited;
   FList:=PSingleArrayList(FBaseList);
end;

// Push
//
procedure TSingleList.Push(const val : Single);
begin
	Add(val);
end;

// Pop
//
function TSingleList.Pop : Single;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=0;
end;

// AddSerie
//
procedure TSingleList.AddSerie(aBase, aDelta : Single; aCount : Integer);
var
   list : PSingle;
   i : Integer;
begin
   if aCount<=0 then Exit;
   AdjustCapacityToAtLeast(Count+aCount);
   list:=@FList[Count];
   for i:=Count to Count+aCount-1 do begin
      list^:=aBase;
      Inc(list);
      aBase:=aBase+aDelta;
   end;
   FCount:=Count+aCount;
end;

// Offset (single)
//
procedure TSingleList.Offset(delta : Single);
begin
   OffsetFloatArray(PFloatVector(FList), FCount, delta);
end;

// Offset (list)
//
procedure TSingleList.Offset(const delta : TSingleList);
begin
   if FCount=delta.FCount then
      OffsetFloatArray(PFloatVector(FList), PFloatVector(delta.FList), FCount)
   else raise Exception.Create('SingleList count do not match');
end;

// Scale
//
procedure TSingleList.Scale(factor : Single);
begin
   ScaleFloatArray(PFloatVector(FList), FCount, factor);
end;

// Sqr
//
procedure TSingleList.Sqr;
var
   i : Integer;
   locList : PSingleArrayList;
begin
   locList:=FList;
   for i:=0 to Count-1 do
      locList[i]:=locList[i]*locList[i];
end;

// Sqrt
//
procedure TSingleList.Sqrt;
var
   i : Integer;
   locList : PSingleArrayList;
begin
   locList:=FList;
   for i:=0 to Count-1 do
      locList[i]:=System.Sqrt(locList[i]);
end;

// Sum
//
function TSingleList.Sum : Single;

   function ComputeSum(list : PSingleArrayList; nb : Integer) : Single; register;
   asm
         fld   dword ptr [eax]
   @@Loop:
         dec   edx
         fadd  dword ptr [eax+edx*4]
         jnz   @@Loop
   end;

begin
   if FCount>0 then
      Result:=ComputeSum(FList, FCount)
   else Result:=0;
end;

// ------------------
// ------------------ TByteList ------------------
// ------------------

// Create
//
constructor TByteList.Create;
begin
   FItemSize:=SizeOf(Byte);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TByteList.Assign(Src : TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TByteList) then
			System.Move(TByteList(Src).FList^, FList^, FCount*SizeOf(Byte));
	end else Clear;
end;

// Add
//
function TByteList.Add(const item : Byte): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then
      SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
  	Inc(FCount);
end;

// Get
//
function TByteList.Get(Index : Integer) : Byte;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result:=FList[Index];
end;

// Insert
//
procedure TByteList.Insert(Index : Integer; const Item : Byte);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[Index], FList[Index+1],
						(FCount-Index)*SizeOf(Byte));
	FList[Index]:=Item;
	Inc(FCount);
end;

// Put
//
procedure TByteList.Put(Index : Integer; const Item : Byte);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList[Index]:=Item;
end;

// SetCapacity
//
procedure TByteList.SetCapacity(NewCapacity : Integer);
begin
   inherited;
   FList:=PByteArray(FBaseList);
end;

// ------------------
// ------------------ TQuaternionList ------------------
// ------------------

// Create
//
constructor TQuaternionList.Create;
begin
   FItemSize:=SizeOf(TQuaternion);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TQuaternionList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TQuaternionList) then
			System.Move(TQuaternionList(Src).FList^, FList^, FCount*SizeOf(TQuaternion));
	end else Clear;
end;

// Add
//
function TQuaternionList.Add(const item : TQuaternion): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList[Result] := Item;
  	Inc(FCount);
end;

// Add
//
function TQuaternionList.Add(const item : TAffineVector; w : Single): Integer;
begin
   Result:=Add(QuaternionMake(item, w));
end;

// Add
//
function TQuaternionList.Add(const x, y, z, w : Single): Integer;
begin
   Result:=Add(QuaternionMake([x, y, z], w));
end;

// Get
//
function TQuaternionList.Get(Index: Integer): TQuaternion;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList[Index];
end;

// Insert
//
procedure TQuaternionList.Insert(Index: Integer; const Item: TQuaternion);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList[Index], FList[Index + 1],
						(FCount - Index) * SizeOf(TQuaternion));
	FList[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TQuaternionList.Put(Index: Integer; const Item: TQuaternion);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList[Index] := Item;
end;

// SetCapacity
//
procedure TQuaternionList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PQuaternionArray(FBaseList);
end;

// Push
//
procedure TQuaternionList.Push(const val : TQuaternion);
begin
	Add(val);
end;

// Pop
//
function TQuaternionList.Pop : TQuaternion;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=IdentityQuaternion;
end;

// IndexOf
//
function TQuaternionList.IndexOf(const item : TQuaternion) : Integer;
var
   i : Integer;
   curItem : PQuaternion;
begin
   for i:=0 to Count-1 do begin
      curItem:=@FList[i];
      if     (item.RealPart=curItem.RealPart)
         and VectorEquals(item.ImagPart, curItem.ImagPart) then begin
         Result:=i;
         Exit;
      end;
   end;
   Result:=-1;
end;

// FindOrAdd
//
function TQuaternionList.FindOrAdd(const item : TQuaternion) : Integer;
begin
   Result:=IndexOf(item);
   if Result<0 then Result:=Add(item);
end;

// Lerp
//
procedure TQuaternionList.Lerp(const list1, list2 : TBaseVectorList; lerpFactor : Single);
var
   i : integer;
begin
   if (list1 is TQuaternionList) and (list2 is TQuaternionList) then begin
      Assert(list1.Count=list2.Count);
      Capacity:=list1.Count;
      FCount:=list1.Count;
      for i:=0 to FCount-1 do
         Put(i,QuaternionSlerp(TQuaternionList(list1)[i],TQuaternionList(list2)[i],lerpFactor));
   end;
end;

// Combine
//
procedure TQuaternionList.Combine(const list2 : TBaseVectorList; factor : Single);

   procedure CombineQuaternion(var q1 : TQuaternion; const q2 : TQuaternion; factor : Single);
   begin
      q1:=QuaternionMultiply(q1,QuaternionSlerp(IdentityQuaternion, q2, factor));
   end;

var
   i : Integer;
begin
   Assert(list2.Count>=Count);
   if list2 is TQuaternionList then begin
      for i:=0 to Count-1 do begin
         CombineQuaternion(PQuaternion(ItemAddress[i])^,
                           PQuaternion(list2.ItemAddress[i])^,
                           factor);
      end;
   end else inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterClasses([TAffineVectorList, TVectorList, TTexPointList, TSingleList]);

end.

