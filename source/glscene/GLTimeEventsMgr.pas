// GLTimeEventsMgr
//
// by GliGli
//
// Time based events mannager using the Cadencer
// can be useful to make animations with GlScene 
//
// 07/02/02 - EG - Added Notification, DoEvent, ElapsedTime and changed Event type
//
unit GLTimeEventsMgr;

interface

uses
  GLCadencer, SysUtils, Classes, GLMisc;

type

    TTimeEvent = class;
    TTimeEvents = class;

	// TGLTimeEventsMGR
	//
   TGLTimeEventsMGR = class(TGLUpdateAbleComponent)
   private
      { Déclarations privées }
      FCadencer : TGLCadencer;
      FEnabled : boolean;
      FFreeEventOnEnd : boolean;
      FEvents : TTimeEvents;

   protected
      { Déclarations protégées }
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      procedure SetCadencer(const val : TGLCadencer);
      procedure SetEvents(const val : TTimeEvents);

   public
      { Déclarations publiques }
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure DoProgress(const progressTime : TProgressTimes); override;

   published
      { Déclarations publiées }
      property Cadencer : TGLCadencer read FCadencer write SetCadencer;
      property Enabled : boolean read FEnabled write FEnabled default True;
      property FreeEventOnEnd : boolean read FFreeEventOnEnd write FFreeEventOnEnd default False;
      property Events : TTimeEvents read FEvents write SetEvents;
   end;

	// TTimeEvents
	//
	TTimeEvents = class (TCollection)
	   protected
	      { Protected Declarations }
	      Owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TTimeEvent);
	      function GetItems(index : Integer) : TTimeEvent;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

         function Add: TTimeEvent;
	      function FindItemID(ID: Integer): TTimeEvent;
         function EventByName(name:ShortString): TTimeEvent;

	      property Items[index : Integer] : TTimeEvent read GetItems write SetItems; default;
   end;

   TTimeEventType = (etOneShot, etContinuous, etPeriodic);
   TTimeEventProc = procedure (event : TTimeEvent) of object;

   // TTimeEvent
   //
   TTimeEvent = class (TCollectionItem)
      private
         { Private Declarations }
         FName: ShortString;
         FStartTime, FEndTime, FElapsedTime : Double;
         FPeriod : Double;
         FEventType: TTimeEventType;
         FOnEvent:TTimeEventProc;
         FEnabled: boolean;

         FTickCount : Cardinal;

      protected
         { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetName(val : ShortString);

         procedure DoEvent(const curTime : Double);

      public
         { Public Declarations }
         constructor Create(Collection : TCollection); override;
         destructor Destroy; override;

         //: Number of times the event was triggered since activation
         property TickCount : Cardinal read FTickCount;
         //: Elapsed time since the event was activated
         property ElapsedTime : Double read FElapsedTime; 

      published
         { Published Declarations }
         property Name : ShortString read FName write SetName;
         property StartTime : Double read FStartTime write FStartTime;
         property EndTime : Double read FEndTime write FEndTime;
         property Period : Double read  FPeriod write FPeriod;
         property EventType : TTimeEventType read FEventType write FEventType default etOneShot;
         property OnEvent : TTimeEventProc read FOnEvent write FOnEvent;
         property Enabled : boolean read FEnabled write FEnabled  default True;

    end;

procedure Register;

implementation

// ------------------
// ------------------ TGLTimeEventsMGR ------------------
// ------------------

// Create
//
constructor TGLTimeEventsMGR.Create(aOwner : TComponent);
begin
    inherited;
    FEnabled:=True;
    FFreeEventOnEnd:=False;
    FEvents:=TTimeEvents.Create(self);
end;

// Destroy
//
destructor TGLTimeEventsMGR.Destroy;
begin
    Cadencer:=nil;
    FEvents.Free;
    inherited Destroy;
end;

// Notification
//
procedure TGLTimeEventsMGR.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=Cadencer) then
      FCadencer:=nil;
   inherited;
end;

// SetCadencer
//
procedure TGLTimeEventsMGR.SetCadencer(const val : TGLCadencer);
begin
   if FCadencer<>val then begin
      if Assigned(FCadencer) then
         FCadencer.UnSubscribe(Self);
      FCadencer:=val;
      if Assigned(FCadencer) then
         FCadencer.Subscribe(Self);
   end;
end;


// SetEvents
//
procedure TGLTimeEventsMGR.SetEvents(const val : TTimeEvents);
begin
   FEvents.Assign(val);
end;

// DoProgress
//
procedure TGLTimeEventsMGR.DoProgress(const progressTime : TProgressTimes);
var
   i : Integer;
begin
   if not Enabled then Exit;

   i:=0;
   with progressTime do while i<=Events.Count-1 do with Events.Items[i] do begin
      if Enabled and Assigned(FOnEvent) then begin
         case EventType of
            etOneShot :
               if (newTime>=StartTime) and (TickCount=0) then
                  DoEvent(newTime);
            etContinuous :
               if (newTime>=StartTime) and ((newTime<=EndTime) or (EndTime<=0)) then
                  DoEvent(newTime);
            etPeriodic :
               if (newTime>=StartTime+TickCount*Period) and ((newTime<=EndTime) or (EndTime<=0)) then
                  DoEvent(newTime);
         else
            Assert(False);
         end;
      end;
      if FreeEventOnEnd and
           ( ((EventType<>etOneShot) and (newTime>EndTime) and (EndTime>=0)) or
             ((EventType=etOneShot) and (TickCount>0)) ) then
         Events[i].Free
      else begin
         // if we delete current event, the next will have same index
         // so increment only if we don't delete
         Inc(i);
      end;
   end;
end;



// ------------------
// ------------------ TTimeEvents ------------------
// ------------------

// Create
//
constructor TTimeEvents.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TTimeEvent);
end;

// GetOwner
//
function TTimeEvents.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

// Setitems
//
procedure TTimeEvents.SetItems(index : Integer; const val : TTimeEvent);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TTimeEvents.GetItems(index : Integer) : TTimeEvent;
begin
	Result:=TTimeEvent(inherited Items[index]);
end;

// Add
//
function TTimeEvents.Add : TTimeEvent;
begin
	Result:=(inherited Add) as TTimeEvent;
end;

// FindItemID
//
function TTimeEvents.FindItemID(ID: Integer): TTimeEvent;
begin
	Result:=(inherited FindItemID(ID)) as TTimeEvent;
end;

// EventByName
//
function TTimeEvents.EventByName(name:ShortString): TTimeEvent;
var i:integer;
begin
    i:=0;
    while (i<Count) and (Items[i].FName<>name) do inc(i);

    if i=Count then result:=nil else result:=Items[i];
end;




// ------------------
// ------------------ TTimeEvent ------------------
// ------------------

// Create
//
constructor TTimeEvent.Create(Collection : TCollection);
begin
   inherited Create(Collection);
   FEventType:=etOneShot;
   FName:=Format('Event%d', [index]); // give a default name different for each event
   FEnabled:=True;
end;

// Destroy
//
destructor TTimeEvent.Destroy;
begin
    inherited Destroy;
end;

// GetDisplayName
//
function TTimeEvent.GetDisplayName : String;
begin
    case EventType of
        etOneShot:
            Result:=Name+Format(' (OneShot ST=%g)',[StartTime]);
        etContinuous:
            Result:=Name+Format(' (Continuous ST=%g ET=%g)',[StartTime,EndTime]);
        etPeriodic:
            Result:=Name+Format(' (Periodic ST=%g ET=%g P=%g)',[StartTime,EndTime,Period]);
    end;
end;

// SetName
//
procedure TTimeEvent.SetName(val : ShortString);
var
   i : Integer;
   ok : Boolean;
begin
   ok := True;
   with self.Collection as TTimeEvents do // we mustn't have 2 events with the same name (for EventByName)
       for i:=0 to Count-1 do
           if Items[i].FName = val then Ok := False;

   if Ok and (val<>'') then FName:=val;
end;

// DoEvent
//
procedure TTimeEvent.DoEvent(const curTime : Double);
begin
   if Assigned(FOnEvent) then begin
      FElapsedTime:=curTime-StartTime;
      FOnEvent(Self);
   end;
   Inc(FTickCount);
end;

//***********************************************************************************
//***********************************************************************************
//***********************************************************************************
//***********************************************************************************
//***********************************************************************************
Procedure Register;   // a mettre dans GLSceneRegister
begin
  RegisterComponents('GLScene Utils', [TGLTimeEventsMGR]);
end;

end.
