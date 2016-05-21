  // In GL windows management
{: In GL windows management classes and structures<p>

	<b>History : </b><font size=-1><ul>
      <li>10/11/05 - Mathx - Fixed TGLPopupMenu stack overflow on method internalRender.
                             Related to bug 1193909.
      <li>24/05/02 - JAJ - Base Unit built on basis of Jan Horn's demo at http://www.sulaco.co.za (http://www.sulaco.co.za/opengl/windows.zip)
      <li>01/06/02 - JAJ - After not having received Jan Horn's blessing, the system have been revised all parts have been rewritten.
      <li>01/01/03 - JAJ - Updated so that focused controls pass focus on hide...
      <li>05/01/03 - JAJ - Cleaned up the DesignTime AccessViolations...
      <li>07/01/03 - JAJ - Jeremy Darling modified the TGLEdit's Render, more updates on TGLEdit expected...
      <li>18/01/03 - JAJ - Added TGLStringList, TGLScrollbar, TGLPopupMenu...
      <li>08/08/03 - PS  - Added Horizontal to GLScrollbar...
      <li>14/08/03 - SG - Fixed TGLBaseComponent.SetGuiLayout (Joen Joensen)
      <li>08/08/03 - JAJ - Merged PS's and SG's update... Added TitleOffset...
      <li>03/07/04 - LR - Added constant for Keyboard (glKey_TAB, ...)
                          Added function GLOKMessageBox to avoid the uses of Forms
                          Replace TColor, TBitmap, TMouseEvent, TKeyEvent, ...
                          by TGLColor, TGLBitmap, TGLMouseEvent, TGLKeyEvent, ...
      <li>25/01/05 - AX - Corrected AlphaChannel default value, must be 1
                          TGLButton, TGLForm - AlphaChannel behaviour text.
                          Added events OnMouseEnter/OnMouseLeave for all controls
      <li>05/02/05 - AX - TGLLabel correct layout depending on Aligment and TextLayout.
	</ul></font>
}

unit GLWindows;

interface

uses
   SysUtils, Classes, GLMisc, GLScene, GLHUDObjects,
   GLTexture, OpenGL1x, GLBitmapFont, GLWindowsFont, VectorGeometry,
   GLGui, GLCrossPlatform;

type

  TGLBaseComponent = class(TGLBaseGuiObject)
  private
    FGUIRedraw : Boolean;
    FGuiLayout     : TGLGuiLayout;
    FGuiLayoutName : TGLGuiComponentName;
    FGuiComponent  : TGLGuiComponent;
    FReBuildGui    : Boolean;
    FRedrawAtOnce : Boolean;
    MoveX, MoveY : TGLFloat;
    FRenderStatus  : TGUIDrawResult;

    FAlphaChannel : Single;
    FRotation : TGLFloat;
    FNoZWrite : Boolean;

    BlockRendering : Boolean;
    RenderingCount : Integer;
    BlockedCount   : Integer;
    GuiDestroying : Boolean;
    FDoChangesOnProgress: Boolean;

    procedure SetGUIRedraw(value : Boolean);
	procedure SetDoChangesOnProgress(const Value: Boolean);
  protected
    Procedure RenderHeader(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
    Procedure RenderFooter(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

    procedure SetGuiLayout(NewGui : TGLGuiLayout); Virtual;
    procedure SetGuiLayoutName(NewName : TGLGuiComponentName);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure SetRotation(const val : TGLFloat);
    procedure SetAlphaChannel(const val : Single);
    function StoreAlphaChannel : Boolean;
    procedure SetNoZWrite(const val : Boolean);

  public
    Procedure BlockRender;
    Procedure UnBlockRender;

    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;

    procedure NotifyChange(Sender : TObject); override;
    Procedure DoChanges; virtual;
    Procedure MoveGUI(XRel, YRel : Single);
    Procedure PlaceGUI(XPos, YPos : Single);
	
    procedure DoProgress(const progressTime : TProgressTimes); override;

    procedure DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    Procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); Virtual;
    property  GUIRedraw     : Boolean read FGUIRedraw write SetGUIRedraw;
    property  ReBuildGui    : Boolean read FReBuildGui write FReBuildGui;
  published
    property  RedrawAtOnce  : Boolean                read FRedrawAtOnce  write FRedrawAtOnce;
    property  GuiLayout     : TGLGuiLayout           read FGuiLayout     write SetGuiLayout;
    property  GuiLayoutName : TGLGuiComponentName    read FGuiLayoutName write SetGuiLayoutName;

    {: This the ON-SCREEN rotation of the GuiComponent.<p>
       Rotatation=0 is handled faster. }
    property Rotation : TGLFloat read FRotation write SetRotation;
    {: If different from 1, this value will replace that of Diffuse.Alpha }
    property AlphaChannel : Single read FAlphaChannel write SetAlphaChannel stored StoreAlphaChannel;
    {: If True, GuiComponent will not write to Z-Buffer.<p>
       GuiComponent will STILL be maskable by ZBuffer test. }
    property NoZWrite : Boolean read FNoZWrite write SetNoZWrite;

    property DoChangesOnProgress : Boolean read FDoChangesOnProgress write SetDoChangesOnProgress;
    property Visible;
    property Width;
    property Height;
    property Left;
    property Top;
    property Position;

  End;

  TGLFocusControl = class;
  TGLBaseControl = class;

  TGLMouseAction = (ma_mouseup,ma_mousedown,ma_mousemove);

  TGLAcceptMouseQuery = procedure (Sender : TGLBaseControl; Shift: TShiftState; Action : TGLMouseAction; Button: TGLMouseButton; X, Y: Integer; var accept: boolean) of Object;
  TGLBaseControl = class(TGLBaseComponent)
  private
    FOnMouseDown: TGLMouseEvent;
    FOnMouseMove: TGLMouseMoveEvent;
    FOnMouseUp  : TGLMouseEvent;
    FKeepMouseEvents  : Boolean;
    FActiveControl    : TGLBaseControl;
    FFocusedControl   : TGLFocusControl;
    FOnAcceptMouseQuery : TGLAcceptMouseQuery;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FEnteredControl: TGLBaseControl;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); Virtual;
    procedure InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); Virtual;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); Virtual;
    Procedure SetActiveControl(NewControl : TGLBaseControl);
    Procedure SetFocusedControl(NewControl : TGLFocusControl);
    Function  FindFirstGui : TGLBaseControl;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoMouseEnter;
    procedure DoMouseLeave;
  public
    Function  MouseDown(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Function  MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Function  MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean; virtual;
    Procedure KeyPress(Sender: TObject; var Key: Char); virtual;
    Procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    Procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    property  ActiveControl : TGLBaseControl  read FActiveControl write SetActiveControl;
    property  KeepMouseEvents : Boolean         read FKeepMouseEvents write FKeepMouseEvents default false;
  published
    property  FocusedControl  : TGLFocusControl read FFocusedControl  write SetFocusedControl;
    property  OnMouseDown     : TGLMouseEvent     read FOnMouseDown     write FOnMouseDown;
    property  OnMouseMove     : TGLMouseMoveEvent read FOnMouseMove     write FOnMouseMove;
    property  OnMouseUp       : TGLMouseEvent     read FOnMouseUp       write FOnMouseUp;
    property  OnMouseEnter    : TNotifyEvent      read FOnMouseEnter    write FOnMouseEnter;
    property  OnMouseLeave    : TNotifyEvent      read FOnMouseLeave    write FOnMouseLeave;
    property  OnAcceptMouseQuery : TGLAcceptMouseQuery read FOnAcceptMouseQuery write FOnAcceptMouseQuery;
  End;

  TGLBaseFontControl = class(TGLBaseControl)
  private
    FBitmapFont: TGLCustomBitmapFont;
    FDefaultColor    : TColorVector;
  protected
    Function  GetDefaultColor : TDelphiColor;
    procedure SetDefaultColor(value : TDelphiColor);
    Procedure SetBitmapFont(NewFont : TGLCustomBitmapFont);
    Function  GetBitmapFont : TGLCustomBitmapFont;
    Procedure WriteTextAt(var rci : TRenderContextInfo; Const X,Y : TGLFloat; Const Data : String; const Color : TColorVector); overload;
    Procedure WriteTextAt(var rci : TRenderContextInfo; Const X1,Y1,X2,Y2 : TGLFloat; Const Data : String; const Color : TColorVector); overload;
    Function  GetFontHeight : Integer;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property BitmapFont : TGLCustomBitmapFont read GetBitmapFont write SetBitmapFont;
    property DefaultColor : TDelphiColor read GetDefaultColor write SetDefaultColor;
  end;

  TGLBaseTextControl = class(TGLBaseFontControl)
  private
    FCaption  : String;
  protected
    Procedure SetCaption(NewCaption : String);
  public
  published
    property  Caption : String read FCaption write SetCaption;
  end;

  TGLFocusControl = class(TGLBaseTextControl)
  private
    FRootControl      : TGLBaseControl;
    FFocused          : Boolean;
    FOnKeyDown        : TGLKeyEvent;
    FOnKeyUp          : TGLKeyEvent;
    FOnKeyPress       : TGLKeyPressEvent;
    FShiftState       : TShiftState;
    FFocusedColor     : TColorVector;
  protected
    Procedure InternalKeyPress(var Key: Char); virtual;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); virtual;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); virtual;
    Procedure SetFocused(Value :Boolean); virtual;
    Function  GetRootControl : TGLBaseControl;
    Function  GetFocusedColor : TDelphiColor;
    Procedure SetFocusedColor(const Val : TDelphiColor);
  public
    Destructor Destroy; override;
    procedure NotifyHide; override;
    procedure MoveTo(newParent : TGLBaseSceneObject); override;
    procedure ReGetRootControl;
    Procedure SetFocus;
    Procedure PrevControl;
    Procedure NextControl;
    Procedure KeyPress(Sender: TObject; var Key: Char); override;
    Procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    Procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  published
    property  RootControl    : TGLBaseControl   read GetRootControl;
    property  Focused        : Boolean          read FFocused    write SetFocused;
    property  FocusedColor   : TDelphiColor           read GetFocusedColor write SetFocusedColor;
    property  OnKeyDown      : TGLKeyEvent        read FOnKeyDown  write FOnKeyDown;
    property  OnKeyUp        : TGLKeyEvent        read FOnKeyUp    write FOnKeyUp;
    property  OnKeyPress     : TGLKeyPressEvent   read FOnKeyPress write FOnKeyPress;
  End;

  TGLCustomControl = Class;
  TGLCustomRenderEvent = procedure (sender : TGLCustomControl; Bitmap : TGLBitmap) of Object;
  TGLCustomControl = Class(TGLFocusControl)
  private
    FCustomData   : Pointer;
    FCustomObject : TObject;
    FOnRender     : TGLCustomRenderEvent;
    FMaterial     : TGLMaterial;
    FBitmap       : TGLBitmap;
    FInternalBitmap : TGLBitmap;
    FBitmapChanged : Boolean;
    FXTexCoord     : Single;
    FYTexCoord     : Single;
    FInvalidRenderCount : Integer;
    FMaxInvalidRenderCount : Integer;
    FCentered: Boolean;
    procedure SetCentered(const Value: Boolean);
  protected
    Procedure   OnBitmapChanged(Sender : TObject);
    Procedure   SetBitmap(ABitmap : TGLBitmap);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    procedure   InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    procedure   SetMaterial(AMaterial : TGLMaterial);
    property    CustomData   : Pointer            read FCustomData write FCustomData;
    property    CustomObject : TObject            read FCustomObject write FCustomObject;
  Published
    property    OnRender     : TGLCustomRenderEvent read FOnRender write FOnRender;
    property    Centered     : Boolean read FCentered write SetCentered;
    property    Material     : TGLMaterial read FMaterial write SetMaterial;
    property    Bitmap       : TGLBitmap read FBitmap write SetBitmap;
    property    MaxInvalidRenderCount : Integer read FMaxInvalidRenderCount write FMaxInvalidRenderCount;
  end;



  TGLPopupMenu = class;
  TGLPopupMenuClick = procedure (Sender: TGLPopupMenu; index : Integer; const MenuItemText : String) of Object;

  TGLPopupMenu = class(TGLFocusControl)
  private
    FOnClick : TGLPopupMenuClick;
    FMenuItems : TStrings;
    FSelIndex  : Integer;
    FMarginSize : Single;
    NewHeight   : Single;
  protected
    Procedure SetFocused(Value :Boolean); override;
    Procedure SetMenuItems(Value :TStrings);
    Procedure SetMarginSize(const val : Single);
    Procedure SetSelIndex(const val : Integer);
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure OnStringListChange(Sender : TObject);
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    Procedure   PopUp(Px,Py : Integer);
    procedure   InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    procedure   DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    Function    MouseDown(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
  published
    property MenuItems : TStrings read FMenuItems write SetMenuItems;
    property OnClick : TGLPopupMenuClick read FOnClick write FOnClick;
    property MarginSize : Single read FMarginSize write SetMarginSize;
    property SelIndex : Integer read FSelIndex write SetSelIndex;
  End;
  TGLForm = class;

  TGLFormCanRequest   = procedure (Sender: TGLForm; var Can: Boolean) of Object;
  TGLFormCloseOptions = (co_Hide, co_Ignore, co_Destroy);
  TGLFormCanClose     = procedure (Sender: TGLForm; var CanClose: TGLFormCloseOptions) of Object;
  TGLFormNotify       = procedure (Sender: TGLForm) of Object;
  TGLFormMove         = procedure (Sender: TGLForm; Var Left, Top : Single) of Object;

  TGLForm = class(TGLBaseTextControl)
  private
    FOnCanMove     : TGLFormCanRequest;
    FOnCanResize   : TGLFormCanRequest;
    FOnCanClose    : TGLFormCanClose;
    FOnShow        : TGLFormNotify;
    FOnHide        : TGLFormNotify;
    FOnMoving      : TGLFormMove;
    Moving         : Boolean;
    OldX           : Integer;
    OldY           : Integer;
    FTitleColor    : TColorVector;
    FTitleOffset   : Single;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
    Function  GetTitleColor : TDelphiColor;
    procedure SetTitleColor(value : TDelphiColor);
  public
    Constructor Create(AOwner : TComponent); override;
    Procedure   Close;

    procedure NotifyShow; override;
    procedure NotifyHide; override;
    Function  MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    Function  MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property  TitleColor    : TDelphiColor                 read GetTitleColor  write SetTitleColor;
    property  OnCanMove     : TGLFormCanRequest      read FOnCanMove     write FOnCanMove;
    property  OnCanResize   : TGLFormCanRequest      read FOnCanResize   write FOnCanResize;
    property  OnCanClose    : TGLFormCanClose        read FOnCanClose    write FOnCanClose;
    property  OnShow        : TGLFormNotify          read FOnShow        write FOnShow;
    property  OnHide        : TGLFormNotify          read FOnHide        write FOnHide;
    property  OnMoving      : TGLFormMove            read FOnMoving      write FOnMoving;
    property  TitleOffset   : Single                 read FTitleOffset   write FTitleOffset;
  end;

  TGLPanel = class(TGLBaseControl)
  end;

  TGLCheckBox = class(TGLBaseControl)
  private
    FChecked : Boolean;
    FOnChange : TNotifyEvent;
    FGuiLayoutNameChecked : TGLGuiComponentName;
    FGuiCheckedComponent  : TGLGuiComponent;
    FGroup                : Integer;
  protected
    Procedure SetChecked(NewChecked : Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure SetGuiLayoutNameChecked(newName : TGLGuiComponentName);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;
    Procedure SetGroup(const val : Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    procedure NotifyChange(Sender : TObject); override;
  published
    property  Group : Integer read FGroup write SetGroup;
    property  Checked : Boolean read FChecked write SetChecked;
    property  OnChange : TNotifyEvent read FOnChange write FOnChange;
    property  GuiLayoutNameChecked : TGLGuiComponentName read FGuiLayoutNameChecked Write SetGuiLayoutNameChecked;
  end;

  TGLButton = class(TGLFocusControl)
  private
    FPressed              : Boolean;
    FOnButtonClick        : TNotifyEvent;
    FGuiLayoutNamePressed : TGLGuiComponentName;
    FGuiPressedComponent  : TGLGuiComponent;
    FBitBtn               : TGLMaterial;
    FGroup                : Integer;
    FLogicWidth           : Single;
    FLogicHeight          : Single;
    FXOffSet              : Single;
    FYOffSet              : Single;
    FAllowUp              : Boolean;
  protected
    Procedure SetPressed(NewPressed : Boolean);
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    Procedure SetFocused(Value :Boolean); override;
    procedure SetGuiLayoutNamePressed(newName : TGLGuiComponentName);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;
    procedure SetBitBtn(AValue: TGLMaterial);
    procedure DestroyHandle; override;
    Procedure SetGroup(const val : Integer);
    Procedure SetLogicWidth(const val : single);
    Procedure SetLogicHeight(const val : single);
    Procedure SetXOffset(const val : single);
    Procedure SetYOffset(const val : single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property Group : Integer read FGroup write SetGroup;
    property BitBtn : TGLMaterial read FBitBtn write SetBitBtn;
    property Pressed : Boolean read FPressed write SetPressed;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property GuiLayoutNamePressed : TGLGuiComponentName read FGuiLayoutNamePressed Write SetGuiLayoutNamePressed;
    property LogicWidth           : Single read FLogicWidth write SetLogicWidth;
    property LogicHeight          : Single read FLogicHeight write SetLogicHeight;
    property XOffset              : Single read FXOffset write SetXOffset;
    property YOffset              : Single read FYOffset write SetYOffset;
    property AllowUp              : Boolean read FAllowUp write FAllowUp;
  end;

  TGLEdit = class(TGLFocusControl)
  private
    FOnChange  : TNotifyEvent;
    FSelStart  : Integer;
    FEditChar  : String;
  protected
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    Procedure InternalKeyPress(var Key: Char); override;
    Procedure InternalKeyDown(var Key: Word; Shift: TShiftState); override;
    Procedure InternalKeyUp(var Key: Word; Shift: TShiftState); override;
    Procedure SetFocused(Value :Boolean); override;
    Procedure SetSelStart(const Value : Integer);
    Procedure SetEditChar(const Value : String);
  public
    Constructor Create(AOwner : TComponent); override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property  EditChar : String read FEditChar write SetEditChar;
    property  OnChange : TNotifyEvent read FOnChange write FOnChange;
    property  SelStart : Integer read FSelStart write SetSelStart;
  end;

  TGLLabel = class(TGLBaseTextControl)
  private
    FAlignment: TAlignment;
    FTextLayout: TGLTextLayout;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetTextLayout(const Value: TGLTextLayout);
  protected
  public
    Constructor Create(AOwner : TComponent); override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property Alignment : TAlignment read FAlignment write SetAlignment;
    property TextLayout: TGLTextLayout read FTextLayout write SetTextLayout;
  end;

  TGLAdvancedLabel = class(TGLFocusControl)
  private
  protected
  public
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
  end;

  TGLScrollbar = class(TGLFocusControl)
  private
    FMin      : Single;
    FMax      : Single;
    FStep     : Single;
    FPos      : Single;
    FPageSize : Single;
    FOnChange : TNotifyEvent;
    FGuiLayoutKnobName : TGLGuiComponentName;
    FGuiKnobComponent  : TGLGuiComponent;
    FKnobRenderStatus      : TGUIDrawResult;
    FScrollOffs  : Single;
    FScrolling   : Boolean;
    FHorizontal : Boolean;
  protected
    Procedure SetMin(const val      : Single);
    Procedure SetMax(const val      : Single);
    Procedure SetPos(const val : Single);
    Procedure SetPageSize(const val : Single);
    Procedure SetHorizontal(const val : Boolean);
    procedure SetGuiLayoutKnobName(newName : TGLGuiComponentName);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;

    Function  GetScrollPosY(ScrollPos : Single) : Single;
    Function  GetYScrollPos(Y : Single) : Single;

    Function  GetScrollPosX(ScrollPos : Single) : Single;
    Function  GetXScrollPos(X : Single) : Single;

    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner : TComponent); override;

    Procedure StepUp;
    Procedure StepDown;
    Procedure PageUp;
    Procedure PageDown;
    Function  MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    Function  MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean; override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
  published
    property Horizontal : Boolean read FHorizontal write SetHorizontal;
    property Pos      : Single read FPos write SetPos;
    property Min      : Single read FMin write SetMin;
    property Max      : Single read FMax write SetMax;
    property Step     : Single read FStep write FStep;
    property PageSize : Single read FPageSize write SetPageSize;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property GuiLayoutKnobName : TGLGuiComponentName read FGuiLayoutKnobName Write SetGuiLayoutKnobName;
  End;


  TGLStringGrid = class(TGLFocusControl)
  private
    FSelCol,FSelRow : Integer;
    FRowSelect : Boolean;
    FColumns : TStrings;
    FRows    : TList;
    FHeaderColor : TColorVector;
    FMarginSize : Integer;
    FColumnSize : Integer;
    FRowHeight  : Integer;
    FScrollbar  : TGLScrollbar;
    FDrawHeader : Boolean;
  protected
    Function  GetCell(X,Y : Integer; out oCol,oRow : Integer) : Boolean;
    procedure InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer); override;
    Procedure SetColumns(const val : TStrings);
    Function  GetRow (index : Integer) : TStringList;
    Procedure SetRow(index : Integer; const val : TStringList);
    Function  GetRowCount : Integer;
    Procedure SetRowCount(const val : Integer);
    Procedure SetSelCol(const val : Integer);
    Procedure SetSelRow(const val : Integer);
    Procedure SetRowSelect(const val : Boolean);
    Procedure SetDrawHeader(const val : Boolean);
    Function  GetHeaderColor : TDelphiColor;
    Procedure SetHeaderColor(const val : TDelphiColor);
    Procedure SetMarginSize(const val : Integer);
    Procedure SetColumnSize(const val : Integer);
    Procedure SetRowHeight(const val : Integer);
    Procedure SetScrollbar(const val : TGLScrollbar);
    procedure SetGuiLayout(NewGui : TGLGuiLayout); Override;
  public
    constructor Create(AOwner : TComponent); override;
    Destructor  Destroy; override;
    Procedure Clear;
    Function  Add(Data : Array of String) : Integer; overload;
    Function  Add(const Data : String) : Integer; overload;
    procedure SetText(Data : String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure NotifyChange(Sender : TObject); override;
    procedure InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean); override;
    Procedure OnStringListChange(Sender : TObject);
    property  Row[index : Integer] : TStringList read GetRow write SetRow;
  published
    property HeaderColor : TDelphiColor read GetHeaderColor write SetHeaderColor;
    property Columns : TStrings read FColumns write SetColumns;
    property MarginSize : Integer read FMarginSize write SetMarginSize;
    property ColumnSize : Integer read FColumnSize write SetColumnSize;
    property RowHeight : Integer read FRowHeight write SetRowHeight;
    property RowCount : Integer read GetRowCount write SetRowCount;
    property SelCol : Integer read FSelCol write SetSelCol;
    property SelRow : Integer read FSelRow write SetSelRow;
    property RowSelect : Boolean read FRowSelect write SetRowSelect;
    property DrawHeader : Boolean read FDrawHeader write SetDrawHeader;
    property Scrollbar : TGLScrollbar read FScrollbar write SetScrollbar;
  end;

Function  UnpressGroup(CurrentObject : TGLBaseSceneObject; AGroupID : Integer) : Boolean;

implementation

uses GLObjects, GLState, GLUtils;

Function  UnpressGroup(CurrentObject : TGLBaseSceneObject; AGroupID : Integer) : Boolean;

Var
  XC : Integer;

Begin
  Result := False;
  If CurrentObject is TGLButton then
  With CurrentObject as TGLButton do
  Begin
    if Group = AGroupID then
    If Pressed then
    Begin
      Pressed := False;
      Result := True;
      Exit;
    End;
  End;

  If CurrentObject is TGLCheckBox then
  With CurrentObject as TGLCheckBox do
  Begin
    if Group = AGroupID then
    If Checked then
    Begin
      Checked := False;
      Result := True;
      Exit;
    End;
  End;

  For XC := 0 to CurrentObject.Count-1 do
  Begin
    if UnpressGroup(CurrentObject.Children[XC],AGroupID) then
    begin
      Result := True;
      Exit;
    End;
  End;
End;

procedure TGLBaseComponent.SetGUIRedraw(value : Boolean);

Begin
  FGUIRedraw := Value;
  If Value then
  Begin
    If csDestroying in ComponentState then Exit;
    if (FRedrawAtOnce) or (csDesigning in ComponentState) then
    Begin
      FGUIRedraw := False;
      StructureChanged;
    End;
  End;
End;

Procedure TGLBaseComponent.BlockRender;

Begin
  While BlockedCount <> 0 do Sleep(1);
  BlockRendering := True;
  While RenderingCount <> BlockedCount do Sleep(1);
End;

Procedure TGLBaseComponent.UnBlockRender;

Begin
  BlockRendering := False;
End;

Procedure TGLBaseComponent.RenderHeader(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  f : Single;
Begin
   FGuiLayout.Material.Apply(rci);
   if AlphaChannel<>1 then
      rci.GLStates.SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
   // Prepare matrices
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentBuffer.BaseProjectionMatrix);
   if rci.renderDPI=96 then
      f:=1
   else f:=rci.renderDPI/96;
   glScalef(f*2/rci.viewPortSize.cx, f*2/rci.viewPortSize.cy, 1);
   glTranslatef(f*Position.X-rci.viewPortSize.cx*0.5,
                rci.viewPortSize.cy*0.5-f*Position.Y, 0);
   if Rotation<>0 then
      glRotatef(Rotation, 0, 0, 1);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_DEPTH_TEST);
   glDepthMask(False);
End;

Procedure TGLBaseComponent.RenderFooter(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
   glDepthMask(True);
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
   FGuiLayout.Material.UnApply(rci);
End;

procedure TGLBaseComponent.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  If FGuiLayout <> NewGui then
  Begin
    If Assigned(FGuiLayout) then
    Begin
      FGuiLayout.RemoveGuiComponent(Self);
    End;
    FGuiComponent := Nil;
    FGuiLayout := NewGui;
    If Assigned(FGuiLayout) then
    If FGuiLayoutName <> '' then
      FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);

    // in effect this code have been moved...
    If Assigned(FGuiLayout) then
      FGuiLayout.AddGuiComponent(Self);

    NotifyChange(Self);
  End;
End;

procedure TGLBaseComponent.SetGuiLayoutName(NewName : TGLGuiComponentName);

Begin
  If FGuiLayoutName <> NewName then
  Begin
    FGuiComponent := Nil;
    FGuiLayoutName := NewName;
    If FGuiLayoutName <> '' then 
    If Assigned(FGuiLayout) then
    Begin
      FGuiComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutName);
    End;
    NotifyChange(Self);
  End;
End;

procedure TGLBaseComponent.Notification(AComponent: TComponent; Operation: TOperation);

Begin
  If Operation = opRemove then
  Begin
    If AComponent = FGuiLayout then
    Begin
      BlockRender;
      GuiLayout := Nil;
      UnBlockRender;
    End;
  End;

  inherited;
End;


// SetRotation
//
procedure TGLBaseComponent.SetRotation(const val : TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		NotifyChange(Self);
	end;
end;

// SetAlphaChannel
//
procedure TGLBaseComponent.SetAlphaChannel(const val : Single);
begin
   if val<>FAlphaChannel then begin
      if val<0 then
         FAlphaChannel:=0
      else if val>1 then
         FAlphaChannel:=1
      else FAlphaChannel:=val;
		NotifyChange(Self);
   end;
end;

// StoreAlphaChannel
//
function TGLBaseComponent.StoreAlphaChannel : Boolean;
begin
	Result:=(FAlphaChannel<>1);
end;

// SetNoZWrite
//
procedure TGLBaseComponent.SetNoZWrite(const val : Boolean);
begin
   FNoZWrite:=val;
   NotifyChange(Self);
end;

Constructor TGLBaseComponent.Create(AOwner : TComponent);

Begin
  inherited;
  FGuiLayout := nil;
  FGuiComponent := nil;
  BlockRendering := False;
  BlockedCount := 0;
  RenderingCount := 0;
  Width      := 50;
  Height     := 50;
  FReBuildGui := True;
  GuiDestroying := False;
  FAlphaChannel := 1;
End;

Destructor  TGLBaseComponent.Destroy;

Begin
  GuiDestroying := True;
  While RenderingCount > 0 do Sleep(1);

  GuiLayout := Nil;
  inherited;
End;

procedure TGLBaseComponent.NotifyChange(Sender : TObject);

Begin
  If Sender = FGuiLayout then
  Begin
    If (FGuiLayoutName <> '') and (GuiLayout <> Nil) then
    Begin
      BlockRender;
      FGuiComponent := GuiLayout.GuiComponents.FindItem(FGuiLayoutName);
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    End else
    Begin
      BlockRender;
      FGuiComponent := Nil;
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    End;
  End;
  If Sender = Self then
  Begin
    ReBuildGui := True;
    GUIRedraw := True;
  End;
  inherited;
End;

Procedure TGLBaseComponent.MoveGUI(XRel, YRel : Single);

Var
  XC : Integer;

Begin
  If RedrawAtOnce then
  Begin
    BeginUpdate;
    try
      MoveX := MoveX + XRel;
      MoveY := MoveY + YRel;
      For XC := 0 to Count -1 do
      If Children[XC] is TGLBaseComponent then
      Begin
        (Children[XC] as TGLBaseComponent).MoveGUI(XRel,YRel);
      End;
      GUIRedraw := True;
      DoChanges;
    finally
      Endupdate;
    End;
  End else
  Begin
    MoveX := MoveX + XRel;
    MoveY := MoveY + YRel;
    For XC := 0 to Count -1 do
    If Children[XC] is TGLBaseComponent then
    Begin
      (Children[XC] as TGLBaseComponent).MoveGUI(XRel,YRel);
    End;
    GUIRedraw := True;
  End;
End;

Procedure TGLBaseComponent.PlaceGUI(XPos, YPos : Single);

Begin
  MoveGUI(XPos-Left,YPos-Top);
end;

Procedure TGLBaseComponent.DoChanges;

Var
  XC : Integer;

Begin
  If GUIRedraw then
  Begin
    GUIRedraw := False;
    BeginUpdate;
    try
      If MoveX <> 0 then Position.X := Position.X + MoveX;
      If MoveY <> 0 then Position.Y := Position.Y + MoveY;
      MoveX := 0;
      MoveY := 0;

      For XC := 0 to Count-1 do
      If Children[XC] is TGLBaseComponent then
      Begin
        (Children[XC] as TGLBaseComponent).DoChanges;
      End;
    finally
      EndUpdate;
    End;
  End else
  Begin
    For XC := 0 to Count-1 do
    If Children[XC] is TGLBaseComponent then
    Begin
      (Children[XC] as TGLBaseComponent).DoChanges;
    End;
  End;
End;

Procedure TGLBaseComponent.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
  If Assigned(FGuiComponent) then
  Begin
    try
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in GuiComponents InternalRender function');
    end;
  End;
End;

procedure TGLBaseComponent.DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  B : Boolean;
Begin
  Inc(RenderingCount);
  B := BlockRendering;
  If B then
  Begin
    Inc(BlockedCount);
    While BlockRendering do sleep(1);
    Dec(BlockedCount);
  End;

  If not GuiDestroying then
  If RenderSelf then
  If FGuiLayout <> nil then
  Begin
    RenderHeader(rci,renderSelf,renderChildren);

    InternalRender(rci,RenderSelf,RenderChildren);

    RenderFooter(rci,renderSelf,renderChildren);
    FReBuildGui := False;
  End;

  If renderChildren then
  if Count>0 then
    Self.RenderChildren(0, Count-1, rci);
  Dec(RenderingCount);
End;

procedure TGLBaseControl.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Begin
  If Assigned(FOnMouseDown) then FOnMouseDown(Self,Button,Shift,X,Y);
End;

procedure TGLBaseControl.InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Begin
  If Assigned(FOnMouseUp) then FOnMouseUp(Self,Button,Shift,X,Y);
End;

procedure TGLBaseControl.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

Begin
  If Assigned(FOnMouseMove) then FOnMouseMove(Self,Shift,X,Y);
End;

Procedure TGLBaseControl.SetActiveControl(NewControl : TGLBaseControl);

Begin
  FActiveControl := NewControl;
End;

Procedure TGLBaseControl.SetFocusedControl(NewControl : TGLFocusControl);

Begin
  If NewControl <> FFocusedControl then
  Begin
    If Assigned(FFocusedControl) then
      FFocusedControl.Focused := False;
    FFocusedControl := NewControl;
    If Assigned(FFocusedControl) then
      FFocusedControl.Focused := True;
  End;
End;

Function  TGLBaseControl.FindFirstGui : TGLBaseControl;

Var
  tmpFirst : TGLBaseControl;
  TmpRoot : TGLBaseSceneObject;

Begin
  tmpFirst := Self;

  TmpRoot := Self;
  While (TmpRoot is TGLBaseComponent) do
  Begin
    If Assigned(TmpRoot.parent) then
    Begin
      If TmpRoot.parent is TGLBaseComponent then
      Begin
        TmpRoot := TmpRoot.parent as TGLBaseComponent;
        If TmpRoot is TGLBaseControl then tmpFirst := TmpRoot as TGLBaseControl;
      End else Break;
    End else Break;
  End;
  Result := tmpFirst;
End;

procedure TGLBaseControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  If Operation = opRemove then
  Begin
    if FEnteredControl <> nil then
    begin
      FEnteredControl.DoMouseLeave;
      FEnteredControl := nil;
    end;
  End;

  inherited;
end;

Function  TGLBaseControl.MouseDown(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
  AcceptMouseEvent : Boolean;

Begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X+Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y));
  If Assigned(OnAcceptMouseQuery) then OnAcceptMouseQuery(Self,shift,ma_mousedown,Button,X,Y,AcceptMouseEvent);

  If AcceptMouseEvent then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseDown(Sender,Button,Shift,X,Y) then Exit;

      For XC := count-1 downto 0 do
      If FActiveControl <> Children[XC] then
      Begin
        If Children[XC] is TGLBaseControl then
        Begin
          If (Children[XC] as TGLBaseControl).MouseDown(Sender,button,shift,x,y) then Exit;
        End;
      End;
    End;
    InternalMouseDown(Shift,Button,X,Y);
  End;
End;

Function  TGLBaseControl.MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
  AcceptMouseEvent : Boolean;

Begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X+Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y));
  If Assigned(OnAcceptMouseQuery) then OnAcceptMouseQuery(Self,shift,ma_mouseup,Button,X,Y,AcceptMouseEvent);

  If AcceptMouseEvent then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseUp(Sender,button,shift,x,y) then Exit;

      For XC := count-1 downto 0 do
      If FActiveControl <> Children[XC] then
      Begin
        If Children[XC] is TGLBaseControl then
        Begin
          If (Children[XC] as TGLBaseControl).MouseUp(Sender,button,shift,x,y) then Exit;
        End;
      End;
    End;
    InternalMouseUp(Shift,Button,X,Y);
  End;
End;

Function  TGLBaseControl.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean;
Var
  Xc : Integer;
  AcceptMouseEvent : Boolean;

Begin
  Result := False;

  AcceptMouseEvent := RecursiveVisible and ((Position.X <= X) and (Position.X + Width > X) and (Position.Y <= Y) and (Position.Y+Height > Y));
  If Assigned(OnAcceptMouseQuery) then
    OnAcceptMouseQuery(Self, shift, ma_mousemove, mbMiddle, X, Y, AcceptMouseEvent);

  If AcceptMouseEvent then
  Begin
    Result := True;
    If not FKeepMouseEvents then
    Begin
      If Assigned(FActiveControl) then
      If FActiveControl.MouseMove(Sender,shift,x,y) then Exit;

      For XC := count-1 downto 0 do
        If FActiveControl <> Children[XC] then
        Begin
          If Children[XC] is TGLBaseControl then
          Begin
            If (Children[XC] as TGLBaseControl).MouseMove(Sender,shift,x,y) then
            begin
              if FEnteredControl <> (Children[XC] as TGLBaseControl) then
              begin
                if FEnteredControl <> nil then
                begin
                  FEnteredControl.DoMouseLeave;
                end;

                FEnteredControl := (Children[XC] as TGLBaseControl);

                if FEnteredControl <> nil then
                begin
                  FEnteredControl.DoMouseEnter;
                end;
              end;

              Exit;
            end;
          End;
        End;
    End;

    if FEnteredControl <> nil then
    begin
      FEnteredControl.DoMouseLeave;
      FEnteredControl := nil;
    end;

    InternalMouseMove(Shift,X,Y);
  End;
End;

Procedure   TGLBaseControl.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyDown(Sender,Key,Shift);
  End;
End;

Procedure   TGLBaseControl.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyUp(Sender,Key,Shift);
  End;
End;

Procedure   TGLBaseControl.KeyPress(Sender: TObject; var Key: Char);

Begin
  If Assigned(FFocusedControl) then
  Begin
    FFocusedControl.KeyPress(Sender,Key);
  End;
End;

Procedure TGLFocusControl.InternalKeyPress(var Key: Char);
Begin
  if assigned(FOnKeyPress) then FOnKeyPress(Self,Key);
End;

Procedure TGLFocusControl.InternalKeyDown(var Key: Word; Shift: TShiftState);
Begin
  if assigned(FOnKeyDown) then FOnKeyDown(Self,Key,shift);
End;

Procedure TGLFocusControl.InternalKeyUp(var Key: Word; Shift: TShiftState);
Begin
  if assigned(FOnKeyUp) then FOnKeyUp(Self,Key,shift);
End;

procedure TGLBaseControl.DoMouseEnter;
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Self);
end;

procedure TGLBaseControl.DoMouseLeave;
begin
  //leave all child controls
  if FEnteredControl <> nil then
  begin
    FEnteredControl.DoMouseLeave;
    FEnteredControl := nil;
  end;

  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

Procedure TGLFocusControl.SetFocused(Value :Boolean);
Begin
  If Value <> FFocused then
  Begin
    FFocused := Value;
    GUIRedraw :=True;
  End;
End;

Function  TGLFocusControl.GetRootControl : TGLBaseControl;

Begin
  if not Assigned(FRootControl) then
  Begin
    FRootControl := FindFirstGui;
  End;
  Result := FRootControl;
End;

procedure TGLFocusControl.NotifyHide;

Begin
  inherited;
  If (RootControl.FFocusedControl = Self) and (self.focused) then
  Begin
    RootControl.FocusedControl.PrevControl;
  End;
End;

procedure TGLFocusControl.ReGetRootControl;

Begin
  FRootControl := FindFirstGui;
End;

Function  TGLFocusControl.GetFocusedColor : TDelphiColor;

Begin
  Result := ConvertColorVector(FFocusedColor);
End;

Procedure TGLFocusControl.SetFocusedColor(const Val : TDelphiColor);

Begin
  FFocusedColor := ConvertWinColor(val);
  GUIRedraw := True;
End;

Procedure TGLFocusControl.SetFocus;

Begin
  RootControl.FocusedControl := Self;
End;

Procedure TGLFocusControl.NextControl;

Var
  Host : TGLBaseComponent;
  Index : Integer;
  IndexedChild : TGLBaseComponent;
  RestartedLoop : Boolean;

Begin
  RestartedLoop := False;
  If Parent is TGLBaseComponent then
  Begin
    Host := Parent as TGLBaseComponent;
    Index := Host.IndexOfChild(Self);
    While not Host.RecursiveVisible do
    Begin
      If Host.Parent is TGLBaseComponent then
      Begin
        IndexedChild := Host;
        Host := Host.Parent as TGLBaseComponent;
        Index := Host.IndexOfChild(IndexedChild);
      End else
      Begin
        RootControl.FocusedControl := Nil;
        Exit;
      End;
    End;

    While true do
    Begin
      If Index > 0 then
      Begin
        Dec(Index);
        If Host.Children[Index] is TGLFocusControl then
        Begin
          With (Host.Children[Index] as TGLFocusControl) do
          If RecursiveVisible then
          Begin
            SetFocus;
            Exit;
          End;
        End else
        Begin
          If Host.Children[Index] is TGLBaseComponent then
          Begin
            IndexedChild := Host.Children[Index] as TGLBaseComponent;
            If IndexedChild.RecursiveVisible then
            Begin
              Host := IndexedChild;
              Index := Host.Count;
            End;
          End;
        End;
      End else
      Begin
        If Host.Parent is TGLBaseComponent then
        Begin
          Index := Host.Parent.IndexOfChild(Host);
          Host := Host.Parent as TGLBaseComponent;
        End else
        Begin
          If RestartedLoop then
          Begin
            SetFocus;
            Exit;
          End;
          Index := Host.Count;
          RestartedLoop := True;
        End;
      End;
    End;
  End;
End;

Procedure TGLFocusControl.PrevControl;

Var
  Host : TGLBaseComponent;
  Index : Integer;
  IndexedChild : TGLBaseComponent;
  RestartedLoop : Boolean;

Begin
  RestartedLoop := False;
  If Parent is TGLBaseComponent then
  Begin
    Host := Parent as TGLBaseComponent;
    Index := Host.IndexOfChild(Self);
    While not Host.RecursiveVisible do
    Begin
      If Host.Parent is TGLBaseComponent then
      Begin
        IndexedChild := Host;
        Host := Host.Parent as TGLBaseComponent;
        Index := Host.IndexOfChild(IndexedChild);
      End else
      Begin
        RootControl.FocusedControl := Nil;
        Exit;
      End;
    End;

    While true do
    Begin
      Inc(Index);

      If Index < Host.Count then
      Begin
        If Host.Children[Index] is TGLFocusControl then
        Begin
          With (Host.Children[Index] as TGLFocusControl) do
          If RecursiveVisible then
          Begin
            SetFocus;
            Exit;
          End;
        End;
        If Host.Children[Index] is TGLBaseComponent then
        Begin
          IndexedChild := Host.Children[Index] as TGLBaseComponent;
          If IndexedChild.RecursiveVisible then
          Begin
            Host := IndexedChild;
            Index := -1;
          End;
        End;
      End else
      Begin
        If Host.Parent is TGLBaseComponent then
        Begin
          IndexedChild := Host;
          Host := Host.Parent as TGLBaseComponent;
          Index := Host.IndexOfChild(IndexedChild);
        End else
        Begin
          If RestartedLoop then
          Begin
            RootControl.FocusedControl := Nil;
            Exit;
          End;
          Index := -1;
          RestartedLoop := True;
        End;
      End;
    End;
  End;
End;

Procedure TGLFocusControl.KeyPress(Sender: TObject; var Key: Char);

Begin
  InternalKeyPress(Key);
  If Key = #9 then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;
End;

Procedure TGLFocusControl.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  FShiftState := Shift;
  InternalKeyDown(Key,Shift);
  If Key = glKey_TAB then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;
End;

Procedure TGLFocusControl.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
Begin
  FShiftState := Shift;
  InternalKeyUp(Key,Shift);
  If Key = glKey_TAB then
  Begin
    If ssShift in FShiftState then
    Begin
      PrevControl;
    End else
    Begin
      NextControl;
    End;
  End;

End;

{ base font control }

Constructor TGLBaseFontControl.Create(AOwner : TComponent);

Begin
  inherited;
  FBitmapFont := nil;
  FDefaultColor := clrBlack;
End;

Destructor TGLBaseFontControl.Destroy;
Begin
  inherited;
  BitmapFont := Nil;
End;

Procedure TGLBaseFontControl.SetBitmapFont(NewFont : TGLCustomBitmapFont);

Begin
   if NewFont<>FBitmapFont then begin
      if Assigned(FBitmapFont) then
      Begin
         FBitmapFont.RemoveFreeNotification(Self);
         FBitmapFont.UnRegisterUser(Self);
      End;
      FBitmapFont:=NewFont;
      if Assigned(FBitmapFont) then begin
         FBitmapFont.RegisterUser(Self);
         FBitmapFont.FreeNotification(Self);
      end;
      GUIRedraw := True;
   end;
End;

Function  TGLBaseFontControl.GetBitmapFont : TGLCustomBitmapFont;

Begin
  Result := Nil;
  if Assigned(FBitmapFont) then
     Result := FBitmapFont
  else
  if Assigned(GuiLayout) then
  if Assigned(GuiLayout.BitmapFont) then
  Begin
    If not (csDesigning in ComponentState) then
    Begin
      If not GuiDestroying then
      Begin
        BitmapFont := GuiLayout.BitmapFont;
        Result := FBitmapFont;
      End;
    End else
    Result := GuiLayout.BitmapFont;
  End;
End;

Function  TGLBaseFontControl.GetDefaultColor : TDelphiColor;

Begin
  Result := ConvertColorVector(FDefaultColor);
End;

procedure TGLBaseFontControl.SetDefaultColor(value : TDelphiColor);

Begin
  FDefaultColor := ConvertWinColor(value);
  GUIRedraw := True;
  NotifyChange(Self);
End;

procedure TGLBaseFontControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation=opRemove) and (AComponent=FBitmapFont) then
  Begin
    BlockRender;
    BitmapFont:=nil;
    UnBlockRender;
  End;
  inherited;
end;

{ GLWindow }

Procedure TGLBaseTextControl.SetCaption(NewCaption : String);

Begin
  FCaption := NewCaption;
  GuiRedraw := True;
End;

Procedure TGLBaseFontControl.WriteTextAt(var rci : TRenderContextInfo; Const X,Y : TGLFloat; Const Data : String; Const Color : TColorVector);

Var
  Position : TVector;
Begin
  If Assigned(BitmapFont) then
  Begin
    Position[0] := Round(X);
    Position[1] := Round(Y);
    Position[2] := 0;
    Position[3] := 0;
    BitmapFont.RenderString(rci, Data,taLeftJustify,tlTop,Color, @Position);
  End;
End;

Procedure TGLBaseFontControl.WriteTextAt(var rci : TRenderContextInfo; Const X1,Y1,X2,Y2 : TGLFloat; Const Data : String; const Color : TColorVector);
var
  Position : TVector;
Begin
  If Assigned(BitmapFont) then
  Begin
    Position[0] := Round(((X2+X1-BitmapFont.CalcStringWidth(Data))*0.5));
    Position[1] := Round(-((Y2+Y1-GetFontHeight)*0.5))+2;
    Position[2] := 0;
    Position[3] := 0;
    BitmapFont.RenderString(rci, Data,taLeftJustify,tlTop,Color,@Position);
  End;
End;


Function  TGLBaseFontControl.GetFontHeight : Integer;

Begin
  If Assigned(BitmapFont) then
    If BitmapFont is TGLWindowsBitmapFont then
      Result := Abs((BitmapFont as TGLWindowsBitmapFont).Font.Height)
    else
      Result := BitmapFont.CharHeight
  else Result := -1;
End;

Constructor TGLCustomControl.Create(AOwner : TComponent);

Begin
  inherited;
  FMaterial := TGLMaterial.create(Self);
  FBitmap   := TGLBitmap.create;
  FBitmap.OnChange := OnBitmapChanged;
  FInternalBitmap := Nil;
  FInvalidRenderCount := 0;

  FXTexCoord     := 1;
  FYTexCoord     := 1;
End;

Destructor  TGLCustomControl.Destroy;
Begin
  If Assigned(FInternalBitmap) then FInternalBitmap.Free;
  Bitmap.Free;
  FMaterial.Free;
  inherited;
End;

procedure TGLCustomControl.SetCentered(const Value: Boolean);
begin
  FCentered := Value;
end;

Procedure   TGLCustomControl.OnBitmapChanged(Sender : TObject);
Begin
  FBitmapChanged := True;
End;

Procedure   TGLCustomControl.SetBitmap(ABitmap : TGLBitmap);
Begin
  FBitmap.Assign(ABitmap);
End;

procedure   TGLCustomControl.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  X1,X2,Y1,Y2 : Single;

Begin
  If Assigned(OnRender) then
  OnRender(self, FBitmap);

  If FBitmapChanged then
  If FInvalidRenderCount >= FMaxInvalidRenderCount then
  Begin
    FInvalidRenderCount := 0;
    If not Assigned(FInternalBitmap) then FInternalBitmap := TGLBitmap.Create;

    FInternalBitmap.PixelFormat := FBitmap.PixelFormat;
    FInternalBitmap.Width  := RoundUpToPowerOf2(FBitmap.Width);
    FInternalBitmap.Height := RoundUpToPowerOf2(FBitmap.Height);
    FInternalBitmap.Canvas.CopyRect(FBitmap.Canvas.ClipRect,FBitmap.Canvas,FBitmap.Canvas.ClipRect);
    FBitmapChanged := False;
    With Material.GetActualPrimaryTexture do
    Begin
      Disabled := False;
      Image.Assign(FInternalBitmap);
    End;
    FXTexCoord     := FBitmap.Width / FInternalBitmap.Width;
    FYTexCoord     := FBitmap.Height / FInternalBitmap.Height;
  End else Inc(FInvalidRenderCount);

  If Assigned(FGuiComponent) then
  Begin
    try
      If Centered then
        FGuiComponent.RenderToArea(-Width / 2,-Height / 2,Width,Height, FRenderStatus, FReBuildGui)
      else
        FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in TGLCustomControl InternalRender function');
    end;
    X1 := FRenderStatus[GLAlCenter].X1;
    X2 := FRenderStatus[GLAlCenter].X2;
    Y1 := -FRenderStatus[GLAlCenter].Y2;
    Y2 := -FRenderStatus[GLAlCenter].Y1;
  End else
  Begin
    If Centered then
    Begin
      X2 := Width / 2;
      Y1 := -Height / 2;
      X1 := -X2;
      Y2 := -Y1;
    End else
    Begin
      X2 := Width;
      Y2 := -Height;
      X1 := 0;
      Y1 := 0;
    End;
  End;

  GuiLayout.Material.UnApply(rci);
  Material.Apply(rci);
  glBegin(GL_QUADS);
    glTexCoord2f( 0, 0);
    glVertex2f(X1,Y2);

    glTexCoord2f( 0, -FYTexCoord);
    glVertex2f(X1,Y1);

    glTexCoord2f( FXTexCoord, -FYTexCoord);
    glVertex2f(X2,Y1);

    glTexCoord2f( FXTexCoord, 0);
    glVertex2f(X2,Y2);
  glEnd();

  Material.UnApply(rci);
  GuiLayout.Material.Apply(rci);
End;

procedure   TGLCustomControl.SetMaterial(AMaterial : TGLMaterial);

Begin
  FMaterial.Assign(AMaterial);
End;

Procedure   TGLPopupMenu.SetFocused(Value :Boolean);

Begin
  inherited;
  if not (csDesigning in ComponentState) then
  If not FFocused then Visible := False;
End;

Procedure   TGLPopupMenu.SetMenuItems(Value :TStrings);

Begin
  FMenuItems.Assign(Value);
  NotifyChange(Self);
End;

Procedure TGLPopupMenu.SetMarginSize(const val : Single);

Begin
  If FMarginSize <> val then
  Begin
    FMarginSize := val;
    NotifyChange(Self);
  End;
End;

Procedure TGLPopupMenu.SetSelIndex(const val : Integer);

Begin
  If FSelIndex <> val then
  Begin
    FSelIndex := val;
    NotifyChange(Self);
  End;
End;

procedure TGLPopupMenu.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);
Var
  ClickIndex : Integer;
  Tx : Single;
  Ty : Single;

Begin
  Tx := X-Left;
  Ty := Y-Top;
  If Button = mbLeft then
  If IsInRect(fRenderStatus[glAlCenter],Tx,Ty) then
  If Assigned(BitmapFont) then
  Begin
    ClickIndex := Round(Int((Ty-fRenderStatus[glAlCenter].y1) / BitmapFont.CharHeight));
    If (ClickIndex >= 0) and (ClickIndex < FMenuItems.Count) then
    Begin
      if Assigned(OnClick) then OnClick(Self,ClickIndex,FMenuItems[ClickIndex]);
      Visible := False;
    End;
  End;
End;

procedure TGLPopupMenu.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
Var
  Tx : Single;
  Ty : Single;
Begin
  Tx := X-Left;
  Ty := Y-Top;
  If IsInRect(fRenderStatus[glAlCenter],Tx,Ty) then
  If Assigned(BitmapFont) then
  Begin
    SelIndex := Round(Int((Ty-fRenderStatus[glAlCenter].y1) / BitmapFont.CharHeight));
  End;
End;

Procedure TGLPopupMenu.OnStringListChange(Sender : TObject);

Var
  CenterHeight : Single;
  TextHeight   : Single;
Begin
  If not FReBuildGui then
  Begin
    If Assigned(BitmapFont) then
    With FRenderStatus[GLalCenter] do
    Begin
      CenterHeight := Y2-Y1;
      CenterHeight := Round(CenterHeight+0.499);
      TextHeight   := BitmapFont.CharHeight*FMenuItems.Count;
      If CenterHeight <> TextHeight then // allways round up!
      Begin
        Height := Height + TextHeight-CenterHeight;
      End;
    End;
  End;
End;

Constructor TGLPopupMenu.Create(AOwner : TComponent);
Begin
  inherited;
  FOnClick   := Nil;
  FMenuItems := TStringList.Create;
  (FMenuItems as TStringList).OnChange := OnStringListChange;
  FSelIndex  := 0;
  NewHeight := -1;
End;

Destructor  TGLPopupMenu.Destroy;
Begin
  inherited;
  FMenuItems.Free;
End;

Procedure   TGLPopupMenu.PopUp(Px,Py : Integer);
Begin
  Position.X := PX;
  Position.Y := PY;
  Visible := True;
  SetFocus;
  RootControl.ActiveControl := Self;
End;

procedure   TGLPopupMenu.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  CenterHeight : Single;
  TextHeight : Single;
  YPos       : Single;
  XPos       : Single;
  XC         : Integer;
  changedHeight: single;
Begin
  If Assigned(FGuiComponent) then
  Begin
    try
      If NewHeight <> -1 then
        FGuiComponent.RenderToArea(0,0,Width,NewHeight, FRenderStatus, FReBuildGui)
      else
        FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in GuiComponents InternalRender function');
    end;
  End;
  If Assigned(BitmapFont) and (FMenuItems.Count > 0) then
  With FRenderStatus[GLalCenter] do
  Begin
    CenterHeight := Y2-Y1;
    CenterHeight := Round(CenterHeight+0.499);
    TextHeight   := BitmapFont.CharHeight*FMenuItems.Count;
    If CenterHeight <> TextHeight then // allways round up!
    Begin
      changedHeight := Height + TextHeight-CenterHeight;
      if changedHeight <> newHeight then begin
        newHeight:= changedHeight;
        InternalRender(rci,RenderSelf,RenderChildren);
      end;
    End else
    Begin
      YPos := -Y1;
      XPos := X1+MarginSize;
      For XC := 0 to FMenuItems.count-1 do
      Begin
        If FSelIndex = XC then
          WriteTextAt(rci, XPos,YPos,FMenuItems[XC],FFocusedColor)
        else
          WriteTextAt(rci, XPos,YPos,FMenuItems[XC],FDefaultColor);
        YPos := YPos - BitmapFont.CharHeight;
      End;
    End;
  End;
End;

procedure TGLPopupMenu.DoRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
  inherited;
  // to avoid gui render-block deadlock!
  If NewHeight <> -1 then
  Begin
    Height := NewHeight;
    NewHeight := -1;
  End;
End;

function TGLPopupMenu.MouseDown(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer): Boolean;
begin
  Result := inherited MouseDown(Sender, Button, Shift, X, Y);

  If (not Result) and (RootControl.ActiveControl = Self) then
  Begin
    RootControl.ActiveControl := Nil;
    NextControl;
  End;
end;

procedure TGLForm.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Var
  CanMove : Boolean;
  YHere : TGLFloat;

Begin
  YHere := Y - Position.Y;
  If YHere < FRenderStatus[GLALTop].Y2 then
  Begin
    If Button = mbLeft then
    Begin
{      If contains(Width-22,Width-6,XHere) and contains(8,24,YHere) then
      Begin
        Close;
      End else{}
      Begin
        CanMove := True;
        If Assigned(FOnCanMove) then FOnCanMove(Self,CanMove);
        If CanMove then
        Begin
          OldX := X;
          OldY := Y;
          Moving := True;
          If Parent is TGLFocusControl then
          (Parent as TGLFocusControl).ActiveControl := Self;
        End;
      End;
    End;
  End else inherited;
End;

procedure TGLForm.InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Begin
  If (Button = mbLeft) and Moving then
  Begin
    Moving := False;
    If Parent is TGLFocusControl then
    (Parent as TGLFocusControl).ActiveControl := Nil;
    Exit;
  End;

  If Y - Position.Y < 27 then
  Begin
  End else inherited;
End;

procedure TGLForm.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

Var
  XRel, YRel : Single;


Begin
  If Moving then
  Begin
    If (X <> OldX) or (Y <> OldY) then
    Begin
      XRel := X - OldX;
      YRel := Y - OldY;

      XRel := XRel + Left;
      YRel := YRel + Top;
      If Assigned(OnMoving) then OnMoving(Self,XRel,YRel);
      XRel := XRel - Left;
      YRel := YRel - Top;

      MoveGUI(XRel,YRel);
      OldX := X;
      OldY := Y;

    End;
  End else
  If Y - Position.Y < 27 then
  Begin

  End else inherited;
End;

Function  TGLForm.GetTitleColor : TDelphiColor;

Begin
  Result := ConvertColorVector(FTitleColor);
End;

procedure TGLForm.SetTitleColor(value : TDelphiColor);

Begin
  FTitleColor := ConvertWinColor(value);
  GUIRedraw := True;
End;

Constructor TGLForm.Create(AOwner : TComponent);

Begin
  inherited;
  FTitleOffset := 2;
End;

Procedure   TGLForm.Close;

Var
  HowClose : TGLFormCloseOptions;

Begin
  HowClose := co_hide;
  If Assigned(FOnCanClose) then FOnCanClose(Self,HowClose);
  Case HowClose of
    co_hide   : Visible := False;
    co_ignore : ;
    co_Destroy : Free;
  End;
End;

procedure TGLForm.NotifyShow;

Begin
  inherited;
  if Assigned(FOnShow) then FOnShow(Self);
End;

procedure TGLForm.NotifyHide;

Begin
  inherited;
  if Assigned(FOnHide) then FOnHide(Self);
End;

Function  TGLForm.MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (Button = mbLeft) and (Moving) then
  Begin
    Result := True;
    InternalMouseUp(Shift,Button,X,Y);
  End else Result := Inherited MouseUp(Sender,Button,Shift,X,Y);
End;

Function  TGLForm.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (Moving) then
  Begin
    Result := True;
    InternalMouseMove(Shift,X,Y);
  End else Result := Inherited MouseMove(Sender,Shift,X,Y);
End;

procedure TGLForm.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
var
  ATitleColor: TColorVector;  
Begin
  If Assigned(FGuiComponent) then
  Begin
    FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);

    ATitleColor := FTitleColor;
    ATitleColor[3] := AlphaChannel;

    WriteTextAt(rci, ((FRenderStatus[GLAlTop].X2+FRenderStatus[GLAlTop].X1-BitmapFont.CalcStringWidth(Caption))*0.5),-((FRenderStatus[GLAlTop].Y2+FRenderStatus[GLAlTop].Y1-GetFontHeight)*0.5)+TitleOffset,Caption,ATitleColor);
  End;
End;

Procedure TGLCheckBox.SetChecked(NewChecked : Boolean);

Begin
  If NewChecked <> FChecked then
  begin
    BlockRender;
    try
      If NewChecked then
      If Group >= 0 then
      UnpressGroup(FindFirstGui,Group);

      FChecked := NewChecked;
    finally
      UnBlockRender;
    end;

    NotifyChange(Self);
    if Assigned(FOnChange) then FOnChange(Self);
  End;
End;

procedure TGLCheckBox.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);
Begin
  Checked := Not Checked;
  inherited;
End;

procedure TGLCheckBox.InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Begin
  inherited;
End;

procedure TGLCheckBox.SetGuiLayoutNameChecked(newName : TGLGuiComponentName);

Begin
  If FGuiLayoutNameChecked <> NewName then
  Begin
    FGuiCheckedComponent := Nil;
    FGuiLayoutNameChecked := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiCheckedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLCheckBox.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  FGuiCheckedComponent := Nil;
  inherited;
  If Assigned(FGuiLayout) then
  Begin
    FGuiCheckedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

Procedure TGLCheckBox.SetGroup(const val : Integer);

Begin
  FGroup := val;
  If Checked then
  Begin
    BlockRender;
    FChecked := False;
    UnpressGroup(FindFirstGui,val);
    FChecked := true;
    UnBlockRender;
  End;
End;

constructor TGLCheckBox.Create(AOwner: TComponent);

Begin
  inherited;
  FChecked := False;
  FGroup := -1;
End;

procedure TGLCheckBox.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);
Begin
  If Checked then
  Begin
    If Assigned(FGuiCheckedComponent) then
    Begin
      FGuiCheckedComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  End else
  Begin
    If Assigned(FGuiComponent) then
    Begin
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  End;
End;

procedure TGLCheckBox.NotifyChange(Sender : TObject);

Begin
  If Sender = FGuiLayout then
  Begin
    If (FGuiLayoutNameChecked <> '') and (GuiLayout <> Nil) then
    Begin
      BlockRender;
      FGuiCheckedComponent := GuiLayout.GuiComponents.FindItem(FGuiLayoutNameChecked);
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    End else
    Begin
      BlockRender;
      FGuiCheckedComponent := Nil;
      ReBuildGui := True;
      GUIRedraw := True;
      UnBlockRender;
    End;
  End;
  inherited;
End;


Procedure TGLButton.SetPressed(NewPressed : Boolean);

Begin
  If FPressed <> NewPressed then
  begin
    BlockRender;
    try
      If NewPressed then
      If Group >= 0 then
      UnpressGroup(RootControl,Group);

      FPressed := NewPressed;
    finally
      UnBlockRender;
    end;

    If FPressed then
    if Assigned(FOnButtonClick) then FOnButtonClick(Self);
    
    NotifyChange(Self);
  End;
End;

procedure TGLButton.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);
Begin
  SetFocus;
  inherited;
  If Button = mbLeft then
  If AllowUp then
    Pressed := not Pressed
  else
    Pressed := True;
End;

procedure TGLButton.InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Begin
  If (Button = mbLeft) and (Group < 0) then
  Pressed := False;
  inherited;
End;

Procedure TGLButton.InternalKeyDown(var Key: Word; Shift: TShiftState);

Begin
  inherited;
  If Key = glKey_SPACE then
  Begin
    Pressed := True;
  End;
  If Key = glKey_RETURN then
  Begin
    Pressed := True;
  End;
End;

Procedure TGLButton.InternalKeyUp(var Key: Word; Shift: TShiftState);

Begin
  If ((Key = glKey_SPACE) or (Key = glKey_RETURN)) and (Group < 0) then
  Begin
    Pressed := False;
  End;
  inherited;
End;

Procedure TGLButton.SetFocused(Value :Boolean);
Begin
  inherited;
  If (not FFocused) and (Group < 0) then
  Pressed := False;
End;

procedure TGLButton.SetGuiLayoutNamePressed(newName : TGLGuiComponentName);

Begin
  If FGuiLayoutNamePressed <> NewName then
  Begin
    FGuiPressedComponent := Nil;
    FGuiLayoutNamePressed := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiPressedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLButton.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  FGuiPressedComponent := Nil;
  inherited;
  If Assigned(FGuiLayout) then
  Begin
    FGuiPressedComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutNamePressed);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

procedure TGLButton.SetBitBtn(AValue: TGLMaterial);

Begin
   FBitBtn.Assign(AValue);
   NotifyChange(Self);
end;

procedure TGLButton.DestroyHandle;
begin
   inherited;
   FBitBtn.DestroyHandles;
end;

Procedure TGLButton.SetGroup(const val : Integer);

Begin
  FGroup := val;
  If Pressed then
  Begin
    BlockRender;
    FPressed := False;
    UnpressGroup(RootControl,Group);
    FPressed := True;
    UnBlockRender;
  End;
end;

Procedure TGLButton.SetLogicWidth(const val : single);

Begin
  FLogicWidth := val;
  NotifyChange(Self);
End;

Procedure TGLButton.SetLogicHeight(const val : single);

Begin
  FLogicHeight := val;
  NotifyChange(Self);
End;

Procedure TGLButton.SetXOffset(const val : single);

Begin
  FXOffSet := val;
  NotifyChange(Self);
End;

Procedure TGLButton.SetYOffset(const val : single);

Begin
  FYOffSet := val;
  NotifyChange(Self);
End;


constructor TGLButton.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FBitBtn:=TGLMaterial.Create(Self);
   FGroup := -1;
   FPressed := False;
end;

destructor TGLButton.Destroy;
begin
   inherited Destroy;
   FBitBtn.Free;
end;

procedure TGLButton.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  B : Boolean;
  TexWidth : Integer;
  TexHeight : Integer;
  Material : TGLMaterial;
  LibMaterial : TGLLibMaterial;
  TextColor: TColorVector;

Begin
  if Pressed then
  begin
    If Assigned(FGuiPressedComponent) then
    Begin
      FGuiPressedComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  end else
  begin
    If Assigned(FGuiComponent) then
    Begin
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    End;
  end;

  B := not BitBtn.Texture.Disabled;
  Material := Nil;
  if not B then
  Begin
    if BitBtn.MaterialLibrary <> nil then
    begin

      LibMaterial := BitBtn.MaterialLibrary.Materials.GetLibMaterialByName(BitBtn.LibMaterialName);
      if LibMaterial <> nil then
      Begin
        Material := LibMaterial.Material;
        B := True;
      End;
    End;
  end else
  begin
    Material := BitBtn;
  End;

  if B then
  with FRenderStatus[GLAlCenter] do
  Begin
    BitBtn.Apply(rci);

    TexWidth  := Material.Texture.TexWidth;
    If TexWidth = 0 then
    TexWidth := Material.Texture.Image.Width;

    TexHeight := Material.Texture.TexHeight;
    If TexHeight = 0 then
    TexHeight := Material.Texture.Image.Height;
    
    glBegin(GL_QUADS);

{    glTexCoord2f(0,1);
    glVertex2f(X1, -Y1);

    glTexCoord2f(0,0);
    glVertex2f(X1, -Y2);

    glTexCoord2f(1,0);
    glVertex2f(X2, -Y2);

    glTexCoord2f(1,1);
    glVertex2f(X2, -Y1);{}

    glTexCoord2f(0,0);
    glVertex2f(X1-XOffSet, -Y1+YOffSet);

    glTexCoord2f(0,-(LogicHeight-1)/TexHeight);
    glVertex2f(X1-XOffSet, -Y1+YOffset-LogicHeight+1);

    glTexCoord2f((LogicWidth-1)/TexWidth,-(LogicHeight-1)/TexHeight);
    glVertex2f(X1-XOffSet+LogicWidth-1, -Y1+YOffset-LogicHeight+1);

    glTexCoord2f((LogicWidth-1)/TexWidth,0);
    glVertex2f(X1-XOffSet+LogicWidth-1, -Y1+YOffSet);

    glEnd();
    BitBtn.UnApply(rci);
  End;

   If Assigned(BitmapFont) then
   Begin

     If FFocused then
     Begin
       TextColor := FFocusedColor;
     End else
     Begin
       TextColor := FDefaultColor;
     End;
     TextColor[3] := AlphaChannel;

     WriteTextAt(rci, FRenderStatus[GLALCenter].X1,
                      FRenderStatus[GLALCenter].Y1,
                      FRenderStatus[GLALCenter].X2,
                      FRenderStatus[GLALCenter].Y2,
                      Caption,
                      TextColor);
   End;
End;

procedure TGLEdit.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);
Begin
  SetFocus;
  inherited;
End;

Procedure TGLEdit.InternalKeyPress(var Key: Char);
Begin
  inherited;

  Case Key of
    #8 :
    Begin
      If FSelStart > 1 then
      Begin
        system.Delete(FCaption,FSelStart-1,1);
        Dec(FSelStart);
        GUIRedraw := True;
      End;
    End;
    else
    Begin
      If Key >= #32 then
      Begin
        system.Insert(Key,FCaption,SelStart);
        inc(FSelStart);
        GUIRedraw := True;
      End;
    End;
  End;
End;

Procedure TGLEdit.InternalKeyDown(var Key: Word; Shift: TShiftState);

Begin
  inherited;

  Case Key of
    glKey_DELETE :
    Begin
      If FSelStart <= Length(Caption) then
      Begin
        System.Delete(FCaption,FSelStart,1);
        GUIRedraw := True;
      End;
    End;
    glKey_LEFT   :
    Begin
      If FSelStart > 1 then
      Begin
        Dec(FSelStart);
        GUIRedraw := True;
      End;
    End;
    glKey_RIGHT   :
    Begin
      If FSelStart < Length(Caption)+1 then
      Begin
        Inc(FSelStart);
        GUIRedraw := True;
      End;
    End;
    glKey_HOME   :
    Begin
      If FSelStart > 1 then
      Begin
        FSelStart := 1;
        GUIRedraw := True;
      End;
    End;
    glKey_END   :
    Begin
      If FSelStart < Length(Caption)+1 then
      Begin
        FSelStart := Length(Caption)+1;
        GUIRedraw := True;
      End;
    End;
  End;

End;

Procedure TGLEdit.InternalKeyUp(var Key: Word; Shift: TShiftState);

Begin
  inherited;
End;

Procedure TGLEdit.SetFocused(Value :Boolean);

Begin
  Inherited;
  If Value then
  SelStart := Length(Caption)+1;
End;

Procedure TGLEdit.SetSelStart(const Value : Integer);

Begin
  FSelStart := Value;
  GUIRedraw := True;
End;

Procedure TGLEdit.SetEditChar(const Value : String);

Begin
  FEditChar := Value;
  GUIRedraw := True;
End;

Constructor TGLEdit.Create(AOwner : TComponent);

Begin
  inherited;
  FEditChar := '*';
End;

procedure TGLEdit.InternalRender(var rci : TRenderContextInfo; renderSelf,
renderChildren : Boolean);
var
  Tekst : String;
  pBig  : Integer;
Begin
// Renders the background
  If Assigned(FGuiComponent) then
  Begin
    FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
  End;
// Renders the text
  If Assigned(FBitmapFont) then
  Begin
    Tekst := Caption;

    if FFocused then
      begin
        // First put in the edit character where it should be.
        system.insert(FEditChar,Tekst,SelStart);
        // Next figure out if the string is too long.
        if FBitmapFont.CalcStringWidth(Tekst) > Width - 2 then
          begin
          // if it is then we need to check to see where SelStart is
          if SelStart >= Length(Tekst) -1 then
            begin
            // SelStart is within close proximity of the end of the string
            // Calculate the % of text that we can use and return it against the length of the string.
              pBig := Trunc(Int(((Width - 2) / FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
              dec(pBig);
              Tekst:= Copy(Tekst, Length(Tekst) - pBig + 1, pBig);
            end
          else
            begin
            // SelStart is within close proximity of the end of the string
            // Calculate the % of text that we can use and return it against the length of the string.
              pBig := Trunc(Int(((Width - 2) / FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
              dec(pBig);
            if SelStart + pBig < Length(Tekst) then
              Tekst := Copy(Tekst, SelStart, pBig)
            else
              Tekst:= Copy(Tekst, Length(Tekst) - pBig + 1, pBig);
            end;
          end;
      end
    else { if FFocused then }
      if FBitmapFont.CalcStringWidth(Tekst) > Width - 2 then
        begin
        // The while loop should never execute more then once, but just in case its here.
          while FBitmapFont.CalcStringWidth(Tekst) > Width - 2 do
            begin
            // Calculate the % of text that we can use and return it against the length of the string.
              pBig := Trunc(Int(((Width - 2) / FBitmapFont.CalcStringWidth(Tekst)) * Length(Tekst)));
              Tekst:= Copy(Tekst, 1, pBig);
            end;
        end;

    If FFocused then
      Begin
        WriteTextAt(rci, FRenderStatus[GLAlLeft].X1,FRenderStatus[GLAlCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Tekst,FFocusedColor);
      End
    else
      Begin
        WriteTextAt(rci, FRenderStatus[GLAlLeft].X1,FRenderStatus[GLAlCenter].Y1,FRenderStatus[GLALCenter].X2,FRenderStatus[GLALCenter].Y2,Tekst,FDefaultColor);
      End;
  End;
End;



constructor TGLLabel.Create(AOwner: TComponent);
begin
  inherited;
  FTextLayout := tlCenter;
end;

procedure TGLLabel.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  TekstPos : TVector;
  Tekst : String;
  TextColor: TColorVector;
Begin
  If Assigned(BitmapFont) then
  Begin
    case Alignment of
      taLeftJustify  : begin
                         TekstPos[0] := 0;
                       end;
      taCenter       : begin
                         TekstPos[0] := Width / 2;
                       end;
      taRightJustify : begin
                         TekstPos[0] := Width;
                       end;
    end;

    case TextLayout of
      tlTop    : begin
                   TekstPos[1] := 0;
                 end;
      tlCenter : begin
                   TekstPos[1] := Round(-Height / 2);
                 end;
      tlBottom : begin
                   TekstPos[1] := -Height;
                 end;
    end;

    TekstPos[2] := 0;
    TekstPos[3] := 0;

    Tekst := Caption;

    TextColor := FDefaultColor;
    TextColor[3] := AlphaChannel;

    BitmapFont.RenderString(rci, Tekst, FAlignment, FTextLayout, TextColor, @TekstPos);
  End;
End;

procedure TGLLabel.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    NotifyChange(Self);
  end;
end;

procedure TGLLabel.SetTextLayout(const Value: TGLTextLayout);
begin
  if FTextLayout <> Value then
  begin
    FTextLayout := Value;
    NotifyChange(Self);
  end;
end;


procedure TGLAdvancedLabel.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Begin
   If Assigned(BitmapFont) then
   Begin
     If Focused then
     Begin
       WriteTextAt(rci, 8,-((Height-GetFontHeight) / 2)+1,Caption,FFocusedColor);
     End else
     Begin
       WriteTextAt(rci, 8,-((Height-GetFontHeight) / 2)+1,Caption,FDefaultColor);
     End;
   End;
End;

Procedure TGLScrollbar.SetMin(const val      : Single);
Begin
  If FMin <> val then
  Begin
    FMin := val;
    If FPos < FMin then Pos := FMin;
    NotifyChange(Self);
  End;
End;

Procedure TGLScrollbar.SetMax(const val      : Single);
Begin
  If FMax <> val then
  Begin
    FMax := val;
    If FMax < FMin then FMax := FMin;
    If FPos > (FMax-FPageSize+1) then Pos := (FMax-FPageSize+1);
    NotifyChange(Self);
  End;
End;

Procedure TGLScrollbar.SetPos(const val : Single);
Begin
  If FPos <> val then
  Begin
    FPos := val;
    If FPos < FMin then FPos := FMin;
    If FPos > (FMax-FPageSize+1) then FPos := (FMax-FPageSize+1);

    NotifyChange(Self);
    If Assigned(FOnChange) then FOnChange(Self);
  End;
End;

Procedure TGLScrollbar.SetPageSize(const val : Single);

Begin
  If FPageSize <> val then
  Begin
    FPageSize := val;
    If FPos > (FMax-FPageSize+1) then Pos := (FMax-FPageSize+1);
    NotifyChange(Self);
  End;
End;

Procedure TGLScrollbar.SetHorizontal(const val : Boolean);

Begin
  If FHorizontal <> val then
  Begin
    FHorizontal := val;
    NotifyChange(Self);
  End;
End;

procedure TGLScrollbar.SetGuiLayoutKnobName(newName : TGLGuiComponentName);

Begin
  if newName <> FGuiLayoutKnobName then
  Begin
    FGuiKnobComponent := Nil;
    FGuiLayoutKnobName := NewName;
    If Assigned(FGuiLayout) then
    Begin
      FGuiKnobComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutKnobName);
      FReBuildGui := True;
      GUIRedraw := True;
    End;
  End;
End;

procedure TGLScrollbar.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  FGuiKnobComponent := Nil;
  inherited;
  If Assigned(FGuiLayout) then
  Begin
    FGuiKnobComponent := FGuiLayout.GuiComponents.FindItem(FGuiLayoutKnobName);
    FReBuildGui := True;
    GUIRedraw := True;
  End;
End;

Function  TGLScrollbar.GetScrollPosY(ScrollPos : Single) : Single;
Begin
  With FRenderStatus[GLAlCenter] do
  Begin
    Result := (ScrollPos-FMin)/(FMax-FMin) * (Y2-Y1) + Y1;
  End;
End;

Function  TGLScrollbar.GetYScrollPos(Y : Single) : Single;
Begin
  With FRenderStatus[GLAlCenter] do
  Begin
    Result := (Y-Y1)/(Y2-Y1)*(FMax-FMin)+FMin;
  End;
End;

Function  TGLScrollbar.GetScrollPosX(ScrollPos : Single) : Single;
Begin
  With FRenderStatus[GLAlCenter] do
  Begin
    Result := (ScrollPos-FMin)/(FMax-FMin) * (X2-X1) + X1;
  End;
End;

Function  TGLScrollbar.GetXScrollPos(X : Single) : Single;
Begin
  With FRenderStatus[GLAlCenter] do
  Begin
    Result := (X-X1)/(X2-X1)*(FMax-FMin)+FMin;
  End;
End;

procedure TGLScrollbar.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Var
  Tx, Ty : Single;

Begin
  if Button = mbLeft then
  Begin
    Tx := x - left;
    Ty := y - top;
    // is in mid area ?
    If IsInRect(FRenderStatus[GLAlCenter],Tx,Ty) then
    Begin
      If FHorizontal then
      Begin
        Tx := GetxScrollPos(Tx);
        If Tx < FPos then PageUp
        else
        If Tx > FPos+FPageSize-1 then PageDown
        else
        Begin
          fScrolling := True;
          FScrollOffs := Tx-FPos;
          RootControl.ActiveControl := Self;
        End;
      End else
      Begin
        Ty := GetYScrollPos(Ty);
        If Ty < FPos then PageUp
        else
        If Ty > FPos+FPageSize-1 then PageDown
        else
        Begin
          fScrolling := True;
          FScrollOffs := Ty-FPos;
          RootControl.ActiveControl := Self;
        End;
      end;
    End else
    Begin
      // if not, is at end buttons ?
      If horizontal then
      Begin
        If IsInRect(FRenderStatus[GLAlLeft],Tx,Ty) then StepUp;
        If IsInRect(FRenderStatus[GLAlRight],Tx,Ty) then StepDown;
      End else
      Begin
        If IsInRect(FRenderStatus[GLAlTop],Tx,Ty) then StepUp;
        If IsInRect(FRenderStatus[GLAlBottom],Tx,Ty) then StepDown;
      End;
    End;
  End;
  inherited;
End;

procedure TGLScrollbar.InternalMouseUp(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);
Begin
  If fScrolling then
  Begin
    fScrolling := False;
    RootControl.ActiveControl := Nil;
  End;

  inherited;
End;

procedure TGLScrollbar.InternalMouseMove(Shift: TShiftState; X, Y: Integer);

Var
  Tx : Single;
  Ty : Single;
Begin
  If fScrolling then
  If FHorizontal then
  Begin
    Tx := GetXScrollPos(x - left)-FScrollOffs;
    Pos := Round(Tx);
  End else
  Begin
    Ty := GetYScrollPos(y - top)-FScrollOffs;
    Pos := Round(Ty);
  End;

  inherited;
End;

constructor TGLScrollbar.Create(AOwner : TComponent);

Begin
  inherited;
  FGuiKnobComponent := Nil;
  FMin := 1;
  FMax := 10;
  FPos := 1;
  FStep := 1;
  FPageSize := 3;
  FOnChange := Nil;
  FGuiLayoutKnobName := '';
  FScrollOffs   := 0;
  FScrolling    := False;
  FHorizontal   := False;
End;

Procedure TGLScrollbar.StepUp;

Begin
  Pos := Pos - FStep;
End;

Procedure TGLScrollbar.StepDown;
Begin
  Pos := Pos + FStep;
End;

Procedure TGLScrollbar.PageUp;
Begin
  Pos := Pos - FPageSize;
End;

Procedure TGLScrollbar.PageDown;
Begin
  Pos := Pos + FPageSize;
End;

Function  TGLScrollbar.MouseUp(Sender: TObject; Button: TGLMouseButton; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (Button = mbLeft) and (FScrolling) then
  Begin
    Result := True;
    InternalMouseUp(Shift,Button,X,Y);
  End else Result := Inherited MouseUp(Sender,Button,Shift,X,Y);
End;

Function  TGLScrollbar.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer) : Boolean;

Begin
  If (FScrolling) then
  Begin
    Result := True;
    InternalMouseMove(Shift,X,Y);
  End else Result := Inherited MouseMove(Sender,Shift,X,Y);
End;

procedure TGLScrollbar.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Var
  Start, Size : Integer;
Begin
  If Assigned(FGuiComponent) then
  Begin
    try
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in GuiComponents InternalRender function');
    end;
  End;
  If Assigned(FGuiKnobComponent) then
  Begin
    try
      With FRenderStatus[GLAlCenter] do
      Begin
        If FHorizontal then
        Begin
           Start := Round(GetScrollPosX(FPos));
           If FPageSize+FPos > FMax+1 then
             Size  := Round(GetScrollPosX(FMax)-X1)
           else
             Size  := Round(GetScrollPosX(FPageSize)-X1);

           FGuiKnobComponent.RenderToArea(Start,Y1,Start+Size,Y2, FKnobRenderStatus, True);
//           Tag := start;
//           tagfloat := size;
        end else
        Begin
           Start := Round(GetScrollPosY(FPos));
           If FPageSize+FPos > FMax+1 then
             Size  := Round(GetScrollPosY(FMax)-Y1)
           else
             Size  := Round(GetScrollPosY(FPageSize)-Y1);
           FGuiKnobComponent.RenderToArea(X1,Start,X2,Start+Size, FKnobRenderStatus, True);
//           Tag := start;
//           tagfloat := size;
        end;
      End;
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in GuiComponents InternalRender function');
    end;
  End;
End;

Function  TGLStringGrid.GetCell(X,Y : Integer; out oCol,oRow : Integer) : Boolean;

Var
  ClientRect : TRectangle;
  XPos : Integer;
  YPos : Integer;
  XC, YC : Integer;

Begin
  Result := False;
  If Assigned(BitmapFont) then
  Begin
    If Assigned(FGuiComponent) then
    Begin
      ClientRect.Left   := Round(FRenderStatus[GLAlCenter].X1);
      ClientRect.Top    := Round(FRenderStatus[GLAlCenter].Y1);
      ClientRect.Width  := Round(FRenderStatus[GLAlCenter].X2);
      ClientRect.Height := Round(FRenderStatus[GLAlCenter].Y2);
    End else
    Begin
      ClientRect.Left   := 0;
      ClientRect.Top    := 0;
      ClientRect.Width  := Round(Width);
      ClientRect.Height := Round(Height);
    End;

    YPos := ClientRect.Top;
    If FDrawHeader then
    YPos := YPos + RowHeight;
    XPos := ClientRect.Left;

    If y < YPos then Exit;
    If x < XPos then Exit;

    XPos := XPos+MarginSize;

    For XC := 0 to Columns.Count-1 do
    Begin
      XPos := XPos + Integer(Columns.Objects[XC]);

      If x > XPos then continue;

      For YC := 0 to RowCount-1 do
      Begin
        YPos := YPos + RowHeight;
        If y < YPos then
        Begin
          Result := True;
          If Assigned(Scrollbar) then
            oRow := YC+Round(Scrollbar.Pos)-1
          else
            oRow := YC;

          oCol := XC;
          Exit;
        End;
      End;
    End;
  End;
End;

procedure TGLStringGrid.InternalMouseDown(Shift: TShiftState; Button: TGLMouseButton; X, Y: Integer);

Var
  tRow, tCol : Integer;
Begin
  SetFocus;
  If GetCell(Round(X-Left),Round(Y-Top), tCol, tRow) then
  Begin
    SelCol := tCol;
    SelRow := tRow;
  End;
  inherited;
End;

Procedure TGLStringGrid.SetColumns(const val : TStrings);

Var
  XC : Integer;
Begin
  FColumns.Assign(val);
  For XC := 0 to Columns.Count-1 do
  Columns.Objects[XC] := TObject(ColumnSize);
end;

Function  TGLStringGrid.GetRow(index : Integer) : TStringList;

Begin
  if (index >= 0) and (index < FRows.Count) then
    Result := TStringList(FRows[index])
  else
    Result := Nil;
End;

Procedure TGLStringGrid.SetRow(index : Integer; const val : TStringList);

Begin
  if (index >= 0) then
  begin
    If (index >= RowCount) then
    RowCount := index+1;

    TStringList(FRows[index]).Assign(val);
  End;
End;

Function  TGLStringGrid.GetRowCount : Integer;

Begin
  Result := FRows.count;
End;

Procedure TGLStringGrid.SetRowCount(const val : Integer);

Var
  XC : Integer;

Begin
  XC := FRows.count;
  If val <> XC then
  Begin
    If val > XC then
    begin
      FRows.count := val;
      For XC := XC to val-1 do
      Begin
        FRows[XC] := TStringList.Create;
        TStringList(FRows[XC]).OnChange := OnStringListChange;
      End;
    End else
    Begin
      For XC := XC-1 downto val do
      Begin
        TStringList(FRows[XC]).Free;
      End;
      FRows.count := val;
    End;
    If Assigned(Scrollbar) then
    Scrollbar.FMax := FRows.Count;
    NotifyChange(Self);
  End;
End;

Procedure TGLStringGrid.SetSelCol(const val : Integer);
Begin
  If FSelCol <> Val then
  Begin
    FSelCol := Val;
    NotifyChange(Self);
  End;
End;

Procedure TGLStringGrid.SetSelRow(const val : Integer);
Begin
  If FSelRow <> Val then
  Begin
    FSelRow := Val;
    NotifyChange(Self);
  End;
End;

Procedure TGLStringGrid.SetRowSelect(const val : Boolean);

Begin
  FRowSelect := Val;
  NotifyChange(Self);
End;

Procedure TGLStringGrid.SetDrawHeader(const val : Boolean);

Begin
  FDrawHeader := Val;
  NotifyChange(Self);
End;

Function  TGLStringGrid.GetHeaderColor : TDelphiColor;

Begin
  Result := ConvertColorVector(FHeaderColor);
End;

Procedure TGLStringGrid.SetHeaderColor(const val : TDelphiColor);

Begin
  FHeaderColor := ConvertWinColor(val);
  GUIRedraw := True;
End;

Procedure TGLStringGrid.SetMarginSize(const val : Integer);

Begin
  If FMarginSize <> val then
  Begin
    FMarginSize := val;
    GUIRedraw := True;
  End;
End;

Procedure TGLStringGrid.SetColumnSize(const val : Integer);

Var
  XC : Integer;

Begin
  If FColumnSize <> val then
  Begin
    FColumnSize := val;
    For XC := 0 to Columns.Count-1 do
    Columns.Objects[XC] := TObject(ColumnSize);
    GUIRedraw := True;
  End;
End;

Procedure TGLStringGrid.SetRowHeight(const val : Integer);

Begin
  If FRowHeight <> val then
  begin
    FRowHeight := val;
    GUIRedraw := True;
  End;
End;

Procedure TGLStringGrid.SetScrollbar(const val : TGLScrollbar);

Begin
  If FScrollbar <> Val then
  Begin
    If Assigned(FScrollbar) then FScrollbar.RemoveFreeNotification(Self);
    FScrollbar := Val;
    If Assigned(FScrollbar) then FScrollbar.FreeNotification(Self);
  End;
End;

procedure TGLStringGrid.SetGuiLayout(NewGui : TGLGuiLayout);

Begin
  inherited;
  If Assigned(Scrollbar) then
  If Scrollbar.GuiLayout <> Nil then
    Scrollbar.GuiLayout := NewGui;
End;

constructor TGLStringGrid.Create(AOwner : TComponent);

Begin
  inherited;
  FRows := TList.Create;
  FColumns := TStringList.Create;
  TStringList(FColumns).OnChange := OnStringListChange;
  FSelCol := 0;
  FSelRow := 0;
  FRowSelect := True;
  FScrollbar := Nil;
  FDrawHeader := True;
End;

Destructor  TGLStringGrid.Destroy;

Begin
  Scrollbar := Nil;
  inherited;
  Clear;
  FRows.Free;
  FColumns.Free;
End;

Procedure TGLStringGrid.Clear;

Begin
  RowCount := 0;
End;

procedure TGLStringGrid.Notification(AComponent: TComponent; Operation: TOperation);

Begin
  If (AComponent = FScrollbar) and (Operation = opRemove) then
  Begin
    FScrollbar := Nil;
  End;
  inherited;
End;

procedure TGLStringGrid.NotifyChange(Sender : TObject);

Begin
  if Sender = Scrollbar then
  Begin
    ReBuildGui := True;
    GUIRedraw := True;
  End;
  inherited;
End;

procedure TGLStringGrid.InternalRender(var rci : TRenderContextInfo; renderSelf, renderChildren : Boolean);

Function CellSelected(X,Y : Integer) : Boolean;

Begin
  if RowSelect then
    Result := Y = SelRow
  else
    Result := (Y = SelRow) and (x = SelCol);
End;

Function CellText(X,Y : Integer) : String;

Begin
  With Row[y] do
  If (X >= 0) and (X < Count) then
    Result := strings[x]
  else
    Result := '';
End;


Var
  ClientRect : TRectangle;
  XPos : Integer;
  YPos : Integer;
  XC, YC : Integer;
  From, Till : Integer;

Begin
  If Assigned(FGuiComponent) then
  Begin
    try
      FGuiComponent.RenderToArea(0,0,Width,Height, FRenderStatus, FReBuildGui);
      ClientRect.Left   := Round(FRenderStatus[GLAlCenter].X1);
      ClientRect.Top    := Round(FRenderStatus[GLAlCenter].Y1);
      ClientRect.Width  := Round(FRenderStatus[GLAlCenter].X2);
      ClientRect.Height := Round(FRenderStatus[GLAlCenter].Y2);
    except
      on E : Exception do
      GLOKMessageBox(E.Message,'Exception in GuiComponents InternalRender function');
    end;
  End else
  Begin
    ClientRect.Left   := 0;
    ClientRect.Top    := 0;
    ClientRect.Width  := Round(Width);
    ClientRect.Height := Round(Height);
  End;

  If Assigned(BitmapFont) then
  Begin
    XPos := ClientRect.Left+MarginSize;

    If Assigned(Scrollbar) then
    Begin
      Scrollbar.Left := Left+FRenderStatus[GLAlCenter].X2-Scrollbar.Width;
      Scrollbar.Top  := Top +FRenderStatus[GLAlCenter].Y1;
      Scrollbar.Height := FRenderStatus[GLAlCenter].Y2-FRenderStatus[GLAlCenter].Y1;
      XC := (ClientRect.Height - ClientRect.Top);
      If FDrawHeader then
        YC := (XC div RowHeight)-1
      else
        YC := (XC div RowHeight);

      Scrollbar.PageSize := YC;
      From := Round(Scrollbar.pos-1);
      Till := Round(Scrollbar.pageSize+From-1);
      If Till > RowCount-1 then Till := RowCount-1;
    End else
    Begin
      From := 0;
      Till := RowCount-1;
    End;

    For XC := 0 to Columns.Count-1 do
    Begin
      YPos := -ClientRect.Top;
      If FDrawHeader then
      Begin
        WriteTextAt(rci, XPos,YPos,Columns[XC],FHeaderColor);
        YPos := YPos - RowHeight;
      End;
      For YC := From to Till do
      Begin
        If CellSelected(XC,YC) then
          WriteTextAt(rci, XPos,YPos,CellText(XC,YC),FFocusedColor)
        else
          WriteTextAt(rci, XPos,YPos,CellText(XC,YC),FDefaultColor);
        YPos := YPos - RowHeight;
      End;
      XPos := XPos + Integer(Columns.Objects[XC]);
    End;
  End;
End;

Procedure TGLStringGrid.OnStringListChange(Sender : TObject);

Begin
  NotifyChange(Self);
End;
Function TGLStringGrid.Add(Data : Array of String) : Integer;
Var
  XC : Integer;
Begin
  Result := RowCount;
  RowCount := RowCount +1;
  For XC := 0 to Length(Data)-1 do
    Row[Result].Add(Data[XC]);
End;

Function TGLStringGrid.Add(const Data : String) : Integer;
Begin
  Result := Add([Data]);
  If Assigned(Scrollbar) then
  Begin
    If Result > Round(Scrollbar.pageSize+Scrollbar.pos-2) then
      Scrollbar.pos := Result-Scrollbar.pageSize+2;
  End;
End;

procedure TGLStringGrid.SetText(Data : String);

Var
  Posi : Integer;
Begin
  Clear;
  While Data <> '' do
  Begin
    Posi := Pos(#13#10,Data);
    If Posi > 0 then
    Begin
      Add(Copy(Data,1,Posi-1));
      Delete(Data,1,Posi+1);
    End else
    Begin
      Add(Data);
      Data := '';
    End;
  End;
End;


destructor TGLFocusControl.Destroy;
begin
  If Focused then
    RootControl.FocusedControl := Nil;
  inherited;
end;

procedure TGLBaseComponent.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;
  If FDoChangesOnProgress then
    DoChanges;

end;

procedure TGLBaseComponent.SetDoChangesOnProgress(const Value: Boolean);
begin
  FDoChangesOnProgress := Value;
End;

procedure TGLFocusControl.MoveTo(newParent: TGLBaseSceneObject);
begin
  inherited;
  ReGetRootControl;
end;

initialization
   RegisterClasses([TGLBaseControl,TGLPopupMenu,TGLForm,TGLPanel,TGLButton,TGLCheckBox,TGLEdit,TGLLabel,TGLAdvancedLabel, TGLScrollbar, TGLStringGrid, TGLCustomControl]);
end.
