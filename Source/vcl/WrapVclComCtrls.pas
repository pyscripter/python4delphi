(**************************************************************************)
(*  This unit is part of the Python for Delphi (P4D) library              *)
(*  Project home: https://github.com/pyscripter/python4delphi             *)
(*                                                                        *)
(*  Project Maintainer:  PyScripter (pyscripter@gmail.com)                *)
(*  Original Authors:    Dr. Dietmar Budelsky (dbudelsky@web.de)          *)
(*                       Morgan Martinet (https://github.com/mmm-experts) *)
(*  Core developer:      Lucas Belo (lucas.belo@live.com)                 *)
(*  Contributors:        See contributors.md at project home              *)
(*                                                                        *)
(*  LICENCE and Copyright: MIT (see project home)                         *)
(**************************************************************************)

{$I ..\Definition.Inc}

unit WrapVclComCtrls;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  Vcl.Controls,
  Vcl.ComCtrls,
  PythonEngine,
  WrapDelphi,
  WrapDelphiClasses,
  WrapVclControls;

type
  TTabChangingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; var AllowChange: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;
    class function GetTypeInfo: PTypeInfo; override;
  end;

  {$IFNDEF FPC}
  TPyDelphiDateTimePicker = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TDateTimePicker;
    procedure SetDelphiObject(const Value: TDateTimePicker);
  public
    class function  DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TDateTimePicker read GetDelphiObject write SetDelphiObject;
  end;
  {$ENDIF FPC}

  TPyDelphiTabSheet = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TTabSheet;
    procedure SetDelphiObject(const Value: TTabSheet);
  protected
    // Property Getters
    function Get_TabIndex( AContext: Pointer): PPyObject; cdecl;
    function Get_PageControl( AContext: Pointer): PPyObject; cdecl;
    // Property Setters
    function Set_PageControl( AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function  DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets( PythonType: TPythonType ); override;
    // Properties
    property DelphiObject: TTabSheet read GetDelphiObject write SetDelphiObject;
  end;

  {
    Access to the child pages of a TPageControl.Pages collection.
  }
  TPagesAccess = class(TContainerAccess)
  private
    function GetContainer: TPageControl;
  public
    function GetItem(AIndex: Integer): PPyObject; override;
    function GetSize: Integer; override;
    function IndexOf(AValue: PPyObject): Integer; override;

    class function ExpectedContainerClass: TClass; override;
    class function SupportsIndexOf: Boolean; override;
    class function Name: string; override;

    property Container: TPageControl read GetContainer;
  end;

  TPyDelphiPageControl = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TPageControl;
    procedure SetDelphiObject(const Value: TPageControl);
  protected
    // methods
    {$IFNDEF FPC}
    function  IndexOfTabAt_Wrapper(args: PPyObject): PPyObject; cdecl;
    function  GetHitTestInfoAt_Wrapper(args: PPyObject): PPyObject; cdecl;
    {$ENDIF FPC}
    function  TabRect_Wrapper(args: PPyObject): PPyObject; cdecl;
    {$IFNDEF FPC}
    function  ScrollTabs_Wrapper(args: PPyObject): PPyObject; cdecl;
    {$ENDIF FPC}
    function  FindNextPage_Wrapper(args: PPyObject): PPyObject; cdecl;
    function  SelectNextPage_Wrapper(args: PPyObject): PPyObject; cdecl;
    // Property Getters
    function Get_ActivePage( AContext: Pointer): PPyObject; cdecl;
    function Get_ActivePageIndex( AContext: Pointer): PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_Canvas( AContext: Pointer): PPyObject; cdecl;
    {$ENDIF FPC}
    function Get_PageCount( AContext: Pointer): PPyObject; cdecl;
    function Get_Pages( AContext: Pointer): PPyObject; cdecl;
    {$IFNDEF FPC}
    function Get_RowCount( AContext: Pointer): PPyObject; cdecl;
    {$ENDIF FPC}
    // Property Setters
    function Set_ActivePage( AValue: PPyObject; AContext: Pointer): integer; cdecl;
    function Set_ActivePageIndex( AValue: PPyObject; AContext: Pointer): integer; cdecl;
  public
    class function  DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets( PythonType: TPythonType ); override;
    class procedure RegisterMethods( PythonType: TPythonType ); override;
    // Properties
    property DelphiObject: TPageControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTrackBar = class (TPyDelphiWinControl)
  private
    function GetDelphiObject: TTrackBar;
    procedure SetDelphiObject(const Value: TTrackBar);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TTrackBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiToolButton = class(TPyDelphiGraphicControl)
  private
    function GetDelphiObject: TToolButton;
    procedure SetDelphiObject(const Value: TToolButton);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TToolButton read GetDelphiObject
      write SetDelphiObject;
  end;

  TToolbarAccess = class(TContainerAccess)
  private
    function GetContainer: TToolbar;
  public
    function GetItem(AIndex: Integer): PPyObject; override;
    function GetSize: Integer; override;
    function IndexOf(AValue: PPyObject): Integer; override;
    class function ExpectedContainerClass: TClass; override;
    class function SupportsIndexOf: Boolean; override;
    class function Name: string; override;
    property Container: TToolbar read GetContainer;
  end;

  TPyDelphiToolbar = class(TPyDelphiWinControl)
  private
    function GetDelphiObject: TToolbar;
    procedure SetDelphiObject(const Value: TToolbar);
  protected
    function Get_ButtonCount(AContext: Pointer): PPyObject; cdecl;
    function Get_Buttons(AContext: Pointer): PPyObject; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class function GetContainerAccessClass: TContainerAccessClass; override;

    property DelphiObject: TToolbar read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiCustomCustomTabControl = class (TPyDelphiWinControl)
  private
    function GetDelphiObject: TCustomTabControl;
    procedure SetDelphiObject(const Value: TCustomTabControl);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomTabControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomTabControl = class (TPyDelphiCustomCustomTabControl)
  private
    function GetDelphiObject: TTabControl;
    procedure SetDelphiObject(const Value: TTabControl);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TTabControl read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomStatusBar = class (TPyDelphiWinControl)
  private
    function GetDelphiObject: TCustomStatusBar;
    procedure SetDelphiObject(const Value: TCustomStatusBar);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomStatusBar read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiStatusBar = class (TPyDelphiCustomStatusBar)
  private
    function GetDelphiObject: TStatusBar;
    procedure SetDelphiObject(const Value: TStatusBar);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TStatusBar read GetDelphiObject write SetDelphiObject;
  end;

  //TTVChangingEvent
  TTVChangingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode; var AllowChange: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVChangedEvent
  TTVChangedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVEditingEvent
  TTVEditingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVEditedEvent
  TTVEditedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode; var S: string);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVExpandingEvent
  TTVExpandingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCollapsingEvent
  TTVCollapsingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVExpandedEvent
  TTVExpandedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node: TTreeNode);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCompareEvent
  TTVCompareEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Node1, Node2: TTreeNode; Data: Integer; var Compare: Integer);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVHintEvent
  TTVHintEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; const Node: TTreeNode; var Hint: String);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCustomDrawEvent
  TTVCustomDrawEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; const ARect: TRect; var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCustomDrawItemEvent
  TTVCustomDrawItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVAdvancedCustomDrawEvent
  TTVAdvancedCustomDrawEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; const ARect: TRect; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVAdvancedCustomDrawItemEvent
  TTVAdvancedCustomDrawItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCreateNodeClassEvent
  TTVCreateNodeClassEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; var NodeClass: TTreeNodeClass);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCheckStateChangedEvent
  TTVCheckStateChangedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; Node: TTreeNode; CheckState: TNodeCheckState);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TTVCheckStateChangingEvent
  TTVCheckStateChangingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomTreeView; Node: TTreeNode;
      NewCheckState, OldCheckState: TNodeCheckState; var AllowChange: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  TPyDelphiTreeNode = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TTreeNode;
    procedure SetDelphiObject(const Value: TTreeNode);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TTreeNode read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomTreeView = class(TPyDelphiWinControl)
  private
    function GetDelphiObject: TCustomTreeView;
    procedure SetDelphiObject(const Value: TCustomTreeView);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomTreeView read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiTreeView = class(TPyDelphiCustomTreeView)
  private
    function GetDelphiObject: TTreeView;
    procedure SetDelphiObject(const Value: TTreeView);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TTreeView read GetDelphiObject write SetDelphiObject;
  end;

  //TLVDeletedEvent
  TLVDeletedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVEditingEvent
  TLVEditingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVEditedEvent
  TLVEditedEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; var S: string);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVChangeEvent
  TLVChangeEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; Change: TItemChange);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVChangingEvent
  TLVChangingEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; Change: TItemChange;
      var AllowChange: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVColumnClickEvent
  TLVColumnClickEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Column: TListColumn);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVColumnRClickEvent
  TLVColumnRClickEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Column: TListColumn; Point: TPoint);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCompareEvent
  TLVCompareEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
      var Compare: Integer);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVNotifyEvent
  TLVNotifyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVSelectItemEvent
  TLVSelectItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; Selected: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCheckedItemEvent
  TLVCheckedItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVDrawItemEvent
  TLVDrawItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCustomDrawEvent
  TLVCustomDrawEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; const ARect: TRect;
      var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCustomDrawItemEvent
  TLVCustomDrawItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCustomDrawSubItemEvent
  TLVCustomDrawSubItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVAdvancedCustomDrawEvent
  TLVAdvancedCustomDrawEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; const ARect: TRect;
      Stage: TCustomDrawStage; var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVAdvancedCustomDrawItemEvent
  TLVAdvancedCustomDrawItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVAdvancedCustomDrawSubItemEvent
  TLVAdvancedCustomDrawSubItemEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVOwnerDataEvent
  TLVOwnerDataEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVOwnerDataFindEvent
  TLVOwnerDataFindEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Find: TItemFind;
      const FindString: string; const FindPosition: TPoint; FindData: TCustomData;
      StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
      var Index: Integer);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVOwnerDataHintEvent
  TLVOwnerDataHintEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; StartIndex, EndIndex: Integer);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVOwnerDataStateChangeEvent
  TLVOwnerDataStateChangeEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; StartIndex,
      EndIndex: Integer; OldState, NewState: TItemStates);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVSubItemImageEvent
  TLVSubItemImageEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; SubItem: Integer;
      var ImageIndex: Integer);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVInfoTipEvent
  TLVInfoTipEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; Item: TListItem; var InfoTip: string);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  //TLVCreateItemClassEvent
  TLVCreateItemClassEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TCustomListView; var ItemClass: TListItemClass);
  public
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject); override;

    class function GetTypeInfo: PTypeInfo; override;
  end;

  TPyDelphiListItem = class(TPyDelphiPersistent)
  private
    function GetDelphiObject: TListItem;
    procedure SetDelphiObject(const Value: TListItem);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TListItem read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomListView = class(TPyDelphiCustomMultiListControl)
  private
    function GetDelphiObject: TCustomListView;
    procedure SetDelphiObject(const Value: TCustomListView);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TCustomListView read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiListView = class(TPyDelphiCustomListView)
  private
    function GetDelphiObject: TListView;
    procedure SetDelphiObject(const Value: TListView);
  public
    class function DelphiObjectClass: TClass; override;
    property DelphiObject: TListView read GetDelphiObject write SetDelphiObject;
  end;

  function NodeCheckStateToPython(const ANodeCheckState: TNodeCheckState): PPyObject;

  function CustomDrawTargetToPython(const ACustomDrawTarget: TCustomDrawTarget): PPyObject;
  function CustomDrawStageToPython(const ACustomDrawStage: TCustomDrawStage): PPyObject;
  function CustomDrawStateToPython(const ACustomDrawState: TCustomDrawState): PPyObject;

  function ItemChangeToPython(const AItemChange: TItemChange): PPyObject;
  function ItemStateToPython(const AItemState: TItemState): PPyObject;
  function ItemStatesToPython(const AItemStates: TItemStates): PPyObject;
  function ItemFindToPython(const AItemFind: TItemFind): PPyObject;
  function SearchDirectionToPython(const ASearchDirection: TSearchDirection): PPyObject;

implementation

uses
  WrapDelphiTypes, WrapDelphiWindows,
  Vcl.ExtCtrls;

{ Register the wrappers, the globals and the constants }
type
  TComCtrlsRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

function NodeCheckStateToPython(const ANodeCheckState: TNodeCheckState): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TNodeCheckState),
      Ord(ANodeCheckState)));
end;

function CustomDrawTargetToPython(const ACustomDrawTarget: TCustomDrawTarget): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TCustomDrawTarget),
      Ord(ACustomDrawTarget)));
end;

function CustomDrawStageToPython(const ACustomDrawStage: TCustomDrawStage): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TCustomDrawStage),
      Ord(ACustomDrawStage)));
end;

function CustomDrawStateToPython(const ACustomDrawState: TCustomDrawState): PPyObject;

  procedure Append(const AList: PPyObject; const AString: string);
  var
    LItem: PPyObject;
  begin
    with GetPythonEngine do begin
      LItem := PyUnicodeFromString(AString);
      PyList_Append(AList, LItem);
      Py_XDecRef(LItem);
    end;
  end;

var
  LCompType: PTypeInfo;
  LMin: integer;
  LMax: integer;
  LState: integer;
begin
  Result := GetPythonEngine().PyList_New(0);
  LCompType := GetTypeData(TypeInfo(TCustomDrawState)).CompType^;
  LMin := LCompType^.TypeData^.MinValue;
  LMax := LCompType^.TypeData^.MaxValue;
  for LState := LMin to LMax do
    Append(Result, GetEnumName(LCompType, LState));
end;

function ItemChangeToPython(const AItemChange: TItemChange): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TItemChange),
      Ord(AItemChange)));
end;

function ItemStateToPython(const AItemState: TItemState): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TItemState),
      Ord(AItemState)));
end;

function ItemStatesToPython(const AItemStates: TItemStates): PPyObject;

  procedure Append(const AList: PPyObject; const AItemState: TItemState);
  var
    LItem: PPyObject;
  begin
    with GetPythonEngine do begin
      LItem := ItemStateToPython(AItemState);
      PyList_Append(AList, LItem);
      Py_XDecRef(LItem);
    end;
  end;

var
  LItemState: TItemState;
begin
  Result := GetPythonEngine().PyList_New(0);
  for LItemState := Low(TItemState) to High(TItemState) do
    Append(Result, LItemState);
end;

function ItemFindToPython(const AItemFind: TItemFind): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TItemFind),
      Ord(AItemFind)));
end;

function SearchDirectionToPython(const ASearchDirection: TSearchDirection): PPyObject;
begin
  Result := GetPythonEngine().PyUnicodeFromString(
    System.TypInfo.GetEnumName(
      TypeInfo(TSearchDirection),
      Ord(ASearchDirection)));
end;

{ TComCtrlsRegistration }

procedure TComCtrlsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TComCtrlsRegistration.Name: string;
begin
  Result := 'ComCtrls';
end;

procedure TComCtrlsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  {$IFNDEF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDateTimePicker);
  {$ENDIF FPC}
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiPageControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTabSheet);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTrackBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiToolButton);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiToolbar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomCustomTabControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomTabControl);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomStatusBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStatusBar);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTreeNode);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomTreeView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiTreeView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListItem);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomListView);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiListView);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TTabChangingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVChangingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVChangedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVEditingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVEditedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVExpandingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCollapsingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVExpandedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCompareEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVHintEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCustomDrawEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCustomDrawItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVAdvancedCustomDrawEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVAdvancedCustomDrawItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCreateNodeClassEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCheckStateChangedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TTVCheckStateChangingEventHandler);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVDeletedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVEditingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVEditedEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVChangeEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVChangingEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVColumnClickEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVColumnRClickEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCompareEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVNotifyEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVSelectItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCheckedItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVDrawItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCustomDrawEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCustomDrawItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCustomDrawSubItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVAdvancedCustomDrawEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVAdvancedCustomDrawItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVAdvancedCustomDrawSubItemEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVOwnerDataEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVOwnerDataFindEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVOwnerDataHintEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVOwnerDataStateChangeEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVSubItemImageEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVInfoTipEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TLVCreateItemClassEventHandler);
end;

{$IFNDEF FPC}

{ TPyDelphiDateTimePicker }

class function TPyDelphiDateTimePicker.DelphiObjectClass: TClass;
begin
  Result := TDateTimePicker;
end;

function TPyDelphiDateTimePicker.GetDelphiObject: TDateTimePicker;
begin
  Result := TDateTimePicker(inherited DelphiObject);
end;

procedure TPyDelphiDateTimePicker.SetDelphiObject(const Value: TDateTimePicker);
begin
  inherited DelphiObject := Value;
end;
{$ENDIF FPC}

{ TPyDelphiPageControl }

class function TPyDelphiPageControl.DelphiObjectClass: TClass;
begin
  Result := TPageControl;
end;

function TPyDelphiPageControl.FindNextPage_Wrapper(
  args: PPyObject): PPyObject;
//  function FindNextPage(CurPage: TTabSheet; GoForward, CheckTabVisible: Boolean): TTabSheet;
var
  _CurPage: TObject;
  _pCurPage: PPyObject;
  _pGoForward, _pCheckTabVisible: PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'OOO:FindNextPage',@_pCurPage, @_pGoForward, @_pCheckTabVisible ) <> 0 then begin
      if CheckObjAttribute(_pCurPage, 'CurPage', TTabSheet, _CurPage) then
        Result := Wrap( DelphiObject.FindNextPage(TTabSheet(_CurPage), PyObject_IsTrue(_pGoForward)<>0, PyObject_IsTrue(_pCheckTabVisible)<>0) )
      else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiPageControl.GetDelphiObject: TPageControl;
begin
  Result := TPageControl(inherited DelphiObject);
end;

{$IFNDEF FPC}
function TPyDelphiPageControl.GetHitTestInfoAt_Wrapper(
  args: PPyObject): PPyObject;

  procedure AppendString(const AText : string);
  var
    _text : PPyObject;
  begin
    with GetPythonEngine do
    begin
      _text := PyUnicodeFromString(AText);
      PyList_Append(Result, _text);
      Py_DecRef(_text);
    end;
  end;

var
  x, y: Integer;
  _result : THitTests;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ii:GetHitTestInfoAt',@x, @y ) <> 0 then begin
      _result := DelphiObject.GetHitTestInfoAt(x, y);
      Result := PyList_New(0);
      if htAbove in _result then
        AppendString('htAbove');
      if htBelow in _result then
        AppendString('htBelow');
      if htNowhere in _result then
        AppendString('htNowhere');
      if htOnItem in _result then
        AppendString('htOnItem');
      if htOnButton in _result then
        AppendString('htOnButton');
      if htOnIcon in _result then
        AppendString('htOnIcon');
      if htOnIndent in _result then
        AppendString('htOnIndent');
      if htOnLabel in _result then
        AppendString('htOnLabel');
      if htOnRight in _result then
        AppendString('htOnRight');
      if htOnStateIcon in _result then
        AppendString('htOnStateIcon');
      if htToLeft in _result then
        AppendString('htToLeft');
      if htToRight in _result then
        AppendString('htToRight');
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiPageControl.Get_ActivePage(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.ActivePage);
end;

function TPyDelphiPageControl.Get_ActivePageIndex(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.ActivePageIndex);
end;

{$IFNDEF FPC}
function TPyDelphiPageControl.Get_Canvas(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Canvas);
end;
{$ENDIF FPC}

function TPyDelphiPageControl.Get_PageCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.PageCount);
end;

function TPyDelphiPageControl.Get_Pages(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TPagesAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

{$IFNDEF FPC}
function TPyDelphiPageControl.Get_RowCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.RowCount);
end;

function TPyDelphiPageControl.IndexOfTabAt_Wrapper(
  args: PPyObject): PPyObject;
var
  x, y: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ii:IndexOfTabAt',@x, @y ) <> 0 then begin
      Result := VariantAsPyObject( DelphiObject.IndexOfTabAt(x, y) );
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

class procedure TPyDelphiPageControl.RegisterGetSets(
  PythonType: TPythonType);
begin
  PythonType.AddGetSet('ActivePage', @TPyDelphiPageControl.Get_ActivePage, @TPyDelphiPageControl.Set_ActivePage,
        'Specifies the page currently displayed by the page control.', nil);
  PythonType.AddGetSet('ActivePageIndex', @TPyDelphiPageControl.Get_ActivePageIndex, @TPyDelphiPageControl.Set_ActivePageIndex,
        'Specifies the page currently displayed by the page control.', nil);
  {$IFNDEF FPC}
  PythonType.AddGetSet('Canvas', @TPyDelphiPageControl.Get_Canvas, nil,
        'Gives access to the tab control canvas.', nil);
  {$ENDIF FPC}
  PythonType.AddGetSet('PageCount', @TPyDelphiPageControl.Get_PageCount, nil,
        'Indicates the number of pages in the TPageControl object.', nil);
  PythonType.AddGetSet('Pages', @TPyDelphiPageControl.Get_Pages, nil,
        'Lists all the pages in the TPageControl.', nil);
  {$IFNDEF FPC}
  PythonType.AddGetSet('RowCount', @TPyDelphiPageControl.Get_RowCount, nil,
        '', nil);
  {$ENDIF FPC}
end;

class procedure TPyDelphiPageControl.RegisterMethods(
  PythonType: TPythonType);
begin
  {$IFNDEF FPC}
  PythonType.AddMethod('IndexOfTabAt', @TPyDelphiPageControl.IndexOfTabAt_Wrapper,
    'TPageControl.IndexOfTabAt()'#10 +
    'Indicates the index of the tab at a specified point.');
  PythonType.AddMethod('GetHitTestInfoAt', @TPyDelphiPageControl.GetHitTestInfoAt_Wrapper,
    'TPageControl.GetHitTestInfoAt()'#10 +
    'Returns information about the location of a point relative to the client area of the tab control.');
  {$ENDIF FPC}
  PythonType.AddMethod('TabRect', @TPyDelphiPageControl.TabRect_Wrapper,
    'TPageControl.TabRect()'#10 +
    'Returns the bounding rectangle for a specified tab.');
  {$IFNDEF FPC}
  PythonType.AddMethod('ScrollTabs', @TPyDelphiPageControl.ScrollTabs_Wrapper,
    'TPageControl.ScrollTabs()'#10 +
    'Scrolls the tabs that are visible when the tab control is not multi-line.');
  {$ENDIF FPC}
  PythonType.AddMethod('FindNextPage', @TPyDelphiPageControl.FindNextPage_Wrapper,
    'TPageControl.FindNextPage()'#10 +
    'Returns the next page in the page control before or after a specified page.');
  PythonType.AddMethod('SelectNextPage', @TPyDelphiPageControl.SelectNextPage_Wrapper,
    'TPageControl.SelectNextPage()'#10 +
    'Changes the ActivePage to the first visible page that is before or after the currently active page.');
end;

{$IFNDEF FPC}
function TPyDelphiPageControl.ScrollTabs_Wrapper(
  args: PPyObject): PPyObject;
var
  delta: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'i:ScrollTabs',@delta ) <> 0 then begin
      DelphiObject.ScrollTabs(delta);
      Result := ReturnNone;
    end else
      Result := nil;
  end;
end;
{$ENDIF FPC}

function TPyDelphiPageControl.SelectNextPage_Wrapper(
  args: PPyObject): PPyObject;
// procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean = True);
var
  _pGoForward, _pCheckTabVisible: PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'OO:SelectNextPage',@_pGoForward, @_pCheckTabVisible ) <> 0 then begin
      DelphiObject.SelectNextPage(PyObject_IsTrue(_pGoForward)<>0, PyObject_IsTrue(_pCheckTabVisible)<>0);
      Result := ReturnNone;
    end else
      Result := nil;
  end;

end;

procedure TPyDelphiPageControl.SetDelphiObject(const Value: TPageControl);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiPageControl.Set_ActivePage(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'ActivePage', TTabSheet, _object) then
  begin
    Self.DelphiObject.ActivePage := TTabSheet(_object);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiPageControl.Set_ActivePageIndex(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'ActivePageIndex', _value) then
  begin
    DelphiObject.ActivePageIndex := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiPageControl.TabRect_Wrapper(args: PPyObject): PPyObject;
var
  idx: Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if GetPythonEngine.PyArg_ParseTuple( args, 'i:TabRect',@idx ) <> 0 then begin
    Result := WrapRect(PyDelphiWrapper, DelphiObject.TabRect(idx));
  end else
    Result := nil;
end;

{ TPyDelphiTabSheet }

class function TPyDelphiTabSheet.DelphiObjectClass: TClass;
begin
  Result := TTabSheet;
end;

function TPyDelphiTabSheet.GetDelphiObject: TTabSheet;
begin
  Result := TTabSheet(inherited DelphiObject);
end;

function TPyDelphiTabSheet.Get_PageControl(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.PageControl);
end;

function TPyDelphiTabSheet.Get_TabIndex(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.TabIndex);
end;

class procedure TPyDelphiTabSheet.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddGetSet('TabIndex', @TPyDelphiTabSheet.Get_TabIndex, nil,
        'Indicates the position of the tab sheet in the set of visible tabs in a TPageControl object.', nil);
  PythonType.AddGetSet('PageControl', @TPyDelphiTabSheet.Get_PageControl, @TPyDelphiTabSheet.Set_PageControl,
        'Indicates the page control object that contains the tab sheet.', nil);
end;

procedure TPyDelphiTabSheet.SetDelphiObject(const Value: TTabSheet);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiTabSheet.Set_PageControl(AValue: PPyObject;
  AContext: Pointer): integer;
var
  _object : TObject;
begin
  Adjust(@Self);
  if CheckObjAttribute(AValue, 'PageControl', TPageControl, _object) then
  begin
    Self.DelphiObject.PageControl := TPageControl(_object);
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPagesAccess }

class function TPagesAccess.ExpectedContainerClass: TClass;
begin
  Result := TPageControl;
end;

function TPagesAccess.GetContainer: TPageControl;
begin
  Result := TPageControl(inherited Container);
end;

function TPagesAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Pages[AIndex] );
end;

function TPagesAccess.GetSize: Integer;
begin
  Result := Container.PageCount;
end;

function TPagesAccess.IndexOf(AValue: PPyObject): Integer;
var
  i : Integer;
  S : string;
  _obj : TPyObject;
  _value : TObject;
  _ctrl : TTabSheet;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if PyUnicode_Check(AValue) then
    begin
      S := PyUnicodeAsString(AValue);
      for i := 0 to Container.PageCount-1 do
        if SameText( Container.Pages[i].Name, S) then
        begin
          Result := i;
          Break;
        end;
    end
    else if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if _obj is TPyDelphiObject then
      begin
        _value := TPyDelphiObject(_obj).DelphiObject;
        if _value is TTabSheet then
        begin
          _ctrl := TTabSheet(_value);
          for i := 0 to Container.PageCount-1 do
            if Container.Pages[i] = _ctrl then
            begin
              Result := i;
              Break;
            end;
        end;
      end;
    end;
  end;
end;

class function TPagesAccess.Name: string;
begin
  Result := 'Pages';
end;

class function TPagesAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TTabChangingEventHandler }

constructor TTabChangingEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTabChangingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

procedure TTabChangingEventHandler.DoEvent(Sender: TObject;
  var AllowChange: Boolean);
Var
  PyObject, PyTuple, PyResult, PyAllowChange: PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyAllowChange := CreateVarParam(PyDelphiWrapper, AllowChange);
      _varParam := PythonToDelphi(PyAllowChange) as TPyDelphiVarParameter;
      PyTuple := PyTuple_New(2);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyAllowChange);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        if Assigned(PyResult) then
        begin
          Py_DECREF(PyResult);
          AllowChange := PyObject_IsTrue(_varParam.Value) = 1;
        end;
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TTabChangingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTabChangingEvent);
end;

{ TPyDelphiTrackBar }

class function TPyDelphiTrackBar.DelphiObjectClass: TClass;
begin
  Result := TTrackBar;
end;

function TPyDelphiTrackBar.GetDelphiObject: TTrackBar;
begin
  Result := TTrackBar(inherited DelphiObject);
end;

procedure TPyDelphiTrackBar.SetDelphiObject(const Value: TTrackBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiToolButton }

class function TPyDelphiToolButton.DelphiObjectClass: TClass;
begin
  Result := TToolButton;
end;

function TPyDelphiToolButton.GetDelphiObject: TToolButton;
begin
  Result := TToolButton(inherited DelphiObject);
end;

procedure TPyDelphiToolButton.SetDelphiObject(const Value: TToolButton);
begin
  inherited DelphiObject := Value;
end;

{ TToolbarAccess }

class function TToolbarAccess.ExpectedContainerClass: TClass;
begin
  Result := TToolbar;
end;

function TToolbarAccess.GetContainer: TToolbar;
begin
  Result := TToolbar(inherited Container);
end;

function TToolbarAccess.GetItem(AIndex: Integer): PPyObject;
begin
  Result := Wrap( Container.Buttons[AIndex] );
end;

function TToolbarAccess.GetSize: Integer;
begin
  Result := Container.ButtonCount;
end;

function TToolbarAccess.IndexOf(AValue: PPyObject): Integer;
var
  _obj: TPyObject;
  _item: TToolButton;
begin
  Result := -1;
  with GetPythonEngine do
  begin
    if IsDelphiObject(AValue) then
    begin
      _obj := PythonToDelphi(AValue);
      if (_obj is TPyDelphiObject) and
        (TPyDelphiObject(_obj).DelphiObject is TToolButton) then
      begin
        _item := TToolButton(TPyDelphiObject(_obj).DelphiObject);
        Result := _item.Index;
      end;
    end;
  end;
end;

class function TToolbarAccess.Name: string;
begin
  Result := 'Toolbar.Buttons'
end;

class function TToolbarAccess.SupportsIndexOf: Boolean;
begin
  Result := True;
end;

{ TPyDelphiToolbar }

class function TPyDelphiToolbar.DelphiObjectClass: TClass;
begin
  Result := TToolbar;
end;

class function TPyDelphiToolbar.GetContainerAccessClass: TContainerAccessClass;
begin
  Result := TToolbarAccess;
end;

function TPyDelphiToolbar.GetDelphiObject: TToolbar;
begin
  Result := TToolbar(inherited DelphiObject);
end;

function TPyDelphiToolbar.Get_Buttons(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TToolbarAccess.Create(Self.PyDelphiWrapper,
      Self.DelphiObject));
end;

function TPyDelphiToolbar.Get_ButtonCount(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyLong_FromLong(DelphiObject.ButtonCount);
end;

class procedure TPyDelphiToolbar.RegisterGetSets(PythonType: TPythonType);
begin
  with PythonType do
  begin
    AddGetSet('ButtonCount', @TPyDelphiToolbar.Get_ButtonCount, nil,
      'Indicates the number of buttons in the toolbar.', nil);
    AddGetSet('Actions', @TPyDelphiToolbar.Get_Buttons, nil,
      'Lists the buttons of the toolbar.', nil);
  end;
end;

procedure TPyDelphiToolbar.SetDelphiObject(const Value: TToolbar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomCustomTabControl }

class function TPyDelphiCustomCustomTabControl.DelphiObjectClass: TClass;
begin
  Result := TCustomTabControl;
end;

function TPyDelphiCustomCustomTabControl.GetDelphiObject: TCustomTabControl;
begin
  Result := TCustomTabControl(inherited DelphiObject);
end;

procedure TPyDelphiCustomCustomTabControl.SetDelphiObject(
  const Value: TCustomTabControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomTabControl }

class function TPyDelphiCustomTabControl.DelphiObjectClass: TClass;
begin
  Result := TTabControl;
end;

function TPyDelphiCustomTabControl.GetDelphiObject: TTabControl;
begin
  Result := TTabControl(inherited DelphiObject);
end;

procedure TPyDelphiCustomTabControl.SetDelphiObject(const Value: TTabControl);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomStatusBar }

class function TPyDelphiCustomStatusBar.DelphiObjectClass: TClass;
begin
  Result := TCustomStatusBar;
end;

function TPyDelphiCustomStatusBar.GetDelphiObject: TCustomStatusBar;
begin
  Result := TCustomStatusBar(inherited DelphiObject);
end;

procedure TPyDelphiCustomStatusBar.SetDelphiObject(
  const Value: TCustomStatusBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStatusBar }

class function TPyDelphiStatusBar.DelphiObjectClass: TClass;
begin
  Result := TStatusBar;
end;

function TPyDelphiStatusBar.GetDelphiObject: TStatusBar;
begin
  Result := TStatusBar(inherited DelphiObject);
end;

procedure TPyDelphiStatusBar.SetDelphiObject(const Value: TStatusBar);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTreeNode }

class function TPyDelphiTreeNode.DelphiObjectClass: TClass;
begin
  Result := TTreeNode;
end;

function TPyDelphiTreeNode.GetDelphiObject: TTreeNode;
begin
  Result := TTreeNode(inherited DelphiObject);
end;

procedure TPyDelphiTreeNode.SetDelphiObject(const Value: TTreeNode);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomTreeView }

class function TPyDelphiCustomTreeView.DelphiObjectClass: TClass;
begin
  Result := TCustomTreeView;
end;

function TPyDelphiCustomTreeView.GetDelphiObject: TCustomTreeView;
begin
  Result := TCustomTreeView(inherited DelphiObject);
end;

procedure TPyDelphiCustomTreeView.SetDelphiObject(const Value: TCustomTreeView);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiTreeView }

class function TPyDelphiTreeView.DelphiObjectClass: TClass;
begin
  Result := TTreeView;
end;

function TPyDelphiTreeView.GetDelphiObject: TTreeView;
begin
  Result := TTreeView(inherited DelphiObject);
end;

procedure TPyDelphiTreeView.SetDelphiObject(const Value: TTreeView);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiListItem }

class function TPyDelphiListItem.DelphiObjectClass: TClass;
begin
  Result := TListItem;
end;

function TPyDelphiListItem.GetDelphiObject: TListItem;
begin
  Result := TListItem(inherited DelphiObject);
end;

procedure TPyDelphiListItem.SetDelphiObject(const Value: TListItem);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomListView }

class function TPyDelphiCustomListView.DelphiObjectClass: TClass;
begin
  Result := TCustomListView;
end;

function TPyDelphiCustomListView.GetDelphiObject: TCustomListView;
begin
  Result := TCustomListView(inherited DelphiObject);
end;

procedure TPyDelphiCustomListView.SetDelphiObject(const Value: TCustomListView);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiListView }

class function TPyDelphiListView.DelphiObjectClass: TClass;
begin
  Result := TListView;
end;

function TPyDelphiListView.GetDelphiObject: TListView;
begin
  Result := TListView(inherited DelphiObject);
end;

procedure TPyDelphiListView.SetDelphiObject(const Value: TListView);
begin
  inherited DelphiObject := Value;
end;

{ TTVChangingEventHandler }

constructor TTVChangingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVChangingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVChangingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVChangingEvent);
end;

procedure TTVChangingEventHandler.DoEvent(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyAllowChange: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyAllowChange := CreateVarParam(PyDelphiWrapper, AllowChange);
      LVarParam := PythonToDelphi(LPyAllowChange) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyAllowChange);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowChange := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVChangedEventHandler }

constructor TTVChangedEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVChangedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVChangedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVChangedEvent);
end;

procedure TTVChangedEventHandler.DoEvent(Sender: TObject; Node: TTreeNode);
var
  LPyObject, LPyNode, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVEditingEventHandler }

constructor TTVEditingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVEditingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVEditingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVEditingEvent);
end;

procedure TTVEditingEventHandler.DoEvent(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyAllowEdit: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyAllowEdit := CreateVarParam(PyDelphiWrapper, AllowEdit);
      LVarParam := PythonToDelphi(LPyAllowEdit) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyAllowEdit);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowEdit := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVEditedEventHandler }

constructor TTVEditedEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVEditedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVEditedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVEditedEvent);
end;

procedure TTVEditedEventHandler.DoEvent(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyS: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyS := CreateVarParam(PyDelphiWrapper, S);
      LVarParam := PythonToDelphi(LPyS) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyS);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          S := PyObjectAsString(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVExpandingEventHandler }

constructor TTVExpandingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVExpandingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVExpandingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVExpandingEvent);
end;

procedure TTVExpandingEventHandler.DoEvent(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyAllowExpansion: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyAllowExpansion := CreateVarParam(PyDelphiWrapper, AllowExpansion);
      LVarParam := PythonToDelphi(LPyAllowExpansion) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyAllowExpansion);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowExpansion := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCollapsingEventHandler }

constructor TTVCollapsingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCollapsingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCollapsingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCollapsingEvent);
end;

procedure TTVCollapsingEventHandler.DoEvent(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyAllowCollapse: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyAllowCollapse := CreateVarParam(PyDelphiWrapper, AllowCollapse);
      LVarParam := PythonToDelphi(LPyAllowCollapse) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyAllowCollapse);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowCollapse := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVExpandedEventHandler }

constructor TTVExpandedEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVExpandedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVExpandedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVExpandedEvent);
end;

procedure TTVExpandedEventHandler.DoEvent(Sender: TObject; Node: TTreeNode);
var
  LPyObject, LPyNode, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCompareEventHandler }

constructor TTVCompareEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCompareEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCompareEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCompareEvent);
end;

procedure TTVCompareEventHandler.DoEvent(Sender: TObject; Node1, Node2: TTreeNode;
  Data: Integer; var Compare: Integer);
var
  LPyObject, LPyNode1, LPyNode2, LPyTuple, LPyResult, LPyCompare: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode1 := PyDelphiWrapper.Wrap(Node1);
      LPyNode2 := PyDelphiWrapper.Wrap(Node2);
      LPyCompare := CreateVarParam(PyDelphiWrapper, Compare);
      LVarParam := PythonToDelphi(LPyCompare) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode1);
      PyTuple_SetItem(LPyTuple, 2, LPyNode2);
      PyTuple_SetItem(LPyTuple, 3, PyLong_FromLong(Data));
      PyTuple_SetItem(LPyTuple, 4, LPyCompare);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          Compare := PyLong_AsLong(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVHintEventHandler }

constructor TTVHintEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVHintEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVHintEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVHintEvent);
end;

procedure TTVHintEventHandler.DoEvent(Sender: TObject; const Node: TTreeNode;
  var Hint: String);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyHint: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyHint := CreateVarParam(PyDelphiWrapper, Hint);
      LVarParam := PythonToDelphi(LPyHint) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, LPyHint);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          Hint := PyObjectAsString(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCustomDrawEventHandler }

constructor TTVCustomDrawEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCustomDrawEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCustomDrawEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCustomDrawEvent);
end;

procedure TTVCustomDrawEventHandler.DoEvent(Sender: TCustomTreeView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  LPyObject, LPyRect, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyRect := WrapRect(PyDelphiWrapper, ARect);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyRect);
      PyTuple_SetItem(LPyTuple, 2, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCustomDrawItemEventHandler }

constructor TTVCustomDrawItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCustomDrawItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCustomDrawItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCustomDrawItemEvent);
end;

procedure TTVCustomDrawItemEventHandler.DoEvent(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 3, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVAdvancedCustomDrawEventHandler }

constructor TTVAdvancedCustomDrawEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVAdvancedCustomDrawEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVAdvancedCustomDrawEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVAdvancedCustomDrawEvent);
end;

procedure TTVAdvancedCustomDrawEventHandler.DoEvent(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  LPyObject, LPyRect, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyRect := WrapRect(PyDelphiWrapper, ARect);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyRect);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStageToPython(Stage));
      PyTuple_SetItem(LPyTuple, 3, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVAdvancedCustomDrawItemEventHandler }

constructor TTVAdvancedCustomDrawItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVAdvancedCustomDrawItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVAdvancedCustomDrawItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVAdvancedCustomDrawItemEvent);
end;

procedure TTVAdvancedCustomDrawItemEventHandler.DoEvent(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyPaintImages, LPyDefaultDraw: PPyObject;
  LVarPaintImages, LVarDefaultDraw: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyPaintImages := CreateVarParam(PyDelphiWrapper, PaintImages);
      LVarPaintImages := PythonToDelphi(LPyPaintImages) as TPyDelphiVarParameter;
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarDefaultDraw := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(6);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 3, CustomDrawStageToPython(Stage));
      PyTuple_SetItem(LPyTuple, 4, LPyPaintImages);
      PyTuple_SetItem(LPyTuple, 5, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          PaintImages := PyObject_IsTrue(LVarPaintImages.Value) = 1;
          DefaultDraw := PyObject_IsTrue(LVarDefaultDraw.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCreateNodeClassEventHandler }

constructor TTVCreateNodeClassEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCreateNodeClassEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCreateNodeClassEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCreateNodeClassEvent);
end;

procedure TTVCreateNodeClassEventHandler.DoEvent(Sender: TCustomTreeView;
  var NodeClass: TTreeNodeClass);
var
  LPyObject, LPyTuple, LPyResult, LPyNodeClass: PPyObject;
  LVarParam: TPyDelphiVarParameter;
  LClass: TClass;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNodeClass := CreateVarParam(PyDelphiWrapper, NodeClass);
      LVarParam := PythonToDelphi(LPyNodeClass) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNodeClass);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);

          LClass := TPyDelphiObjectClass(
            PythonToPythonType(LVarParam.Value).PyObjectClass).DelphiObjectClass;

          NodeClass := TTreeNodeClass(LClass);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCheckStateChangedEventHandler }

constructor TTVCheckStateChangedEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCheckStateChangedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCheckStateChangedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCheckStateChangedEvent);
end;

procedure TTVCheckStateChangedEventHandler.DoEvent(Sender: TCustomTreeView;
  Node: TTreeNode; CheckState: TNodeCheckState);
var
  LPyObject, LPyNode, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, NodeCheckStateToPython(CheckState));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TTVCheckStateChangingEventHandler }

constructor TTVCheckStateChangingEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TTVCheckStateChangingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TTVCheckStateChangingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TTVCheckStateChangingEvent);
end;

procedure TTVCheckStateChangingEventHandler.DoEvent(Sender: TCustomTreeView;
  Node: TTreeNode; NewCheckState, OldCheckState: TNodeCheckState;
  var AllowChange: Boolean);
var
  LPyObject, LPyNode, LPyTuple, LPyResult, LPyAllowChange: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyNode := PyDelphiWrapper.Wrap(Node);
      LPyAllowChange := CreateVarParam(PyDelphiWrapper, AllowChange);
      LVarParam := PythonToDelphi(LPyAllowChange) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyNode);
      PyTuple_SetItem(LPyTuple, 2, NodeCheckStateToPython(NewCheckState));
      PyTuple_SetItem(LPyTuple, 3, NodeCheckStateToPython(OldCheckState));
      PyTuple_SetItem(LPyTuple, 4, LPyAllowChange);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowChange := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVDeletedEventHandler }

constructor TLVDeletedEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVDeletedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVDeletedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVDeletedEvent);
end;

procedure TLVDeletedEventHandler.DoEvent(Sender: TObject; Item: TListItem);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVEditingEventHandler }

constructor TLVEditingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVEditingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVEditingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVEditingEvent);
end;

procedure TLVEditingEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyAllowEdit: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyAllowEdit := CreateVarParam(PyDelphiWrapper, AllowEdit);
      LVarParam := PythonToDelphi(LPyAllowEdit) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, LPyAllowEdit);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowEdit := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVEditedEventHandler }

constructor TLVEditedEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVEditedEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVEditedEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVEditedEvent);
end;

procedure TLVEditedEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  var S: string);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyS: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyS := CreateVarParam(PyDelphiWrapper, S);
      LVarParam := PythonToDelphi(LPyS) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, LPyS);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          S := PyObjectAsString(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVChangeEventHandler }

constructor TLVChangeEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVChangeEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVChangeEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVChangeEvent);
end;

procedure TLVChangeEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, ItemChangeToPython(Change));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVChangingEventHandler }

constructor TLVChangingEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVChangingEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVChangingEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVChangingEvent);
end;

procedure TLVChangingEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  Change: TItemChange; var AllowChange: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyAllowChange: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyAllowChange := CreateVarParam(PyDelphiWrapper, AllowChange);
      LVarParam := PythonToDelphi(LPyAllowChange) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, ItemChangeToPython(Change));
      PyTuple_SetItem(LPyTuple, 3, LPyAllowChange);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          AllowChange := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVColumnClickEventHandler }

constructor TLVColumnClickEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVColumnClickEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVColumnClickEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVColumnClickEvent);
end;

procedure TLVColumnClickEventHandler.DoEvent(Sender: TObject;
  Column: TListColumn);
var
  LPyObject, LPyColumn, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyColumn := PyDelphiWrapper.Wrap(Column);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyColumn);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVColumnRClickEventHandler }

constructor TLVColumnRClickEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVColumnRClickEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVColumnRClickEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVColumnRClickEvent);
end;

procedure TLVColumnRClickEventHandler.DoEvent(Sender: TObject;
  Column: TListColumn; Point: TPoint);
var
  LPyObject, LPyColumn, LPyPoint, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyColumn := PyDelphiWrapper.Wrap(Column);
      LPyPoint := WrapPoint(PyDelphiWrapper, Point);
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyColumn);
      PyTuple_SetItem(LPyTuple, 2, LPyPoint);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCompareEventHandler }

constructor TLVCompareEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCompareEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCompareEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCompareEvent);
end;

procedure TLVCompareEventHandler.DoEvent(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  LPyObject, LPyItem1, LPyItem2, LPyTuple, LPyResult, LPyCompare: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem1 := PyDelphiWrapper.Wrap(Item1);
      LPyItem2 := PyDelphiWrapper.Wrap(Item2);
      LPyCompare := CreateVarParam(PyDelphiWrapper, Compare);
      LVarParam := PythonToDelphi(LPyCompare) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem1);
      PyTuple_SetItem(LPyTuple, 2, LPyItem2);
      PyTuple_SetItem(LPyTuple, 3, PyLong_FromLong(Data));
      PyTuple_SetItem(LPyTuple, 4, LPyCompare);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          Compare := PyLong_AsLong(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVNotifyEventHandler }

constructor TLVNotifyEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVNotifyEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVNotifyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVNotifyEvent);
end;

procedure TLVNotifyEventHandler.DoEvent(Sender: TObject; Item: TListItem);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVSelectItemEventHandler }

constructor TLVSelectItemEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVSelectItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVSelectItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVSelectItemEvent);
end;

procedure TLVSelectItemEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, PyBool_FromLong(Ord(Selected)));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCheckedItemEventHandler }

constructor TLVCheckedItemEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCheckedItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCheckedItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCheckedItemEvent);
end;

procedure TLVCheckedItemEventHandler.DoEvent(Sender: TObject; Item: TListItem);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVDrawItemEventHandler }

constructor TLVDrawItemEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVDrawItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVDrawItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVDrawItemEvent);
end;

procedure TLVDrawItemEventHandler.DoEvent(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  LPyObject, LPyItem, LPyRect, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyRect := WrapRect(PyDelphiWrapper, Rect);
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, LPyRect);
      PyTuple_SetItem(LPyTuple, 3, OwnerDrawStateToPython(State));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCustomDrawEventHandler }

constructor TLVCustomDrawEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCustomDrawEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCustomDrawEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCustomDrawEvent);
end;

procedure TLVCustomDrawEventHandler.DoEvent(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
var
  LPyObject, LPyRect, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyRect := WrapRect(PyDelphiWrapper, ARect);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyRect);
      PyTuple_SetItem(LPyTuple, 2, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCustomDrawItemEventHandler }

constructor TLVCustomDrawItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCustomDrawItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCustomDrawItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCustomDrawItemEvent);
end;

procedure TLVCustomDrawItemEventHandler.DoEvent(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 3, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCustomDrawSubItemEventHandler }

constructor TLVCustomDrawSubItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCustomDrawSubItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCustomDrawSubItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCustomDrawSubItemEvent);
end;

procedure TLVCustomDrawSubItemEventHandler.DoEvent(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, PyLong_FromLong(SubItem));
      PyTuple_SetItem(LPyTuple, 3, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 4, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVAdvancedCustomDrawEventHandler }

constructor TLVAdvancedCustomDrawEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVAdvancedCustomDrawEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVAdvancedCustomDrawEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVAdvancedCustomDrawEvent);
end;

procedure TLVAdvancedCustomDrawEventHandler.DoEvent(Sender: TCustomListView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  LPyObject, LPyRect, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyRect := WrapRect(PyDelphiWrapper, ARect);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyRect);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStageToPython(Stage));
      PyTuple_SetItem(LPyTuple, 3, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVAdvancedCustomDrawItemEventHandler }

constructor TLVAdvancedCustomDrawItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVAdvancedCustomDrawItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVAdvancedCustomDrawItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVAdvancedCustomDrawItemEvent);
end;

procedure TLVAdvancedCustomDrawItemEventHandler.DoEvent(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
  var DefaultDraw: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarDefaultDraw: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarDefaultDraw := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 3, CustomDrawStageToPython(Stage));
      PyTuple_SetItem(LPyTuple, 4, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarDefaultDraw.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVAdvancedCustomDrawSubItemEventHandler }

constructor TLVAdvancedCustomDrawSubItemEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVAdvancedCustomDrawSubItemEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVAdvancedCustomDrawSubItemEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVAdvancedCustomDrawSubItemEvent);
end;

procedure TLVAdvancedCustomDrawSubItemEventHandler.DoEvent(
  Sender: TCustomListView; Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyDefaultDraw: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyDefaultDraw := CreateVarParam(PyDelphiWrapper, DefaultDraw);
      LVarParam := PythonToDelphi(LPyDefaultDraw) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(6);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, PyLong_FromLong(SubItem));
      PyTuple_SetItem(LPyTuple, 3, CustomDrawStateToPython(State));
      PyTuple_SetItem(LPyTuple, 4, CustomDrawStageToPython(Stage));
      PyTuple_SetItem(LPyTuple, 5, LPyDefaultDraw);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          DefaultDraw := PyObject_IsTrue(LVarParam.Value) = 1;
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVOwnerDataEventHandler }

constructor TLVOwnerDataEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVOwnerDataEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVOwnerDataEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVOwnerDataEvent);
end;

procedure TLVOwnerDataEventHandler.DoEvent(Sender: TObject; Item: TListItem);
var
  LPyObject, LPyItem, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVOwnerDataFindEventHandler }

constructor TLVOwnerDataFindEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVOwnerDataFindEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVOwnerDataFindEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVOwnerDataFindEvent);
end;

procedure TLVOwnerDataFindEventHandler.DoEvent(Sender: TObject; Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: TCustomData;
  StartIndex: Integer; Direction: TSearchDirection; Wrap: Boolean;
  var Index: Integer);
var
  LPyObject, LPyTuple, LPyResult, LPyIndex: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyIndex := CreateVarParam(PyDelphiWrapper, Index);
      LVarParam := PythonToDelphi(LPyIndex) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(9);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, ItemFindToPython(Find));
      PyTuple_SetItem(LPyTuple, 2, PyUnicodeFromString(FindString));
      PyTuple_SetItem(LPyTuple, 3, WrapPoint(PyDelphiWrapper, FindPosition));
      PyTuple_SetItem(LPyTuple, 4, PyLong_FromLong(NativeInt(FindData)));
      PyTuple_SetItem(LPyTuple, 5, PyLong_FromLong(StartIndex));
      PyTuple_SetItem(LPyTuple, 6, SearchDirectionToPython(Direction));
      PyTuple_SetItem(LPyTuple, 7, PyBool_FromLong(Ord(Wrap)));
      PyTuple_SetItem(LPyTuple, 8, LPyIndex);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          Index := PyLong_AsLong(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVOwnerDataHintEventHandler }

constructor TLVOwnerDataHintEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVOwnerDataHintEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVOwnerDataHintEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVOwnerDataHintEvent);
end;

procedure TLVOwnerDataHintEventHandler.DoEvent(Sender: TObject; StartIndex,
  EndIndex: Integer);
var
  LPyObject, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, PyLong_FromLong(StartIndex));
      PyTuple_SetItem(LPyTuple, 2, PyLong_FromLong(EndIndex));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVOwnerDataStateChangeEventHandler }

constructor TLVOwnerDataStateChangeEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVOwnerDataStateChangeEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVOwnerDataStateChangeEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVOwnerDataStateChangeEvent);
end;

procedure TLVOwnerDataStateChangeEventHandler.DoEvent(Sender: TObject;
  StartIndex, EndIndex: Integer; OldState, NewState: TItemStates);
var
  LPyObject, LPyTuple, LPyResult: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyTuple := PyTuple_New(5);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, PyLong_FromLong(StartIndex));
      PyTuple_SetItem(LPyTuple, 2, PyLong_FromLong(EndIndex));
      PyTuple_SetItem(LPyTuple, 3, ItemStatesToPython(OldState));
      PyTuple_SetItem(LPyTuple, 4, ItemStatesToPython(NewState));
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then
          Py_DECREF(LPyResult);
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVSubItemImageEventHandler }

constructor TLVSubItemImageEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVSubItemImageEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVSubItemImageEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVSubItemImageEvent);
end;

procedure TLVSubItemImageEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  SubItem: Integer; var ImageIndex: Integer);
var
  LPyObject, LPyItem, LPySubItem, LPyTuple, LPyResult, LPyImageIndex: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPySubItem := PyLong_FromLong(SubItem);
      LPyImageIndex := CreateVarParam(PyDelphiWrapper, ImageIndex);
      LVarParam := PythonToDelphi(LPyImageIndex) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(4);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, LPySubItem);
      PyTuple_SetItem(LPyTuple, 3, LPyImageIndex);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          ImageIndex := PyLong_AsLong(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVInfoTipEventHandler }

constructor TLVInfoTipEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVInfoTipEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVInfoTipEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVInfoTipEvent);
end;

procedure TLVInfoTipEventHandler.DoEvent(Sender: TObject; Item: TListItem;
  var InfoTip: string);
var
  LPyObject, LPyItem, LPyTuple, LPyResult, LPyInfoTip: PPyObject;
  LVarParam: TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItem := PyDelphiWrapper.Wrap(Item);
      LPyInfoTip := CreateVarParam(PyDelphiWrapper, InfoTip);
      LVarParam := PythonToDelphi(LPyInfoTip) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(3);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItem);
      PyTuple_SetItem(LPyTuple, 2, LPyInfoTip);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);
          InfoTip := PyObjectAsString(LVarParam.Value);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

{ TLVCreateItemClassEventHandler }

constructor TLVCreateItemClassEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  LMethod: TMethod;
begin
  inherited;
  LMethod.Code := @TLVCreateItemClassEventHandler.DoEvent;
  LMethod.Data := Self;
  SetMethodProp(Component, PropertyInfo, LMethod);
end;

class function TLVCreateItemClassEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TLVCreateItemClassEvent);
end;

procedure TLVCreateItemClassEventHandler.DoEvent(Sender: TCustomListView;
  var ItemClass: TListItemClass);
var
  LPyObject, LPyTuple, LPyResult, LPyItemClass: PPyObject;
  LVarParam: TPyDelphiVarParameter;
  LClass: TClass;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK() then
    with GetPythonEngine() do begin
      LPyObject := PyDelphiWrapper.Wrap(Sender);
      LPyItemClass := CreateVarParam(PyDelphiWrapper, ItemClass);
      LVarParam := PythonToDelphi(LPyItemClass) as TPyDelphiVarParameter;
      LPyTuple := PyTuple_New(2);
      PyTuple_SetItem(LPyTuple, 0, LPyObject);
      PyTuple_SetItem(LPyTuple, 1, LPyItemClass);
      try
        LPyResult := PyObject_CallObject(Callable, LPyTuple);
        if Assigned(LPyResult) then begin
          Py_DECREF(LPyResult);

          LClass := TPyDelphiObjectClass(
            PythonToPythonType(LVarParam.Value).PyObjectClass).DelphiObjectClass;

          ItemClass := TListItemClass(LClass);
        end;
      finally
        Py_DECREF(LPyTuple);
      end;
      CheckError();
    end;
end;

initialization
  RegisteredUnits.Add( TComCtrlsRegistration.Create );
  {$IFNDEF FPC}
  System.Classes.RegisterClasses([TDateTimePicker]);
  {$ENDIF FPC}

end.
