(*-----------------------------------------------------------------------------
 Unit Name: WrapDelphi
 Author:    Kiriakos Vlahos
 Date:      24-Feb-2005
 Purpose:   Provide automatic wrapping of Delphi variables utilising RTTI

 Contributors:
   Morgan Martinet (mmm@free.fr)

 Features:
   Published properties and methods compiled with {$METHODINFO ON} are
   handled automatically (Note that METHODINFO can be used only with Delphi7
   or later, but all the other wrapping features will work with previous
   versions of Delphi starting from Delphi5).
   Moreover common methods and properties of
   the following frequently used Delphi classes are also exported
   (Note that this list is not exhaustive):

     TObject (ClassName, Free, InheritsFrom)
       TPersistent (Assign)
         TCollection (sequence interface, Items, Count, Insert, Add, Clear)
         TStrings (mapping interface, Text, Add, AddObject, Delete, IndexOf, Clear)
         TComponent (Event properties, Subproperties, Owner, ComponentCount, Components)
           TControl (Parent)
             TWinControl (ControlCount, Controls)
               TForm (Show, ShowModal, Release)


   TStrings, TCollection.Items, TComponent.Components and
   TWinControl.Controls are exposed as sequence/mapping interfaces.
   You can also access the Screen and Application objects, and some other
   constants like mrOk, mrCancel...

   PyDelphiWrapper.RegisterDelphiWrapper allows the customized wrapping of
   additional Delphi classes over which you do not have direct control.
   PyDelphiWrapper.EventHandlers.RegisterHandler() can be used to add event handling
   functionality.  TNotify events are handled out-of-the-box.  To handle
   other types of events you need to write a TEventHandler descendent and
   register the EventHandler.

   A Module level function CreateComponent(ClassName, Owner) is also exported.
   For this function to work, the class needs to be registered using
   Classes.RegisterClass (Some classes are already pre-registered like TForm,
   TApplication, TScreen, TButton, TCheckBox...).

   You can subclass TForm as you would do in Delphi, but you are not able to
   override the Delphi methods in Python. There is also a helper
   function BindMethodsToEvents that can connect your method handlers to the
   component events if you respect a specific pattern for naming your methods:
   handle_ComponentName_EventName  --> handle_Button1_OnClick
   This function is especially useful when you subclass an existing Delphi form,
   as the form will already have all the necessary components setup, but you'll
   be missing the events to your Python code.
   If you subclass Form in Python and name your class with the same name as
   an existing Delphi form (that must be registered with RegisterClass),
   then this class will be used to instanciate the form instead of the regular empty TForm.

class TTestForm(Form):
  def __init__(self, Owner):
    self.Caption = self.Caption + ' - changed by Python subclass'
    self.BindMethodsToEvents() # this will connect handle_btnAdd_OnClick to btnAdd.OnClick

  def handle_btnAdd_OnClick(self, Sender):
    self.ListBox1.Items.Add(self.Edit1.Text)

  There is also a helper method named SetProps at the TPyDelphiObject level,
  allowing any wrapped object to do:
     button.SetProps(Left=10, Top=20, Caption='Clickme!)

  You can inspect the published properties of any wrapped object by inspecting the
  __published__ property.

  Note that events requiring var parameters like OnCloseQuery will provide a specific object
  containing a single Value property that will hold the actual value of the parameter,
  because Python does not allow modifying the parameters:
    def handle_close_query(self, sender, accept):
      accept.Value = False  # accept = False would have not effect!

 Usage:
   Drop a PyDelphiWrapper component on a form, set its engine and module
   properties to a PythonEngine and PythonModule.

   Note that it is very important to add each wrapped Delphi unit to your uses
   clause or you won't access the specific wrappers as they would not be
   registered.
   To make it easier, you can simply add the unit WrapDelphiVCL to your uses
   clause.

   Alternatively create a PyDelphiWrapper component using code,
   set its engine and module properties and initialize e.g.

    PyDelphiWrapper := TPyDelphiWrapper.Create(Self);
    PyDelphiWrapper.Engine := PyEngine;
    PyDelphiWrapper.Module := PythonModule;
    PyDelphiWrapper.Initialize;  // Should only be called if PyDelphiWrapper is created at run time

    Use PyDelphiWrapper.Wrap to wrap a given object
      var
        p : PPyObject;
      begin
        // Wrap the Form itself.
        p := PyDelphiWrapper.Wrap(Form1);
        PythonModule.SetVar( 'Form', p );
        PyEngine.Py_DecRef(p);
      end;

     Look at the demos 31 and 32 for further examples of usage.

 History:
   1.00  24-Feb-2005  Kiriakos Vlahos
     Initial release

   1.01  12-May-2005 Morgan Martinet

  - inherit TPyDelphiWrapper from TPythonClient
  - removed type TPythonTypeCustomCreate as TPythonType now has a new attribute GenerateCreateFunction
    the custom types didn't use the former CanCreate property and thus CreateComponent conflicted
    with the function exposed by TPyDelphiWrapper.
  - changed the boolean parameter of TObjectToPyObject into an enumeration, to help understand
    when you read the code, if the object is owned or not.
  - added property __bound__ to TPyDelphiObject, to know if the wrapper is still bound to the instance.
  - added property __owned__ to the base Delphi wrapper, to know if the wrapper owns the underlying
    object or not.
  - added SqAssItem and SqSlice to the TStringsWrapper
  - moved method Show of the Form wrapper to the Control wrapper
  - added Exception's message to the exception raised in TPyDelphiMethodObject.Call
  - fixed bug in Collection iterator (method Iter and IterNext were swapped)
  - refactored iterators with a common base class
  - added automatic support of sequences and iterators if the wrapper overrides the GetContainerAccessClass method.
  - refactored index checking
  - implemented sequence protocol with more collections
  - used new class method SetupType for configuring the services exposed by the python type
    allowing better polymorphism.
  - TStrings wrapper now inherits from TPersistent wrapper.
  - Fixed bug in TStrings.SqItem that returned a string instead of a wrapped TObject.
  - Changed DelphiObject member field to a property and redefined its type for each subclass,
    in order to avoid casting DelphiObject each time need to invoke an attribute.
    This was too much error prone, especially with Copy&Paste.
  - Added various helper functions to check parameter types.
  - Allowed events with TObject subclasses, using an interfaces IFreeNotification/IFreeNotificationSubscriber
  - Added helper class TFreeNotificationImpl handling the details of the IFreeNotification implementation.
  - Fixed bug when accessing attributes of an unbound wrapper
  - Renamed TPyStringsObject into TPyDelphiStrings for consistency
  - Changed the TForm wrapper into a TCustomForm wrapper
  - Added helper methods ToTuple, ToList to any wrapper supporting sequences (TStrings, TComponent...)
  - Added Objects property to TStrings
  - TStrings can be accessed with an integer to get the string item or with a key string to get
    the associated object.

   1.02  23-May-2005 Morgan Martinet

  - Wrapped TBasicAction
  - Wrapped TActionList
  - Wrapped Screen object
  - Defined TModalResult constants
  - fixed bug when exiting application with Python events still attached -> crash
  - fixed bug in event handlers: when destroying an event, only set the handler to nil if it is our handler!
  - created TEventHandlers collection
  - Moved code that gets/sets events outside of GetAttrO/SetAttrO into TEventHandlers
  - return the associated Python handler of an event (in TPyDelphiObject.GetAttrO)

   1.03  30-May-2005 Morgan Martinet

  - Wrapped TMonitor
  - Wrapped TApplication
  - The wrappers now will try to receive a free notification from the wrapped object. This will always
    work with components and may work with classes that implement IFreeNotification.
  - Refactored the registration of wrappers and helper types.
    Now you don't have to create your TPythonType instance. This will be done automatically in the
    RegisterDelphiWrapper and RegisterHelperType methods.
    You can setup the new type by overriding the SetupType class method of TPyObject.
      procedure RegisterDelphiWrapper(AWrapperClass : TPyDelphiObjectClass);
      RegisterHelperType(APyObjectClass : TPyObjectClass);
    Also, note that RegisterDelphiClass as been renamed RegisterDelphiWrapper and there's no
    ne need to give the associated Delphi class, as the wrapper class will override a new
    class function named DelphiObjectClass that must return the wrapped delphi class.
  - Moved wrappers into new dedicated units for each Delphi VCL unit:
     WrapDelphiClasses, WrapDelphiControls, WrapDelphiForms, WrapDelphiActnList
  - Added a new registration system at the unit level, to allow each dedicated unit to register
    the wrappers of the unit's classes.
  - New way to define getters/setters by using Class methods instead of global functions,
    thanks to Michiel du Toit.

   1.04  30-May-2005 Morgan Martinet

  - Made WrapDelphi compatible with previous versions of Delphi (below 7):
    all the wrapping features are available, and only the dynamic method invocation
    relying on {$METHODINFO ON} is disabled. Have to check compilation with D5 to D6.
  - Allowed subclassing of components. Introduced new wrappers for TForm and TButton.
  - Added new unit WrapDelphiStdCtrls

   1.05  11-June-2005 Morgan Martinet

  - renamed method TObjectToPyObject into Wrap
  - stored default wrapper types pointers into public properties of TPyDelphiWrapper,
    for immediate access (instead of doing a lookup in the list).
  - added class TPyDelphiVarParameter for handling Delphi var Parameters.
  - Defined event for TForm.OnCloseQuery
  - Defined event for TForm.OnClose

   1.06  13-June-2005 Morgan Martinet

  - Created wrappers for all controls of the StdCtrls unit.
  - Created wrappers for all controls of the ExtCtrls unit in new unit WrapDelphiExtCtrls.
  - Added property __published__ to TPyDelphiObject, that will return the list of all published properties
    of the wrapped class. This can be use to know which properties can be accessed and for documenting...
  - Made Helper types visible at a module level, because Point, Rect... are helper types.
  - Added wrapper for TPoint
  - Implemented method TScreen.MonitorFromPoint using Point object.

   1.07  25-June-2005 Morgan Martinet

  - When creating an instance of a form (with a Python subclass of Form), if the Owner is Application,
    then we use Application.CreateForm instead of just instanciating the metaclass, otherwise the Application
    will never have a Main form.
  - Started making a Python dll module hosting the Delphi wrappers.
  - fixed a declaration error of the property setters in TApplication wrapper
  - Added method RegisterFunction to TPyDelphiWrapper
  - Wrapped api FreeConsole in WrapDelphiForms
  - Added method SetProps at the TPyDelphiObject level, allowing any wrapped object to do:
     button.SetProps(Left=10, Top=20, Caption='Clickme!)
  - Wrapped procedure Abort
  - Created new type for wrapping TRect records.
  - New behaviour with forms: if you subclass Form in Python and name your class with the same name as
    a Delphi form (that must be registered with RegisterClass), then this class will be used to instanciate
    the form instead of the regular empty TForm.
  - Added a fake get/set method to TPyDelphiObject and create get/set definitions for each published property, using
    those fake methods that won't do anything, because the property value will be fetched in the GetAttr method,
    before even trying to use the python properties.
    This will help a lot documenting existing wrappers, using regular python tools, and it will also allow the
    use of the code insight provided by the IDE.

    1.08  16-July-2005 Morgan Martinet

  - Added method BindMethodsToEvents to TComponent wrapper. It will allow a subclassed form
    to automatically bind its controls to the form's methods, if you respect a specific naming
    convention. Each method must be named like:
    def handle_MyComponent_OnClick(self, sender): pass

    Note that for the hooking the form's properties, you have to use a special component name "Self":
    def handle_Self_OnCloseQuery(self, sender, CanClose): pass

    Note that BindMethodsToEvents accepts a default parameter for specifying the expected prefix,
    which defaults to "handle_".

    Note that BindMethodsToEvents returns a list of tuples. Each tuple contains:
    ComponentName, EventName, MethodObject

    This method is especially useful if you create a base form in Delphi, using the form designer,
    with no code (or not much), then you subclass this form in Python, provide events that will
    be automatically be connected when you invoke BindMethodsToEvents in the __init__ handler.

  - Finished cleanup of the property getters (global function --> method)

    1.09  18-Dec-2005 Morgan Martinet

  - Added new unit WrapDelphiWindows (to define a couple of symbols only)
  - Added new unit WrapDelphiComCtrls
  - Added new unit WrapDelphiGrids
  - Added new unit WrapDelphiGraphics
  - Added new unit WrapDelphiButtons
  - Wrapped TSize
  - Wrapped TCanvas, TGraphic, TBitmap, TMetaFile, TIcon, TPicture
  - Wrapped TKeyPressEvent and TKeyEvent
  - Made a breaking change when dealing with property sets:
    now we expect a sequence of strings. Each string should have the name as the enumeration in the set.
    Ex:  MainForm.Anchors = ['akLeft', 'akTop']
    Of course, a set property will now return a list of strings.
    In the past, it would have returned an integer containing all the bits of the set,
    and it would have accepted to assign either the same kind of integer value or
    a string like "[akLeft, akTop]".
  - Made a breaking change when dealing with property enumerations:
    return a string representing its value instead of the ordinal value.
  - You don't need to call explicitely RegisterClass for your registered Python types as it will be done
    automatically for you in RegisterDelphiWrapper. But it is still usefull if you need to create VCL objects
    that have no wrapper, using the CreateComponent helper function.

    1.10  24-Feb-2006 Morgan Martinet

  - Wrapped TPageControl and TTabSheet

    1.11  14-Mar-2006 Morgan Martinet

  - Added methods Repaint and Invalidate to the TControl wrapper
  - Fixed bug when running WrapDelphi without Assertions in the compiler options
    thanks to a report from Dominique Whali
  - made fields fDefaultIterType and fDefaultContainerType of TPyDelphiWrapper protected

    Oct-2019 PyScripter
  - Major refactoring and clean-up
  - In Delhi version newer than XE, enhanced RTTI is used to provide access to
    methods, fields and properties.  So in most cases you no longer need to
    create wrapping classes.
  - __published__ property was replaced with the implementation of the __dir__()
    method, so that you can do for example dir(MainForm) to inspect the
    methods, fields and properties of MainForm.
  - Demo 31 has been updated to test/showcase some of the new features.

    Apr-2020 PyScripter
  - Wrapping of Records using extended RTTI
  - Wrapping of Interfaces using extended RTTI (see unit tests)

    2021
    - FMX Wrapping by Lucas Belo
    - Vcl Menu and Toolbar wrapping by PyScripter
 TODO:
  - Extend SetProps: if property receiving the value is a TStrings and the value a sequence,
    then assign the sequence content to the TStrings.
  - can we debug the Python code executed from a triggered event? Presently not, as we directly ask Python
    to execute a specific callable...
  - Create a simple app that just initializes Python and executes a script? To avoid having a console...
  - Bug with Delphi pyd: can't change the application title, because TApplication creates its own handle
  - Wrap TApplicationEvents. In fact define the events used by TApplicationEvents.
  - Wrap TObjectList
  - Unit Test all exposed attributes
  - Wrap simple types like TMessage
  - Generate Documentation from available metainformation (see __members__, ClassName...)
  - Allow Wrappers to handle IFreeNotification for the wrapped object when the object does not
    support it, only when the wrapper knows that it is safe enough (singleton delphi object)
  - Be able to return an object containing the current event handler of any Delphi object that was hooked by Delphi,
    and not by Python, as presently, if a button has a Delphi OnClick event, inspecting this event from Python
    will return None.

 -----------------------------------------------------------------------------*)
{$I Definition.Inc}

unit WrapDelphi;

interface

uses
  SysUtils, Classes, PythonEngine,  TypInfo, Types,
  Variants,
{$IFNDEF FPC}
{$IFDEF EXTENDED_RTTI}
  Rtti,
{$ELSE}
  ObjAuto,
{$ENDIF}
{$ENDIF}
  Contnrs;

Type
  TObjectOwnership = (soReference, soOwned);

  // forward declaration
  TPyDelphiWrapper = class;

  {
    If you want to benefit from subscribing to events from Python when your
    wrapped class does not inherit from TComponent, then you can simply
    implement the IFreeNotification, store the subscriber event sink and
    trigger its Notify method in your destructor.
    Note that TFreeNotificationImpl does all the plumbing for you.
  }
  IFreeNotificationSubscriber = interface
    ['{F08FB6EA-3D8B-43C0-8343-77C8E06DE401}']
    procedure Notify(ADeletedObject : TObject);
  end;

  IFreeNotification = interface
    ['{085FD1BB-44FC-457A-B357-4E06071BBEA5}']
    procedure Subscribe(const ASubscriber: IFreeNotificationSubscriber);
    procedure UnSubscribe(const ASubscriber: IFreeNotificationSubscriber);
  end;

  { Helper class that handles the detail of implementing IFreeNotification.
    Usage:

    TMyClass = class(TInterfacedObject, IFreeNotification)
    private
      fFreeNotifImpl : IFreeNotification;
    protected
      property FreeNotifImpl : IFreeNotification read fFreeNotifImpl implements IFreeNotification;
    public
      constructor Create;
    end;

    constructor TMyClass.Create;
    begin
      fFreeNotifImpl := TFreeNotificationImpl.Create(Self);
    end;
  }
  TFreeNotificationImpl = class(TInterfacedObject, IFreeNotification)
  private
    fSubscribers : TInterfaceList;
    fOwner: TObject;

    function GetSubscribers : TInterfaceList;
  protected
    // implementation of IFreeNotification
    procedure Subscribe(const ASubscriber: IFreeNotificationSubscriber);
    procedure UnSubscribe(const ASubscriber: IFreeNotificationSubscriber);
  public
    constructor Create(AOwner : TObject);
    destructor Destroy; override;

    property Owner : TObject read fOwner;
  end;

  {
    This class helps wrappers to implement sequence and iterator protocols.
    You must subclass it, override GetItem, GetSize.
    If you override IndexOf, then you must override SupportsIndexOf and return True.
    If you override SetItem, then you must override SupportsWrite and return True.
    You can give a specific name to the container if you override the Name function.

    Note that an instance of this class must belong to a single owner, if you want
    to give it to another class (like a container to an iterator, then you must
    clone it).
  }
  TContainerAccess = class
  private
    fContainer: TObject;
    fWrapper: TPyDelphiWrapper;
  protected
    function Wrap(Obj : TObject; Ownership: TObjectOwnership = soReference) : PPyObject;
  public
    constructor Create(AWrapper : TPyDelphiWrapper; AContainer: TObject); virtual;

    function Clone : TContainerAccess; virtual;
    function GetItem(AIndex : Integer) : PPyObject; virtual; abstract;
    function GetSize : Integer; virtual; abstract;
    function IndexOf(AValue : PPyObject) : Integer; virtual;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; virtual;

    class function ExpectedContainerClass : TClass; virtual; abstract;
    class function Name : string; virtual;
    class function SupportsWrite : Boolean; virtual;
    class function SupportsIndexOf : Boolean; virtual;

    property Container : TObject read fContainer;
    property Wrapper : TPyDelphiWrapper read fWrapper;
  end;
  TContainerAccessClass = class of TContainerAccess;


  {
    Abstract sequence relying on the container access protocol.
    This will help us support the VCL way to access elements,
    for instance: form.Components[i]
    Note that we could simply write form[i], but then we might use it for
    form.Controls[i] (as Components would be the default sequence).
    As the sequence supports iterators, you can also write:
    for i in form: pass
    for i in form.Components: pass
    for i in form.Controls: pass
  }
  TPyDelphiContainer = class(TPyObject)
  private
    fContainerAccess: TContainerAccess;
    fPyDelphiWrapper: TPyDelphiWrapper;
  public
    destructor Destroy; override;

    procedure Setup(APyDelphiWrapper : TPyDelphiWrapper; AContainerAccess : TContainerAccess);
    class procedure SetupType( PythonType : TPythonType ); override;

    function  Repr : PPyObject; override;
    function  Iter : PPyObject; override;

    // Sequence services
    function  SqLength : NativeInt; override;
    function  SqItem( idx : NativeInt ) : PPyObject; override;
    function  SqAssItem( idx : NativeInt; obj : PPyObject) : Integer; override;
    function  SqContains( obj: PPyObject): integer; override;

    // Properties
    property ContainerAccess : TContainerAccess read fContainerAccess;
    property PyDelphiWrapper : TPyDelphiWrapper read fPyDelphiWrapper;
  end;

  {
    Abstract iterator relying on the container access protocol.
  }
  TPyDelphiIterator = class(TPyObject)
  private
    fPosition: Integer;
    fContainerAccess: TContainerAccess;
  public
    destructor Destroy; override;

    procedure Setup(AContainerAccess : TContainerAccess);
    class procedure SetupType( PythonType : TPythonType ); override;

    function  Repr : PPyObject; override;
    function  Iter : PPyObject; override;
    function  IterNext : PPyObject; override;

    // Properties
    property Position : Integer read fPosition;
    property ContainerAccess : TContainerAccess read fContainerAccess;
  end;

  {
    Base class allowing us to implement interfaces.
  }
  TPyInterfacedObject = class(TPyObject, IInterface)
  private
    // implementation of interface IInterface
    {$IFDEF FPC_HAS_CONSTREF}
    function QueryInterface(constref IID: TGUID; out Obj): HResult;  {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _AddRef: Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: Integer; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    {$ELSE}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$ENDIF}
  end;

  {
    PyObject wrapping TObject
    Exposes published properties and methods
    Also exposes the property ClassName and methods InheritesFrom and Free
    Do not create  TPyDelphi or its subclasses directly - Instead use
    PyDelphiWrapper.Wrap
  }
  TPyDelphiObject = class (TPyInterfacedObject, IFreeNotificationSubscriber)
  private
    fDelphiObject: TObject;
    fContainerAccess : TContainerAccess;
    function  GetContainerAccess: TContainerAccess;
    procedure SetDelphiObject(const Value: TObject);
  protected
    function CheckBound : Boolean;
    function HasContainerAccessClass : Boolean;
    procedure SubscribeToFreeNotification; virtual;
    procedure UnSubscribeToFreeNotification; virtual;
    class function GetTypeName : string; virtual;
    // Exposed Methods
    function Free_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function InheritsFrom_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ToTuple_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function ToList_Wrapper(args : PPyObject) : PPyObject; cdecl;
    function SetProps(args, keywords : PPyObject) : PPyObject; cdecl;
    function Dir_Wrapper(args: PPyObject): PPyObject; cdecl;
    // Exposed Getters
    function Get_ClassName(Acontext : Pointer) : PPyObject; cdecl;
    function Get_Owned(Acontext : Pointer) : PPyObject; cdecl;
    function Set_Owned(AValue: PPyObject; AContext: Pointer): Integer;
    function Get_Bound(Acontext : Pointer) : PPyObject; cdecl;
    // implementation of interface IFreeNotificationSubscriber
    procedure Notify(ADeletedObject : TObject);
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    Owned: Boolean;

    constructor Create( APythonType : TPythonType ); override;
    destructor Destroy; override;

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    function  SetAttrO( key, value: PPyObject) : Integer; override;
    // Objects are equal when they refer to the same DelphiObject
    function  Compare( obj: PPyObject) : Integer; override;
    function  Repr : PPyObject; override;
    // automatic iterator support when the wrapper implements IContainerAccessProvider
    function  Iter : PPyObject; override;
    // Sequence services
    function  SqLength : NativeInt; override;
    function  SqItem( idx : NativeInt ) : PPyObject; override;
    function  SqContains( obj: PPyObject): integer; override;
    function  SqAssItem( idx : NativeInt; obj : PPyObject) : Integer; override;

    class function  DelphiObjectClass : TClass; virtual;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
    // if the class is a container (TStrings, TComponent, TCollection...),
    // then return the class implementing the access to the contained items.
    class function  GetContainerAccessClass : TContainerAccessClass; virtual;
    // creates a container access object using the class returned by GetContainerAccess.
    function  CreateContainerAccess : TContainerAccess; virtual;

    // helper methods
    function Wrap(AObject : TObject; AOwnership: TObjectOwnership = soReference) : PPyObject;
    // Properties
    property DelphiObject: TObject read fDelphiObject write SetDelphiObject;
    property ContainerAccess : TContainerAccess read GetContainerAccess;
  end;
  TPyDelphiObjectClass = class of TPyDelphiObject;

  { This class will simply hold a Python object in its Value property.
    This is required for Delphi var parameters because Python  won't let you
    replace a parameter value with another one, so, we will provide a container
    and you'll be able to change its content. }
  TPyDelphiVarParameter = class(TPyObject)
  private
    fValue: PPyObject;
    procedure SetValue(const Value: PPyObject);
  protected
    // Exposed Getters
    function Get_Value(Acontext : Pointer) : PPyObject; cdecl;
    // Exposed Setters
    function Set_Value(AValue : PPyObject; AContext : Pointer) : Integer; cdecl;
  public
    destructor Destroy; override;

    function  RichCompare( obj : PPyObject; Op : TRichComparisonOpcode) : PPyObject; override;
    function  Repr : PPyObject; override;

    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;

    property Value : PPyObject read fValue write SetValue;
  end;

  {$IFDEF EXTENDED_RTTI}
  { Base class for exposing Records and Interfaces when Extended RTTI is available }
  TPyRttiObject = class (TPyObject)
  private
     fAddr: Pointer;
     fRttiType: TRttiStructuredType;
     function GetValue: TValue; virtual; abstract;
  protected
    // Exposed Methods
    function SetProps(args, keywords : PPyObject) : PPyObject; cdecl;
    function Dir_Wrapper(args: PPyObject): PPyObject; cdecl;
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    constructor Create( APythonType : TPythonType ); override;
    procedure SetAddrAndType(Address: Pointer; Typ: TRttiStructuredType);

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    function  SetAttrO( key, value: PPyObject) : Integer; override;
    function  Repr : PPyObject; override;
    property Addr: Pointer read fAddr;
    property RttiType: TRttiStructuredType read fRttiType;
    property Value: TValue read GetValue;
    //
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure SetupType( PythonType : TPythonType ); override;
  end;

  TPyPascalRecord = class(TPyRttiObject)
  private
    function GetValue: TValue; override;
  public
    class procedure SetupType( PythonType : TPythonType ); override;
  end;

  TPyPascalInterface = class(TPyRttiObject)
  private
    function GetValue: TValue; override;
  public
    class procedure SetupType( PythonType : TPythonType ); override;
  end;
  {$ENDIF}

  TEventHandler = class
  private
    fComponent: TObject;
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    PropertyInfo : PPropInfo;
    EventType : PTypeInfo;
    Callable : PPyObject;
    // connects to the event on creation
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); virtual;
    // Disconnects from the event on destruction
    destructor Destroy; override;
    // Disconnects from the free notification event now
    procedure Unsubscribe;
    // returns the type info of the supported event
    class function GetTypeInfo : PTypeInfo; virtual; abstract;
    // properties
    property Component : TObject read fComponent;
  end;
  TEventHandlerClass = class of TEventHandler;

  TEventHandlers = class
  private
    fItems : TObjectList;
    fRegisteredClasses : TClassList;
    fPyDelphiWrapper: TPyDelphiWrapper;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TEventHandler;
    function GetRegisteredClass(AIndex: Integer): TEventHandlerClass;
    function GetRegisteredClassCount: Integer;
  protected
    function FindHandler(ATypeInfo : PTypeInfo) : TEventHandlerClass;
    property RegisteredClasses[AIndex : Integer] : TEventHandlerClass read GetRegisteredClass;
    property RegisteredClassCount : Integer read GetRegisteredClassCount;
  public
    constructor Create(APyDelphiWrapper : TPyDelphiWrapper);
    destructor Destroy; override;

    function  Add(AEventHandler : TEventHandler) : Boolean;
    procedure Clear;
    procedure Delete(AIndex : Integer);
    function  GetCallable(AComponent : TObject; APropInfo : PPropInfo) : PPyObject; overload;
    function  GetCallable(AComponent : TObject; const APropName : string) : PPyObject; overload;
    function  Link(AComponent : TObject; APropInfo : PPropInfo;
      ACallable : PPyObject; out ErrMsg: string) : Boolean;
    function  IndexOf(AComponent : TObject; APropInfo : PPropInfo) : Integer;
    procedure RegisterHandler(AEventHandlerClass : TEventHandlerClass);
    function  Unlink(AComponent : TObject; APropInfo : PPropInfo) : Boolean;

    property Count : Integer read GetCount;
    property Items[AIndex : Integer] : TEventHandler read GetItem; default;
    property PyDelphiWrapper : TPyDelphiWrapper read fPyDelphiWrapper;
  end;

  TNotifyEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  { Subclass TRegisteredUnit to register your wrappers for a specific unit.
    See WrapDelphiForms which will wrapp some of the classes of the Forms.pas unit.

      type
        TFormsRegistration = class(TRegisteredUnit)
        public
          function Name : string; override;
          procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
          procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
        end;

      procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
      begin
        inherited;
        // Singletons
        APyDelphiWrapper.DefineVar('Application', Application);
        APyDelphiWrapper.DefineVar('Screen',      Screen);

        // MessageBox flags
        APyDelphiWrapper.DefineVar('MB_ABORTRETRYIGNORE', MB_ABORTRETRYIGNORE);
        APyDelphiWrapper.DefineVar('MB_OK',               MB_OK);
      end;

      function TFormsRegistration.Name: string;
      begin
        Result := 'Forms';
      end;

      procedure TFormsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
      begin
        inherited;
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScreen);
        APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiMonitor);

        APyDelphiWrapper.EventHandlers.RegisterHandler(TCloseQueryEventHandler);
      end;

    You must also register this class to the RegisteredUnits singleton like this:
      initialization
        RegisteredUnits.Add(TFormsRegistration.Create);
  }
  TRegisteredUnit = class
  public
    function Name : string; virtual; abstract;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); virtual;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); virtual;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); virtual;
  end;

  { Singleton containing all registered units.
    This will be used by TPyDelphiWrapper for registering the wrappers of
    classes contained in those units.
    The advantage is that we can select what we want to wrap simply by
    including the Wrapped units into the project, and thus avoid code bloating
    if we don't need those units.
  }
  TRegisteredUnits = class
  private
    fItems : TObjectList;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TRegisteredUnit;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ARegisteredModule : TRegisteredUnit);

    property Count : Integer read GetCount;
    property Items[AIndex : Integer] : TRegisteredUnit read GetItem; default;
  end;

  {
     The main component of this unit.
     Method Wrap wraps Delphi objects into Python objects
     Method RegisterDelphiWrapper can be used to extend its functionality.
     Method EventHandlers.RegisterHandler can be used to add event handling functionality
  }
  {$IF not Defined(FPC) and (CompilerVersion >= 23)}
  [ComponentPlatformsAttribute(pidSupportedPlatforms)]
  {$IFEND}
  TPyDelphiWrapper = class(TEngineClient, IFreeNotificationSubscriber)
  private
    // Stores Delphi class registration information
    fClassRegister : TObjectList;
    // Stores registration for Helper Types (do not correspond to Delphi classes)
    fHelperClassRegister : TStringList;
    // Stores Created Event Handlers
    fEventHandlerList : TEventHandlers;
    fVarParamType: TPythonType;
{$IFNDEF FPC}
    fDelphiMethodType: TPythonType;
{$ENDIF}
{$IFDEF EXTENDED_RTTI}
    fRecordType: TPythonType;
    fInterfaceType: TPythonType;
{$ENDIF}
    // Exposed Module level function CreateComponent(ComponentClass, Owner)
    function  CreateComponent( pself, args : PPyObject ) : PPyObject; cdecl;
    // Implementation of interface IFreeNotificationSubscriber
    procedure Notify(ADeletedObject : TObject);
  protected
    FModule : TPythonModule;
    fDefaultIterType: TPythonType;
    fDefaultContainerType: TPythonType;
    procedure CreateWrappers; virtual;
    procedure CreateModuleVars; virtual;
    procedure CreateModuleFunctions; virtual;
    procedure SetEngine(Value : TPythonEngine ); override;
    procedure SetModule(const Value: TPythonModule);
    procedure Notification( AComponent: TComponent;
                            Operation: TOperation); override;
    procedure ModuleReady(Sender : TObject); override;
    procedure UnsubscribeFreeNotifications;
    procedure CreatePyFunc(AModule : TPythonModule; AMethodDef : PPyMethodDef);
  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Initialize; override;
    procedure Finalize; override;
    procedure DefineVar(const AName : string; const AValue : Variant); overload;
    procedure DefineVar(const AName : string; AValue : TObject); overload;
    procedure RegisterDelphiWrapper(AWrapperClass : TPyDelphiObjectClass);
    function  RegisterHelperType(APyObjectClass : TPyObjectClass) : TPythonType;
    function  RegisterFunction(AFuncName : PAnsiChar; AFunc : PyCFunction; ADocString : PAnsiChar ): PPyMethodDef; overload;
    function  RegisterFunction(AFuncName : PAnsiChar; AFunc : TDelphiMethod; ADocString : PAnsiChar ): PPyMethodDef; overload;
    function  GetHelperType(const TypeName : string) : TPythonType;
    //  Function that provides a Python object wrapping an object
    function Wrap(AObj : TObject; AOwnership: TObjectOwnership = soReference) : PPyObject;
    function WrapClass(AClass: TClass): PPyObject;
    {$IFDEF EXTENDED_RTTI}
    //  Function that provides a Python object wrapping a record
    function WrapRecord(Address: Pointer; Typ: TRttiStructuredType): PPyObject;
    //  Function that provides a Python object wrapping an interface
    //  Note the the interface must be compiled in {$M+} mode and have a guid
    //  Usage: WrapInterface(TValue.From(YourInterfaceReference))
    function WrapInterface(const IValue: TValue): PPyObject;
    {$ENDIF}
    // properties
    property EventHandlers : TEventHandlers read fEventHandlerList;
    // Helper types
    property DefaultContainerType : TPythonType read fDefaultContainerType;
    property DefaultIterType : TPythonType read fDefaultIterType;
{$IFNDEF FPC}
    property DelphiMethodType : TPythonType read fDelphiMethodType;
{$ENDIF}
    property VarParamType : TPythonType read fVarParamType;
  published
    property Module : TPythonModule read FModule write SetModule;
  end;

  { Singletons }
  function RegisteredUnits : TRegisteredUnits;
  function GlobalDelphiWrapper: TPyDelphiWrapper;

  { Helper Functions }

  function  CheckIndex(AIndex, ACount : Integer; const AIndexName : string = 'Index') : Boolean;
  function  CheckIntAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Integer) : Boolean;
  function  CheckFloatAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Double) : Boolean;
  function  CheckBoolAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Boolean) : Boolean;
  function  CheckStrAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : string) : Boolean;
  function  CheckObjAttribute(AAttribute : PPyObject; const AAttributeName : string;
                              AExpectedClass : TClass;
                              out AValue : TObject) : Boolean;
  function  CheckCallableAttribute(AAttribute : PPyObject; const AAttributeName : string) : Boolean;
  function  CheckEnum(const AEnumName : string; AValue, AMinValue, AMaxValue : Integer) : Boolean;
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; const AValue : Variant) : PPyObject; overload;
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; AObject: TObject) : PPyObject; overload;
  function  SetToPython(ATypeInfo: PTypeInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(APropInfo: PPropInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(AInstance: TObject; APropInfo: PPropInfo) : PPyObject; overload;
  function  PythonToSet(APropInfo: PPropInfo; ASet : PPyObject) : Integer; overload;
  function  PythonToSet(ATypeInfo: PTypeInfo; ASet : PPyObject) : Integer; overload;
  function  SupportsFreeNotification(AObject : TObject) : Boolean;
  procedure RaiseNotifyEvent(PyDelphiWrapper : TPyDelphiWrapper; ACallable : PPyObject; Sender: TObject);
  {Sets mulptiple properties of PyObject from keywords argument}
  function SetProperties(PyObject: PPyObject; keywords: PPyObject): PPyObject;

implementation

Uses
  Math,
  RTLConsts;

resourcestring
  rs_ErrCheckIndex = '%s "%d" out of range';
  rs_ErrCheckInt = '%s receives only integer values';
  rs_ErrCheckFloat = '%s receives only float values';
  rs_ErrCheckStr = '%s receives only string values';
  rs_ErrCheckCallable = '%s accepts only None or Callable values';
  rs_ErrCheckEnum = 'Enum %s accepts values between %d and %d. Received %d.';
  rs_ErrCheckObjOfType = '%s receives only Delphi objects of type %s';
  rs_ErrCheckObj = '%s receives only Delphi objects';
  rs_ErrSqAss = 'Container %s does not support indexed write (f[i] = x)';
  rs_ErrSqContains = 'Container %s does not support the Contains protocol';
  rs_ErrCheckBound = 'Delphi wrapper %s is not bound';
  rs_ErrSequence = 'Wrapper %s does not support sequences';
  rs_ErrInvalidArgs = '"%s" called with invalid arguments.'#$A'Error: %s';
  rs_ErrInvalidRet = 'Call "%s" returned a value that could not be coverted to Python'#$A'Error: %s';
  rs_IncompatibleArguments = 'Could not find a method with compatible arguments';
  rs_ErrAttrGet = 'Error in getting property "%s".'#$A'Error: %s';
  rs_UnknownAttribute = 'Unknown attribute';
  rs_ErrIterSupport = 'Wrapper %s does not support iterators';
  rs_ErrAttrSetr = 'Error in setting property %s'#$A'Error: %s';
  rs_IncompatibleClasses = 'Incompatible classes';
  rs_IncompatibleRecords = 'Incompatible record types';
  rs_IncompatibleInterfaces = 'Incompatible interfaces';
  rs_NotPublished = 'Event handling is available only for published properties';
  rs_ExpectedObject = 'Expected a Pascal object';
  rs_ExpectedRecord = 'Expected a Pascal record';
  rs_ExpectedClass = 'Expected a Pascal class';
  rs_ExpectedInterface = 'Expected a Pascal interface';
  rs_InvalidClass = 'Invalid class';
  rs_ErrEventNotReg = 'No Registered EventHandler for events of type "%s';
  rs_ErrEventNoSuport = 'Class %s does not support events because it must '+
    'either inherit from TComponent or implement interface IFreeNotification';
  rs_ErrEventExpectCallable = 'You can only assign a callable to method property "%s"';
  rs_NotWritable = 'The class members  is not writable';
  rs_NotReadable = 'The class member is not readable';
  rs_NoAccess = 'Private and protected class members cannot be accessed';
  rs_ErrValueToPython = 'Unsupported conversion from TValue to Python value';
  rs_ErrPythonToValue = 'Unsupported conversion from Python value to TValue';
  rs_ErrNoTypeInfo = 'TypeInfo is not available';
  rs_ErrUnexpected = 'Unexpected error';

var
  gRegisteredUnits : TRegisteredUnits;

{$IFDEF EXTENDED_RTTI}
  function RttiCall(ParentAddress: pointer; PythonType: TPythonType;
    DelphiWrapper: TPyDelphiWrapper; MethName: string;
    ParentRtti: TRttiStructuredType; ob1, ob2: PPyObject;
    AParentAddrIsClass: boolean = false): PPyObject; forward;
{$ENDIF EXTENDED_RTTI}

function RegisteredUnits : TRegisteredUnits;
begin
  if not Assigned(gRegisteredUnits) then
    gRegisteredUnits := TRegisteredUnits.Create;
  Result := gRegisteredUnits;
end;

function GlobalDelphiWrapper: TPyDelphiWrapper;
var
  Engine: TPythonEngine;
  I: Integer;
begin
  Engine := GetPythonEngine;
  for I := 0 to Engine.ClientCount -1 do
    if Engine.Clients[I] is TPyDelphiWrapper then
      Exit(TPyDelphiWrapper(Engine.Clients[I]));
  Result := nil;
end;

{ Helper functions }

{$IFDEF EXTENDED_RTTI}
function DynArrayToPython(const Value: TValue): PPyObject;
var
  I: Integer;
  V: Variant;
  PyEngine: TPythonEngine;
begin
  PyEngine := GetPythonEngine();
  Result := PyEngine.PyList_New(Value.GetArrayLength);
  for I := 0 to Value.GetArrayLength() - 1 do
  begin
    V := Value.GetArrayElement(i).AsVariant;
    PyEngine.PyList_SetItem(Result, I, PyEngine.VariantAsPyObject(V));
  end;
end;

function SimpleValueToPython(const Value: TValue; out ErrMsg: string): PPyObject;
begin
  Result := nil;
  if Value.IsEmpty then begin
    Result := GetPythonEngine.ReturnNone;
    Exit;
  end;
  try
    case Value.TypeInfo^.Kind  of
      tkUnknown:  Result := GetPythonEngine.ReturnNone;
      tkInteger, tkChar, tkFloat,
      tkString, tkWChar, tkLString,
      tkWString, tkUString, tkInt64,
      tkVariant:
        Result := GetPythonEngine.VariantAsPyObject(Value.AsVariant);
      tkEnumeration:
        begin
          if Value.TypeInfo = TypeInfo(Boolean) then
          with GetPythonEngine do begin
            if Value.AsBoolean then
              Result := PPyObject(Py_True)
            else
              Result := PPyObject(Py_False);
            Py_XIncRef(Result);
          end
          else
            Result := GetPythonEngine.PyUnicodeFromString(GetEnumName(Value.TypeInfo,
              PInteger(Value.GetReferenceToRawData)^));
        end;
      tkSet:
        begin
          Result := SetToPython(Value.TypeData.CompType^,
            PInteger(Value.GetReferenceToRawData)^);
        end;
      tkArray, tkDynArray:
        Result := DynArrayToPython(Value);
      tkClass, tkMethod,
      tkRecord, tkInterface,
      tkClassRef, tkPointer, tkProcedure:
        ErrMsg := rs_ErrValueToPython;
    else
      ErrMsg := rs_ErrUnexpected;
    end;
  except
    on E: Exception do begin
      Result := nil;
      ErrMsg := E.Message;
    end;
  end;
end;

function SimplePythonToValue(PyValue: PPyObject; TypeInfo: PTypeInfo;
  out Value: TValue; out ErrMsg: string): Boolean;
Var
  S: string;
  I : integer;
  V : TValue;
begin
  Result := False;
  if TypeInfo = nil then begin
    ErrMsg := rs_ErrNoTypeInfo;
    Exit;
  end;
  try
    case TypeInfo^.Kind  of
      tkUnknown:
        if PyValue = GetPythonEngine.Py_None then
        begin
          Value := TValue.Empty;
          Result := True;
        end
        else
          ErrMsg := rs_ErrPythonToValue;
      tkString, tkWString, tkUString,
      tkLString, tkChar, tkWChar:
        begin
          V := GetPythonEngine.PyObjectAsString(PyValue);
          Value := V.Cast(TypeInfo);
          Result := True;
        end;
      tkInteger, tkFloat, tkInt64, tkVariant:
        begin
          V := TValue.From<Variant>(GetPythonEngine.PyObjectAsVariant(PyValue));
          if TypeInfo^.Kind = tkVariant then
            Value := V
          else
            Value := V.Cast(TypeInfo);
          Result := True;
        end;
      tkEnumeration:
        begin
          S := GetPythonEngine.PyObjectAsString(PyValue);
          I := GetEnumValue(TypeInfo, S);
          Value := TValue.FromOrdinal(TypeInfo, I);
          Result := True;
        end;
      tkSet:
        begin
          I := PythonToSet(TypeInfo, PyValue);
          TValue.Make(@I, TypeInfo, Value);
          Result := True;
        end;
      tkClass, tkMethod, tkArray,
      tkRecord, tkInterface,
      tkClassRef, tkPointer, tkProcedure:
        ErrMsg := rs_ErrPythonToValue;
    else
      ErrMsg := rs_ErrUnexpected;
    end;
  except
    on E: Exception do begin
      Result := False;
      ErrMsg := E.Message;
    end;
  end
end;

function ValidateRecordProperty(PyValue: PPyObject; TypeInfo: PTypeInfo;
  out RecValue: TValue; out ErrMsg: string): Boolean;
var
  PyObject : TPyObject;
begin
  Result := False;
  if IsDelphiObject(PyValue) then
  begin
    PyObject := PythonToDelphi(PyValue);
    if PyObject is TPyPascalRecord then
    begin
      RecValue := TPyPascalRecord(PyObject).Value;
      if RecValue.TypeInfo = TypeInfo then
        Result := True
      else
        ErrMsg := rs_IncompatibleRecords;
    end
    else
      ErrMsg := rs_ExpectedRecord;
  end
  else
    ErrMsg := rs_ExpectedRecord;
end;

function ValidateInterfaceProperty(PyValue: PPyObject; RttiType: TRttiInterfaceType;
  out IValue: TValue; out ErrMsg: string): Boolean;
var
  PyObject : TPyObject;
begin
  if PyValue = GetPythonEngine.Py_None then begin
     Result := True;
     TValue.Make(nil, RttiType.Handle, IValue);
     Exit;
  end;
  Result := False;
  if IsDelphiObject(PyValue) then
  begin
    PyObject := PythonToDelphi(PyValue);
    if PyObject is TPyPascalInterface then
    begin
      IValue := TPyPascalInterface(PyObject).Value;
      if Supports(IValue.AsInterface, RttiType.GUID) then
        Result := True
      else
        ErrMsg := rs_IncompatibleInterfaces;
    end
    else
      ErrMsg := rs_ExpectedInterface;
  end
  else
    ErrMsg := rs_ExpectedInterface;
end;

function ValidateClassRef(PyValue: PPyObject; TypeInfo: PTypeInfo;
  out ClassRef: TClass; out ErrMsg: string): Boolean;
var
  LTypeName: AnsiString;
  LPythonType: TPythonType;
begin
  ClassRef := nil;
  if (PyValue = GetPythonEngine.Py_None) then begin
     Result := True;
     Exit;
  end;

  Result := False;
  // Is PyValue a Python type?
  if PyValue^.ob_type^.tp_name = 'type' then
    LTypeName := PPyTypeObject(PyValue).tp_name
  else
  begin
    ErrMsg := rs_ExpectedClass;
    Exit;
  end;

  LPythonType := GetPythonEngine.FindPythonType(LTypeName);
  if Assigned(LPythonType) then
  begin
    if Assigned(LPythonType) and LPythonType.PyObjectClass.InheritsFrom(TPyDelphiObject) then
    begin
      ClassRef := TPyDelphiObjectClass(LPythonType.PyObjectClass).DelphiObjectClass;
      TypeInfo := GetTypeData(TypeInfo)^.InstanceType^;
      if Assigned(TypeInfo) and (ClassRef.InheritsFrom(GetTypeData(TypeInfo)^.ClassType)) then
        Result := True
      else
        ErrMsg := rs_IncompatibleClasses;
    end
    else
      ErrMsg := rs_ExpectedClass;
  end
  else
    ErrMsg := rs_ExpectedClass;
end;
{$ENDIF}

function ValidateClassProperty(PyValue: PPyObject; TypeInfo: PTypeInfo;
  out Obj: TObject; out ErrMsg: string): Boolean;
var
  PyObject : TPyObject;
begin
  if PyValue = GetPythonEngine.Py_None then begin
     Result := True;
     Obj := nil;
     Exit;
  end;
  Result := False;
  if IsDelphiObject(PyValue) then
  begin
    PyObject := PythonToDelphi(PyValue);
    if PyObject is TPyDelphiObject then
    begin
      Obj := TPyDelphiObject(PyObject).DelphiObject;
      if Obj.ClassType.InheritsFrom(GetTypeData(TypeInfo).ClassType) then
        Result := True
      else
        ErrMsg := rs_IncompatibleClasses;
    end
    else
      ErrMsg := rs_ExpectedObject;
  end
  else
    ErrMsg := rs_ExpectedObject;
end;

function CheckIndex(AIndex, ACount : Integer; const AIndexName : string = 'Index') : Boolean;
begin
  if (AIndex < 0) or (AIndex >= ACount) then
    with GetPythonEngine do
    begin
      Result := False;
      PyErr_SetObject (PyExc_IndexError^, PyUnicodeFromString(
          Format(rs_ErrCheckIndex,[AIndexName, AIndex])));
    end
  else
    Result := True;
end;

function CheckIntAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Integer) : Boolean;
begin
  if GetPythonEngine.PyLong_Check(AAttribute) then
  begin
    AValue := GetPythonEngine.PyLong_AsLong(AAttribute);
    Result := True;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckInt, [AAttributeName])));
  end;
end;

function CheckFloatAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Double) : Boolean;
begin
  if GetPythonEngine.PyFloat_Check(AAttribute) then
  begin
    AValue := GetPythonEngine.PyFloat_AsDouble(AAttribute);
    Result := True;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckFloat, [AAttributeName])));
  end;
end;

function CheckBoolAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : Boolean) : Boolean;
begin
  AValue := GetPythonEngine.PyObject_IsTrue(AAttribute) <> 0;
  Result := True;
end;

function CheckStrAttribute(AAttribute : PPyObject; const AAttributeName : string; out AValue : string) : Boolean;
begin
  if GetPythonEngine.PyUnicode_Check(AAttribute) then
  begin
    AValue := GetPythonEngine.PyUnicodeAsString(AAttribute);
    Result := True;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckStr, [AAttributeName])));
  end;
end;

function CheckCallableAttribute(AAttribute : PPyObject; const AAttributeName : string) : Boolean;
begin
  if (AAttribute = GetPythonEngine.Py_None) or (GetPythonEngine.PyCallable_Check(AAttribute) <> 0) then
    Result := True
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckCallable, [AAttributeName])));
  end;
end;

function  CheckEnum(const AEnumName : string; AValue, AMinValue, AMaxValue : Integer) : Boolean;
begin
  if (AValue >= AMinValue) and (AValue <= AMaxValue) then
    Result := True
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckEnum,
        [AEnumName, AMinValue, AMaxValue, AValue])));
  end;
end;

function CheckObjAttribute(AAttribute : PPyObject; const AAttributeName : string;
                           AExpectedClass : TClass;
                           out AValue : TObject) : Boolean;
var
  PyObject : TPyObject;
begin
  if AAttribute = GetPythonEngine.Py_None then
  begin
    Result := True;
    AValue := nil;
  end
  else if IsDelphiObject(AAttribute) then
  begin
    PyObject := PythonToDelphi(AAttribute);
    if not (PyObject is TPyDelphiObject) or
       not (TPyDelphiObject(PyObject).DelphiObject.InheritsFrom(AExpectedClass)) then
    begin
      Result := False;
      with GetPythonEngine do
        PyErr_SetObject (PyExc_AttributeError^,
          PyUnicodeFromString(Format(rs_ErrCheckObjOfType, [AAttributeName, AExpectedClass.ClassName])));
    end
    else
    begin
      Result := True;
      AValue := TPyDelphiObject(PyObject).DelphiObject;
    end;
  end
  else
  begin
    Result := False;
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckObj, [AAttributeName])));
  end;
end;

function CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; const AValue : Variant) : PPyObject;
var
  tmp : PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Result := PyDelphiWrapper.VarParamType.CreateInstance;
  _varParam := PythonToDelphi(Result) as TPyDelphiVarParameter;
  tmp := GetPythonEngine.VariantAsPyObject(AValue);
  _varParam.Value := tmp; // refcount was incremented
  GetPythonEngine.Py_DECREF(tmp);
end;

function CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; AObject: TObject) : PPyObject;
var
  tmp: PPyObject;
begin
  Result := PyDelphiWrapper.VarParamType.CreateInstance;
  tmp := PyDelphiWrapper.Wrap(AObject);
  (PythonToDelphi(Result) as TPyDelphiVarParameter).Value := tmp;
  GetPythonEngine.Py_DECREF(tmp);
end;

function SupportsFreeNotification(AObject : TObject) : Boolean;
var
  _FreeNotification : IFreeNotification;
begin
  Result := (AObject is TComponent) or AObject.GetInterface(IFreeNotification, _FreeNotification);
end;

procedure RaiseNotifyEvent(PyDelphiWrapper : TPyDelphiWrapper; ACallable : PPyObject; Sender: TObject);
Var
  PyObject, PyTuple, PyResult : PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(ACallable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyTuple := PyTuple_New(1);
      try
        PyTuple_SetItem(PyTuple, 0, PyObject);
        PyResult := PyObject_CallObject(ACallable, PyTuple);
        if Assigned(PyResult) then Py_DECREF(PyResult);
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

function SetToPython(ATypeInfo: PTypeInfo; AValue : Integer) : PPyObject; overload;
var
  S: TIntegerSet;
  I: Integer;
  _name : PPyObject;
begin
  Result := GetPythonEngine.PyList_New(0);
  Integer(S) := AValue;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      _name := GetPythonEngine.PyUnicodeFromString(GetEnumName(ATypeInfo, I));
      GetPythonEngine.PyList_Append(Result, _name);
      GetPythonEngine.Py_XDecRef(_name);
    end;
end;

function SetToPython(APropInfo: PPropInfo; AValue : Integer) : PPyObject; overload;
begin
  {$IFDEF FPC}
  Result := SetToPython(GetTypeData(APropInfo.PropType)^.CompType, AValue);
  {$ELSE FPC}
  Result := SetToPython(GetTypeData(APropInfo^.PropType^)^.CompType^, AValue);
  {$ENDIF FPC}
end;

function SetToPython(AInstance: TObject; APropInfo: PPropInfo) : PPyObject; overload;
begin
  Result := SetToPython(APropInfo, GetOrdProp(AInstance, APropInfo));
end;

function  PythonToSet(ATypeInfo: PTypeInfo; ASet : PPyObject) : Integer; overload;
var
  i : Integer;
  EnumObj: PPyObject;
  EnumName: string;
  EnumValue: Integer;
  EnumInfo: PTypeInfo;
begin
  Result := 0;
  with GetPythonEngine do
  begin
    Assert(PySequence_Check(ASet) <> 0, 'PythonToSet expects a Python sequence as first parameter');

   {$IFDEF FPC}
    EnumInfo := GetTypeData(ATypeInfo)^.CompType;
   {$ELSE FPC}
    EnumInfo := GetTypeData(ATypeInfo)^.CompType^;
   {$ENDIF FPC}
    for i := 0 to PySequence_Length(ASet)-1 do
    begin
      EnumObj := PySequence_GetItem(ASet, i);
      try
        EnumName := PyObjectAsString(EnumObj);
      finally
        Py_XDecRef(EnumObj);
      end;
      EnumValue := GetEnumValue(EnumInfo, EnumName);
      if EnumValue < 0 then
        raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyElement, [EnumName]);
      Include(TIntegerSet(Result), EnumValue);
    end;
  end;
end;

function PythonToSet(APropInfo: PPropInfo; ASet : PPyObject) : Integer; overload;
begin
  {$IFDEF FPC}
  Result := PythonToSet(APropInfo^.PropType, ASet);
  {$ELSE FPC}
  Result := PythonToSet(APropInfo^.PropType^, ASet);
  {$ENDIF FPC}
end;

{$IFDEF FPC}
function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
begin
  Result := Variants.GetPropValue(Instance, PropInfo, False);
end;

procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo; const Value: Variant);
begin
  Variants.SetPropValue(Instance, PropInfo, Value);
end;
{$ENDIF}

function Abort_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
begin
  Result := nil;
  Abort;
end;

Type
  //  Used for class registration by TPyDelphiWrapper fClassRegister
  TRegisteredClass = class
  public
    DelphiClass : TClass;
    PythonType : TPythonType;
  end;

{$IFNDEF FPC}
Type
  //  PyObject wrapping TObject method call
  //  Helper object used by TPyDelphiObject
  TPyDelphiMethodObject = class (TPyObject)
  public
    {$IFDEF EXTENDED_RTTI}
    ParentAddress: Pointer;
    ParentRtti: TRttiStructuredType;
    fDelphiWrapper : TPyDelphiWrapper;
    MethName: string;
    {$ELSE}
    DelphiObject: TObject;
    MethodInfo : TMethodInfoHeader;
    {$ENDIF}
    function  Call( ob1, ob2 : PPyObject) : PPyObject; override;
    function  Repr : PPyObject; override;
    class procedure SetupType( PythonType : TPythonType ); override;
  end;
{$ENDIF}

{ TFreeNotificationImpl }

constructor TFreeNotificationImpl.Create(AOwner: TObject);
begin
  inherited Create;
  Assert(Assigned(AOwner));
  fOwner := AOwner;
end;

destructor TFreeNotificationImpl.Destroy;
var
  i : Integer;
begin
  if Assigned(fSubscribers) then
  begin
    for i := 0 to fSubscribers.Count-1 do
      (fSubscribers[i] as IFreeNotificationSubscriber).Notify(Owner);
    fSubscribers.Free;
  end;
  inherited;
end;

function TFreeNotificationImpl.GetSubscribers: TInterfaceList;
begin
  if not Assigned(fSubscribers) then
    fSubscribers := TInterfaceList.Create;
  Result := fSubscribers;
end;

procedure TFreeNotificationImpl.Subscribe(
  const ASubscriber: IFreeNotificationSubscriber);
begin
  Assert(Assigned(ASubscriber));
  if not Assigned(fSubscribers) or (fSubscribers.IndexOf(ASubscriber) < 0) then
    GetSubscribers.Add(ASubscriber);
end;

procedure TFreeNotificationImpl.UnSubscribe(
  const ASubscriber: IFreeNotificationSubscriber);
begin
  if Assigned(fSubscribers) then
  begin
    fSubscribers.Remove(ASubscriber);
    if fSubscribers.Count = 0 then
      FreeAndNil(fSubscribers);
  end;
end;

{ TContainerAccess }

function TContainerAccess.Clone: TContainerAccess;
begin
  Result := TContainerAccessClass(ClassType).Create(Wrapper, Container);
end;

constructor TContainerAccess.Create(AWrapper: TPyDelphiWrapper;
  AContainer: TObject);
begin
  inherited Create;
  Assert(Assigned(AWrapper));
  Assert(Assigned(AContainer));
  Assert(AContainer.InheritsFrom(ExpectedContainerClass), Format('Class %s expects a container of class %s', [ClassName, ExpectedContainerClass.ClassName]));
  fWrapper := AWrapper;
  fContainer := AContainer;
end;

function TContainerAccess.IndexOf(AValue: PPyObject): Integer;
begin
  Result := -1;
end;

class function TContainerAccess.Name: string;
begin
  Result := ExpectedContainerClass.ClassName;
end;

function TContainerAccess.SetItem(AIndex: Integer; AValue: PPyObject): Boolean;
begin
  Result := False;
end;

class function TContainerAccess.SupportsIndexOf: Boolean;
begin
  Result := False;
end;

class function TContainerAccess.SupportsWrite: Boolean;
begin
  Result := False;
end;

function TContainerAccess.Wrap(Obj: TObject;
  Ownership: TObjectOwnership): PPyObject;
begin
  Result := Wrapper.Wrap(Obj, Ownership);
end;

{ TPyDelphiContainer }

destructor TPyDelphiContainer.Destroy;
begin
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiContainer.Iter: PPyObject;
begin
  Result := PyDelphiWrapper.DefaultIterType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiIterator do
    Setup(Self.ContainerAccess.Clone);
end;

function TPyDelphiContainer.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyUnicodeFromString( Format('<Delphi %s at %x>',
         [ContainerAccess.Name, NativeInt(Self)]) );
end;

procedure TPyDelphiContainer.Setup(APyDelphiWrapper : TPyDelphiWrapper;
  AContainerAccess : TContainerAccess);
begin
  Assert(Assigned(APyDelphiWrapper));
  Assert(Assigned(AContainerAccess));
  fPyDelphiWrapper := APyDelphiWrapper;
  fContainerAccess := AContainerAccess;
end;

class procedure TPyDelphiContainer.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Name := 'DefaultContainerType';
  PythonType.TypeName := 'DelphiDefaultContainer';
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Abstract Container type for Delphi';
  PythonType.Services.Basic    := PythonType.Services.Basic + [bsRepr, bsIter];
  PythonType.Services.Sequence := PythonType.Services.Sequence + [ssLength, ssItem, ssAssItem, ssContains];
end;

function TPyDelphiContainer.SqAssItem(idx: NativeInt;
  obj: PPyObject): integer;
begin
  if ContainerAccess.SupportsWrite then
  begin
    if not CheckIndex(idx, ContainerAccess.GetSize) then
      Result := -1
    else
      if ContainerAccess.SetItem(idx, obj) then
        Result := 0
      else
        Result := -1;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSqAss, [fContainerAccess.Name])) );
  end;
end;

function TPyDelphiContainer.SqContains(obj: PPyObject): integer;
begin
  if ContainerAccess.SupportsIndexOf then
  begin
    if ContainerAccess.IndexOf(Obj) > -1 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSqContains, [fContainerAccess.Name])) );
  end;
end;

function TPyDelphiContainer.SqItem(idx: NativeInt): PPyObject;
begin
  if CheckIndex(idx, SqLength) then
    Result := ContainerAccess.GetItem(idx)
  else
    Result := nil;
end;

function TPyDelphiContainer.SqLength: NativeInt;
begin
  Result := ContainerAccess.GetSize;
end;

{ TPyDelphiIterator }

destructor TPyDelphiIterator.Destroy;
begin
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiIterator.Iter: PPyObject;
begin
  Result := GetSelf;
  GetPythonEngine.Py_XINCREF(Result);
end;

function TPyDelphiIterator.IterNext: PPyObject;
begin
  if fPosition >= ContainerAccess.GetSize then with GetPythonEngine do begin
    Result := nil;
    PyErr_SetString (PyExc_StopIteration^, 'StopIteration');
  end else begin
    Result := ContainerAccess.GetItem(fPosition);
    Inc(fPosition);
  end;
end;

function TPyDelphiIterator.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyUnicodeFromString( Format('<Delphi %sIterator at %x>',
         [ContainerAccess.Name, NativeInt(Self)]) );
end;

procedure TPyDelphiIterator.Setup(AContainerAccess : TContainerAccess);
begin
  Assert(Assigned(AContainerAccess));
  fContainerAccess := AContainerAccess;
  fPosition := 0;
end;

class procedure TPyDelphiIterator.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.Name := 'DefaultIterType';
  PythonType.TypeName := 'DelphiDefaultIterator';
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Iterator for Abstract Containers';
  PythonType.Services.Basic := [bsRepr, bsStr, bsIter, bsIterNext];
end;

{ TPyInterfacedObject }

function TPyInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TPyInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

function TPyInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ TPyDelphiObject }

function TPyDelphiObject.CheckBound: Boolean;
begin
  Result := Assigned(DelphiObject);
  if not Result then
    with GetPythonEngine do
      PyErr_SetObject(PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrCheckBound, [ClassName])));
end;

function TPyDelphiObject.Compare(obj: PPyObject): Integer;
Var
  PyObject : TPyObject;
begin
  if IsDelphiObject(obj) then begin
    PyObject := PythonToDelphi(obj);
    if PyObject is TPyDelphiObject then
      Result := Sign(NativeInt(TPyDelphiObject(PyObject).DelphiObject) - NativeInt(DelphiObject))
    else
      Result := -1;  // not equal
  end else
    Result := -1;  // not equal
end;

constructor TPyDelphiObject.Create(APythonType: TPythonType);
begin
  inherited;
  if Assigned(APythonType) and (APythonType.Owner is TPyDelphiWrapper) then
    PyDelphiWrapper := TPyDelphiWrapper(APythonType.Owner);
end;

function TPyDelphiObject.CreateContainerAccess: TContainerAccess;
var
  _ContainerAccessClass : TContainerAccessClass;
begin
  _ContainerAccessClass := GetContainerAccessClass;
  if Assigned(_ContainerAccessClass) then
    Result := _ContainerAccessClass.Create(PyDelphiWrapper, DelphiObject)
  else
    raise Exception.CreateFmt('Wrapper class %s does not provide a container access', [ClassName]);
end;

class function TPyDelphiObject.DelphiObjectClass: TClass;
begin
  Result := TObject;
end;

destructor TPyDelphiObject.Destroy;
begin
  DelphiObject := nil; // will free the object if owned
  fContainerAccess.Free;
  inherited;
end;

function TPyDelphiObject.Free_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, ':Free' ) <> 0 then
    begin
      if Assigned(fDelphiObject)then
      begin
        UnSubscribeToFreeNotification;
        FreeAndNil(fDelphiObject);
      end;
      Owned := False;
      Result := ReturnNone;
    end
    else
      Result := nil;
  end;
end;

{$IFDEF EXTENDED_RTTI}
procedure Rtti_Dir(SL: TStringList; RttiType: TRttiType);
var
  RttiMethod: TRttiMethod;
  RttiProperty: TRttiProperty;
  RttiField: TRttiField;
begin
  for RttiMethod in RttiType.GetMethods do
    if Ord(RttiMethod.Visibility) > Ord(mvProtected) then
      SL.Add(RttiMethod.Name);
  for RttiProperty in RttiType.GetProperties do
    if Ord(RttiProperty.Visibility) > Ord(mvProtected) then
      SL.Add(RttiProperty.Name);
  for RttiField in RttiType.GetFields do
    if Ord(RttiField.Visibility) > Ord(mvProtected) then
      SL.Add(RttiField.Name);
end;

function RttiCall(ParentAddress: pointer; PythonType: TPythonType;
  DelphiWrapper: TPyDelphiWrapper; MethName: string;
  ParentRtti: TRttiStructuredType; ob1, ob2: PPyObject;
  AParentAddrIsClass: boolean): PPyObject;

  function ParamAsDynArray(PyValue: PPyObject; const RttiParam: TRttiParameter; out ParamValue: TValue): Boolean;
  var
    Arr: array of TValue;
    I: Integer;
    elType: PPTypeInfo;
    V: Variant;
    Num: Int64;
  begin
    Result := False;
    if (RttiParam.ParamType = nil) or (RttiParam.ParamType.Handle = nil) or (GetTypeData(RttiParam.ParamType.Handle) = nil) then
      Exit;
    elType := GetTypeData(RttiParam.ParamType.Handle).elType;
    if elType = nil then
      elType := GetTypeData(RttiParam.ParamType.Handle).elType2;
    if elType = nil then
      Exit;

    SetLength(Arr, PythonType.Engine.PyList_Size(PyValue));
    for I := 0 to PythonType.Engine.PyList_Size(PyValue) - 1 do
    begin
      V := PythonType.Engine.PyObjectAsVariant(PythonType.Engine.PyList_GetItem(PyValue, i));
      if elType^.Kind = tkEnumeration then
      begin
        Num := TValue.FromVariant(V).Cast(TypeInfo(Int64)).AsInt64;
        Arr[i] := TValue.FromOrdinal(elType^, Num);
      end
      else
        Arr[i] := TValue.FromVariant(V).Cast(elType^);
    end;
    ParamValue := TValue.FromArray(RttiParam.ParamType.Handle, Arr);
    Result := True;
  end;

  function FindMethod(const MethName:string; RttiType : TRttiType;
    PyArgs: PPyObject; var Args: array of TValue):TRttiMethod;
  // Deals with overloaded methods
  // Constructs the Arg Array
  // PyArgs is a Python tuple
  Var
    Method: TRttiMethod;
    Index: Integer;
    ErrMsg: string;
    Obj: TObject;
    ClassRef: TClass;
    PyValue : PPyObject;
    Param: TRttiParameter;
    Params : TArray<TRttiParameter>;
    SearchContinue: Boolean;
  begin
    Result := nil;
    for Method in RttiType.GetMethods do
      if SameText(Method.Name, MethName) then
      begin
        Params := Method.GetParameters;
        if Length(Args) = Length(Params) then
        begin
          Result := Method;
          SearchContinue := False;
          for Index := 0 to Length(Params) - 1 do
          begin
            Param := Params[Index];
            if (Param.ParamType = nil) or
              (Param.Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> []) then
            begin
              Result := nil;
              SearchContinue := True;
              Break;
            end;

            PyValue := PythonType.Engine.PyTuple_GetItem(PyArgs, Index);
            if Param.ParamType = nil then
            begin
              Result := nil;
              Break
            end
            else if Param.ParamType.TypeKind = tkClass then
            begin
              if ValidateClassProperty(PyValue, Param.ParamType.Handle, Obj, ErrMsg)
              then
                Args[Index] := Obj
              else begin
                Result := nil;
                Break
              end
            end
            else if (Param.ParamType.TypeKind = tkClassRef) then
            begin
              if ValidateClassRef(PyValue, Param.ParamType.Handle, ClassRef, ErrMsg) then
                Args[Index] := ClassRef
              else begin
                Result := nil;
                Break
              end
            end
            else if (Param.ParamType.TypeKind = tkDynArray) and PythonType.Engine.PyList_Check(PyValue) then
            begin
              if ParamAsDynArray(PyValue, Param, Args[Index]) then
                Continue; //to avoid last check
            end
            else begin
              if not SimplePythonToValue(PyValue, Param.ParamType.Handle,
                Args[Index], ErrMsg) then
              begin
                Result := nil;
                Break
              end;
            end;

            if (Param.ParamType <> nil) and not Args[Index].IsType(Param.ParamType.Handle) then
            begin
              Result :=nil;
              Break;
            end;
          end; // for params

          if not SearchContinue then
            Break;
        end;
     end;
  end;

  procedure InvalidArguments(const MethName, ErrMsg : string);
  begin
    with GetPythonEngine do
      PyErr_SetObject(PyExc_TypeError^, PyUnicodeFromString(
        Format(rs_ErrInvalidArgs,
        [MethName, ErrMsg])));
  end;

Var
  Args: array of TValue;
  ArgCount: Integer;
  meth: TRttiMethod;
  ret: TValue;
  ErrMsg : string;
  Addr: TValue;

begin
  Result := nil;
  // Ignore keyword arguments ob2
  // ob1 is a tuple with zero or more elements

  ArgCount := PythonType.Engine.PyTuple_Size(ob1);
  SetLength(Args, ArgCount);

  meth := FindMethod(MethName, ParentRtti, ob1, Args);

  if not Assigned(meth) then begin
    InvalidArguments(MethName, rs_IncompatibleArguments);
    Exit;
  end;

  try
    if ParentRtti is TRttiInstanceType then
      if meth.IsClassMethod then
        Addr := TValue.From(TObject(ParentAddress).ClassType)
      else
        Addr := TValue.From(TObject(ParentAddress))
    else if ParentRtti is TRttiInterfaceType then
       TValue.Make(@ParentAddress, ParentRtti.Handle, Addr)
    else
      Addr := TValue.From(ParentAddress);
    ret := meth.Invoke(Addr, Args);
    if ret.IsEmpty then
      Result := GetPythonEngine.ReturnNone
    else if ret.Kind = tkClass then
      Result := DelphiWrapper.Wrap(ret.AsObject)
    else if ret.Kind = tkClassRef then
      Result := DelphiWrapper.WrapClass(ret.AsClass)
    else begin
      Result := SimpleValueToPython(ret, ErrMsg);
      if Result = nil then
        with PythonType.Engine do
          PyErr_SetObject(PyExc_TypeError^, PyUnicodeFromString(
            Format(rs_ErrInvalidRet, [MethName, ErrMsg])));
    end;
  except
    on E: Exception do begin
      Result := nil;
      InvalidArguments(MethName, E.Message);
    end;
  end;
end;

function GetRttiAttr(ParentAddr: Pointer; ParentType: TRttiStructuredType;
  const AttrName: string; PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): PPyObject;
var
  Prop: TRttiProperty;
  Meth: TRttiMethod;
  Field: TRttiField;
begin
  Result := nil;

  try
    Meth := ParentType.GetMethod(AttrName);
    if Meth <> nil then
    begin
      Result := PyDelphiWrapper.DelphiMethodType.CreateInstance;
      with PythonToDelphi(Result) as TPyDelphiMethodObject do
      begin
        fDelphiWrapper := PyDelphiWrapper;
        MethName := Meth.Name;
        ParentRtti := ParentType;
        ParentAddress := ParentAddr;
      end;
    end
    else
    begin
      Prop := ParentType.GetProperty(AttrName);
      if Prop <> nil then
      begin
        if Ord(Prop.Visibility) < Ord(mvPublic) then
          ErrMsg := rs_NoAccess
        else if not Prop.IsReadable then
          ErrMsg := rs_NotReadable
        else if Prop.PropertyType = nil then
          ErrMsg := rs_ErrNoTypeInfo
        else
        case Prop.PropertyType.TypeKind of
          tkClass:
            Result := PyDelphiWrapper.Wrap(Prop.GetValue(ParentAddr).AsObject);
          tkInterface:
            Result := PyDelphiWrapper.WrapInterface(Prop.GetValue(ParentAddr));
          tkMethod:
            if (ParentType is TRttiInstanceType) and (Prop is TRttiInstanceProperty) then
              Result := PyDelphiWrapper.fEventHandlerList.GetCallable(TObject(ParentAddr),
                TRttiInstanceProperty(Prop).PropInfo);
        else
          Result := SimpleValueToPython(Prop.GetValue(ParentAddr), ErrMsg)
        end;
      end
      else
      begin
        Field := ParentType.GetField(AttrName);
        if Field <> nil then
        begin
          if Ord(Field.Visibility) < Ord(mvPublic) then
            ErrMsg := rs_NoAccess
          else if Field.FieldType = nil then
            ErrMsg := rs_ErrNoTypeInfo
          else
          case Field.FieldType.TypeKind of
            tkClass:
              Result := PyDelphiWrapper.Wrap(Field.GetValue(ParentAddr).AsObject);  // Returns None if Field is nil
            tkInterface:
              Result := PyDelphiWrapper.WrapInterface(Field.GetValue(ParentAddr));
            tkRecord:
              if Field.FieldType is TRttiStructuredType then
                //Result := PyDelphiWrapper.WrapRecord(Pointer(PPByte(ParentAddr)^ + Field.Offset),  TRttiStructuredType(Field.FieldType));
                Result := PyDelphiWrapper.WrapRecord(PByte(ParentAddr) + Field.Offset,  TRttiStructuredType(Field.FieldType));
          else
            Result := SimpleValueToPython(Field.GetValue(ParentAddr), ErrMsg)
          end;
        end
        else
          ErrMsg := rs_UnknownAttribute;
      end;
    end;
  except
    on E: Exception do begin
      Result := nil;
      ErrMsg := E.Message;
    end;
  end;
end;

function SetRttiAttr(const ParentAddr: Pointer;  ParentType: TRttiStructuredType;
  const AttrName: string; Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean;
var
  Prop: TRttiProperty;
  Field: TRttiField;
  V: TValue;
  Obj: TObject;
  ValueOut: TValue;
begin
  Result := False;

  Prop := ParentType.GetProperty(AttrName);
  if Prop <> nil then
    try
      if Ord(Prop.Visibility) < Ord(mvPublic) then
        ErrMsg := rs_NoAccess
      else if not Prop.IsWritable then
        ErrMsg := rs_NotWritable
      else if Prop.PropertyType = nil then
        ErrMsg := rs_ErrNoTypeInfo
      else
      case Prop.PropertyType.TypeKind of
        tkClass:
          if ValidateClassProperty(Value, Prop.PropertyType.Handle, Obj, ErrMsg) then begin
            Prop.SetValue(ParentAddr, Obj);
            Result := True;
          end;
        tkInterface:
          if ValidateInterfaceProperty(Value, Prop.PropertyType as TRttiInterfaceType, ValueOut, ErrMsg) then begin
            Prop.SetValue(ParentAddr, ValueOut);
            Result := True;
          end;
        tkRecord:
          if ValidateRecordProperty(Value, Prop.PropertyType.Handle, ValueOut, ErrMsg) then begin
            Prop.SetValue(ParentAddr, ValueOut);
            Result := True;
          end;
        tkMethod:
          if Prop.Visibility = mvPublished then
            Result := PyDelphiWrapper.EventHandlers.Link(TObject(ParentAddr),
              (Prop as TRttiInstanceProperty).PropInfo, Value, ErrMsg)
          else
            ErrMsg := rs_NotPublished;
      else
        begin
          Result := SimplePythonToValue(Value, Prop.PropertyType.Handle, V, ErrMsg);
          if Result then
            Prop.SetValue(ParentAddr, V);
        end;
      end;
    except
      on E: Exception do begin
        Result := False;
        ErrMsg := E.Message;
      end;
    end
  else
  begin
    Field := ParentType.GetField(AttrName);
    if Field <> nil then
      try
        if Ord(Field.Visibility) < Ord(mvPublic) then
          ErrMsg := rs_NoAccess
        else if Field.FieldType = nil then
          ErrMsg := rs_ErrNoTypeInfo
        else
        case Field.FieldType.TypeKind of
          tkClass:
            if ValidateClassProperty(value, Field.FieldType.Handle, Obj, ErrMsg) then begin
              Field.SetValue(ParentAddr, Obj);
              Result := True;
            end;
          tkInterface:
            if ValidateInterfaceProperty(Value, Field.FieldType as TRttiInterfaceType, ValueOut, ErrMsg) then begin
              Field.SetValue(ParentAddr, ValueOut);
              Result := True;
            end;
          tkRecord:
            if ValidateRecordProperty(Value, Field.FieldType.Handle, ValueOut, ErrMsg) then begin
              Field.SetValue(ParentAddr, ValueOut);
              Result := True;
            end;
        else
          begin
            Result := SimplePythonToValue(Value, Field.FieldType.Handle, V, ErrMsg);
            if Result then
              Field.SetValue(ParentAddr, V);
          end;
        end;
      except
        on E: Exception do begin
          Result := False;
          ErrMsg := E.Message;
        end;
      end
  end;
end;

{ TPyRttiObject }

constructor TPyRttiObject.Create(APythonType: TPythonType);
begin
  inherited;
  if Assigned(APythonType) and (APythonType.Owner is TPyDelphiWrapper) then
    PyDelphiWrapper := TPyDelphiWrapper(APythonType.Owner);
end;

function TPyRttiObject.Dir_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
  SL : TStringList;
begin
  Adjust(@Self);
  SL := TStringList.Create;
  SL.Sorted := True;
  SL.Duplicates := dupIgnore;
  try
    // Add methods
    for i := 0 to PythonType.MethodCount - 1 do
      SL.Add(string(AnsiString(PythonType.Methods[i].ml_name)));
    for i := 0 to PythonType.GetSetCount - 1 do
      SL.Add(string(AnsiString(PythonType.GetSet[i].name)));
    Rtti_Dir(SL, RttiType);
    Result := GetPythonEngine.StringsToPyList(SL);
  finally
    SL.Free;
  end;
end;

function TPyRttiObject.GetAttrO(key: PPyObject): PPyObject;
var
  KeyName: string;
  ErrMsg : string;
begin
  Result := nil;
  if (fAddr <> nil) and GetPythonEngine.PyUnicode_Check(Key) then
    KeyName := GetPythonEngine.PyUnicodeAsString(Key)
  else
    Exit;

  if Assigned(RttiType) then
    Result := GetRttiAttr(fAddr, RttiType, KeyName, PyDelphiWrapper, ErrMsg);
  if not Assigned(Result) then
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrAttrGet,[KeyName, ErrMsg])));
end;

class procedure TPyRttiObject.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethodWithKeywords('SetProps', @TPyRttiObject.SetProps,
    'TObject.SetProps(prop1=val1, prop2=val2...)'#10 +
    'Sets several properties in one call');
  PythonType.AddMethod('__dir__', @TPyRttiObject.Dir_Wrapper,
    'Returns the list of all methods, fields and properties of this instance.');
end;

function TPyRttiObject.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(
   Format('<Delphi record of type %s at %x>',
   [RttiType.Name, NativeInt(Self)]) )
end;

function TPyRttiObject.SetAttrO(key, value: PPyObject): Integer;
var
   KeyName: string;
   ErrMsg: string;
begin
  Result := -1;
  if (fAddr <> nil) and GetPythonEngine.PyUnicode_Check(Key) then
    KeyName := GetPythonEngine.PyUnicodeAsString(Key)
  else begin
    Exit;
  end;

  if SetRttiAttr(fAddr, RttiType, KeyName, Value, PyDelphiWrapper, ErrMsg) then
      Result := 0;

  if Result <> 0 then
    with GetPythonEngine do
      PyErr_SetObject(PyExc_AttributeError^, PyUnicodeFromString(
        Format(rs_ErrAttrSetr, [KeyName, ErrMsg])));
end;

function TPyRttiObject.SetProps(args, keywords: PPyObject): PPyObject;
begin
  Adjust(@Self);
  Result := SetProperties(GetSelf, keywords);
end;

class procedure TPyRttiObject.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'RttiObject';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Wrapper of a Pascal record';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr];
end;

procedure TPyRttiObject.SetAddrAndType(Address: Pointer; Typ: TRttiStructuredType);
begin
  fAddr := Address;
  Assert(Assigned(Typ));
  Assert((Typ is TRttiRecordType) or (Typ is TRttiInterfaceType));
  fRttiType := Typ;
end;

{ TPyPascalRecord }

function TPyPascalRecord.GetValue: TValue;
begin
   TValue.Make(fAddr, RttiType.Handle, Result);
end;

class procedure TPyPascalRecord.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'PascalRecord';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
end;

{ TPyPascalInterface }

function TPyPascalInterface.GetValue: TValue;
begin
   TValue.Make(@fAddr, RttiType.Handle, Result);
end;

class procedure TPyPascalInterface.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'PascalInterface';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
end;

{$ENDIF}

function TPyDelphiObject.GetAttrO(key: PPyObject): PPyObject;
(*
    First look whether the attribute has ben wrapped (RegisterGetSet, RegisterMethod).
    This is done by calling the inherited GetAttrO.  If this fails then
      -  Use Rtti to locate the property in DELPHIXE_OR_HIGHER (EXTENDED_RTTI)
      or for other versions
      -  First Look for "published" methods compiled with {$METHODINFO ON}
      -  Then look for published properties
*)

var
  KeyName: string;
  ErrMsg : string;
  {$IFNDEF EXTENDED_RTTI}
  {$IFNDEF FPC}
  Info: PMethodInfoHeader;
  {$ENDIF}
  PropInfo: PPropInfo;
  Obj : TObject;
  {$ELSE}
  Context: TRttiContext;
  RttiType: TRttiStructuredType;
  {$ENDIF}
begin
  Result := inherited GetAttrO(key);
  if GetPythonEngine.PyErr_Occurred = nil then Exit;  // We found what we wanted

  // should not happen
  if not (Assigned(DelphiObject) and
     CheckStrAttribute(Key, 'GetAttrO key parameter', KeyName))
  then
    Exit;

  GetPythonEngine.PyErr_Clear;
{$IFDEF EXTENDED_RTTI}
  // Use RTTI
  if Assigned(DelphiObject) then begin
    Context := TRttiContext.Create();
    try
      RttiType := Context.GetType(DelphiObject.ClassType) as TRttiStructuredType;
      if Assigned(RttiType) then
        Result := GetRttiAttr(DelphiObject, RttiType, KeyName, PyDelphiWrapper, ErrMsg);
    finally
      Context.Free;
    end;
  end;
{$ELSE}
{$IFNDEF FPC}
  if Assigned(DelphiObject) then
    Info := GetMethodInfo(DelphiObject, KeyName)
  else
    Info := nil;

  if Info <> nil then // We have a method
  begin
    // Ensure the method information has enough type information
    if Info.Len <= SizeOf(Info^) - SizeOf(ShortString) + 1 + Length(Info.Name) then
    begin
      ErrMsg := rs_UnknownAttribute;
    end
    else
    begin
      Result := PyDelphiWrapper.DelphiMethodType.CreateInstance;
      with PythonToDelphi(Result) as TPyDelphiMethodObject do begin
        MethodInfo := Info^;
        DelphiObject := self.DelphiObject;
      end;
    end;
  end
  else{$ENDIF} if Assigned(DelphiObject) then
  try
    // Not a  method, try a property.
    PropInfo := GetPropInfo(DelphiObject, KeyName);
    if PropInfo <> nil then
    begin
      // we have a property
      if PropInfo^.PropType^.Kind = tkClass then begin
        Obj := TObject(GetOrdProp(Self.DelphiObject, PropInfo));
        Result := Wrap(Obj);
      end else if PropInfo^.PropType^.Kind = tkMethod then begin
        Result := PyDelphiWrapper.fEventHandlerList.GetCallable(Self.DelphiObject, PropInfo)
      end else if PropInfo^.PropType^.Kind = tkSet then begin
        Result := SetToPython(Self.DelphiObject, PropInfo)
      end else if PropInfo^.PropType^.Kind = tkEnumeration then begin
      begin
        {$IFDEF FPC}
        if GetTypeData(PropInfo^.PropType)^.BaseType = TypeInfo(Boolean) then
        {$ELSE FPC}
        if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
        {$ENDIF FPC}
          Result := GetPythonEngine.VariantAsPyObject(Boolean(GetOrdProp(Self.DelphiObject, PropInfo)))
        else
        {$IFDEF FPC}
          Result := GetPythonEngine.PyUnicodeFromString(GetEnumName(PropInfo^.PropType,
        {$ELSE FPC}
          Result := GetPythonEngine.PyUnicodeFromString(GetEnumName(PropInfo^.PropType^,
        {$ENDIF FPC}
            GetOrdProp(Self.DelphiObject, PropInfo)));
      end
      end else
         Result := GetPythonEngine.VariantAsPyObject(GetPropValue(DelphiObject, PropInfo));
    end;
  except
    on E: Exception do begin
      Result := nil;
      ErrMsg := E.Message;
    end;
  end;
{$ENDIF}
  if not Assigned(Result) then
    with GetPythonEngine do
      PyErr_SetObject (PyExc_AttributeError^,
        PyUnicodeFromString(Format(rs_ErrAttrGet,[KeyName, ErrMsg])));
end;

function TPyDelphiObject.GetContainerAccess: TContainerAccess;
begin
  if not Assigned(fContainerAccess) then
    fContainerAccess := CreateContainerAccess;
  Result := fContainerAccess;
end;

class function TPyDelphiObject.GetContainerAccessClass : TContainerAccessClass;
begin
  Result := nil;
end;

function TPyDelphiObject.Get_Bound(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(Assigned(DelphiObject));
end;

function TPyDelphiObject.Get_ClassName(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  if CheckBound then
    Result := GetPythonEngine.PyUnicodeFromString(DelphiObject.ClassName)
  else
    Result := nil;
end;

function TPyDelphiObject.Get_Owned(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  if CheckBound then
    Result := GetPythonEngine.VariantAsPyObject(Owned)
  else
    Result := nil;
end;

function TPyDelphiObject.Dir_Wrapper(args: PPyObject): PPyObject;
var
  SL : TStringList;
  PyEngine: TPythonEngine;

  procedure AddItemsFromDict(PyObj: PPyObject);
  var
    PyDict: PPyObject;
    PyList: PPyObject;
  begin
    if PyEngine.PyObject_HasAttrString(PyObj, '__dict__') = 1 then
    begin
      PyDict := PyEngine.PyObject_GetAttrString(PyObj, '__dict__');
      PyList := PyEngine.PyMapping_Keys(PyDict);
      if Assigned(PyList) then
        PyEngine.PyListToStrings(PyList, SL, False);
      PyEngine.Py_XDECREF(PyList);
      PyEngine.Py_XDECREF(PyDict);
    end;
  end;

var
  PyType: PPyTypeObject;
{$IFDEF EXTENDED_RTTI}
  Context: TRttiContext;
  RttiType: TRTTIType;
{$ELSE}
  _PropList: PPropList;
  _propCount : Integer;
{$ENDIF}
begin
  Adjust(@Self);
  SL := TStringList.Create;
  SL.Sorted := True;
  SL.Duplicates := dupIgnore;
  PyEngine := GetPythonEngine;
  try
    AddItemsFromDict(GetSelf);
    PyType := PythonType.TheTypePtr;
    while PyType <> nil do
    begin
      AddItemsFromDict(PPyObject(PyType));
      PyType := PyType.tp_base;
    end;

{$IFDEF EXTENDED_RTTI}
    Context := TRttiContext.Create();
    try
      RttiType := Context.GetType(DelphiObject.ClassType);
      Rtti_Dir(SL, RttiType);
    finally
      Context.Free();
    end;
{$ELSE}
    _propCount := GetPropList(DelphiObject, _PropList);
    if _propCount > 0  then
      try
        for i := 0 to _propCount - 1 do
          SL.Add(_PropList^[i].Name);
      finally
        FreeMem(_PropList);
      end;
{$ENDIF}
    Result := GetPythonEngine.StringsToPyList(SL);
  finally
    SL.Free;
  end;
end;

class function TPyDelphiObject.GetTypeName : string;
begin
  Result := Copy(DelphiObjectClass.ClassName, 2, MaxInt);
end;

function TPyDelphiObject.HasContainerAccessClass: Boolean;
begin
  Result := GetContainerAccessClass <> nil;
end;

function TPyDelphiObject.InheritsFrom_Wrapper(args: PPyObject): PPyObject;
var
  _obj : PPyObject;
  AClass: TClass;
  KlassName: string;
  IsSubClass: Boolean;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, 'O:InheritsFrom',@_obj ) <> 0 then begin
      if CheckBound then begin
        KlassName := PyObjectAsString(_obj);
        AClass := DelphiObject.ClassType;
        repeat
          IsSubClass := SameText(AClass.ClassName, KlassName);
          if IsSubClass then Break;
          AClass := AClass.ClassParent;
        until AClass = nil;
        if IsSubClass then
          Result := PPyObject(Py_True)
        else
          Result := PPyObject(Py_False);
        Py_INCREF( Result );
      end else
        Result := nil;
    end else
      Result := nil;
  end;
end;

function TPyDelphiObject.Iter: PPyObject;
begin
  if HasContainerAccessClass then
  begin
    Result := PyDelphiWrapper.DefaultIterType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiIterator do
      Setup(Self.ContainerAccess.Clone);
  end
  else
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrIterSupport,
        [Self.ClassName])) );
  end;
end;

procedure TPyDelphiObject.Notify(ADeletedObject: TObject);
begin
  if fDelphiObject = ADeletedObject then
    fDelphiObject := nil;
end;

class procedure TPyDelphiObject.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;

  // then register TObject + custom getters/setters.
  with PythonType do
    begin
      AddGetSet('ClassName', @TPyDelphiObject.Get_ClassName, nil,
        'Returns the TObject.ClassName', nil);
      AddGetSet('__bound__', @TPyDelphiObject.Get_Bound, nil,
        'Returns True if the wrapper is still bound to the Delphi instance.', nil);
      AddGetSet('__owned__', @TPyDelphiObject.Get_Owned, @TPyDelphiObject.Set_Owned,
        'Returns True if the wrapper owns the Delphi instance.', nil);
    end;
end;

class procedure TPyDelphiObject.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethodWithKeywords('SetProps', @TPyDelphiObject.SetProps,
    'TObject.SetProps(prop1=val1, prop2=val2...)'#10 +
    'Sets several properties in one call');
  PythonType.AddMethod('Free', @TPyDelphiObject.Free_Wrapper,
    'TObject.Free()'#10 +
    'Frees the Wrapped Delphi Object');
  PythonType.AddMethod('InheritsFrom', @TPyDelphiObject.InheritsFrom_Wrapper,
    'TObject.InheritsFrom(ClassName)'#10 +
    'Returns True if Delphi Object is or inherits from ClassName');
  PythonType.AddMethod('ToTuple', @TPyDelphiObject.ToTuple_Wrapper,
    'TStrings.ToTuple()'#10 +
    'If the object is a container (TStrings, TComponent...), it returns the content of the sequence as a Python tuple object.');
  PythonType.AddMethod('ToList', @TPyDelphiObject.ToList_Wrapper,
    'TStrings.ToList()'#10 +
    'If the object is a container (TStrings, TComponent...), it returns the content of the sequence as a Python list object.');
  PythonType.AddMethod('__dir__', @TPyDelphiObject.Dir_Wrapper,
    'Returns the list of all methods, fields and properties of this instance.');
end;

function TPyDelphiObject.Repr: PPyObject;
begin
  with GetPythonEngine do
    if Assigned(DelphiObject) then
      Result := PyUnicodeFromString( Format('<Delphi object of type %s at %x>',
           [DelphiObject.ClassName, NativeInt(Self)]) )
    else
      Result := PyUnicodeFromString( Format('<Unbound Delphi wrapper of type %s at %x>',
           [DelphiObjectClass.ClassName, NativeInt(Self)]) );
end;

function TPyDelphiObject.SetAttrO(key, value: PPyObject): Integer;
(*
    First look whether the attribute exists., e.g. has been wrapped with
    RegisterGetSet, RegisterMethod, etc.
    If it does then the inherited generic SetAttrO is called.
    If the attribute does not exist or the generic SetAttO fails (unlikely) then
      -  Use Rtti to locate the property in DELPHIXE_OR_HIGHER (EXTENDED_RTTI)
      or for other versions
      -  Look for published properties
    Finally, if all the above fail then you call the inherited generic SetAttrO
    which adds a new field in the object dictionary
*)

  function HandleEvent(PropInfo: PPropInfo; out ErrMsg: string) : Integer;
  begin
    if PyDelphiWrapper.EventHandlers.Link(DelphiObject, PropInfo, value, ErrMsg) then
      Result := 0
    else
      Result := -1;
  end;

  function HandleClass(PropInfo: PPropInfo; out ErrMsg: string) : Integer;
  Var
    Obj : TObject;
  begin
    Result := -1;
    if ValidateClassProperty(value, PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}, Obj, ErrMsg) then
    begin
      SetOrdProp(DelphiObject, PropInfo, NativeInt(Obj));
      Result := 0;
    end
  end;

  function HandleSet(PropInfo: PPropInfo; out ErrMsg: string) : Integer;
  begin
    try
      SetPropValue(DelphiObject, PropInfo, PythonToSet(PropInfo, Value));
      Result := 0;
    except
      on E: Exception do with GetPythonEngine do
      begin
        Result := -1;
        ErrMsg := E.Message;
      end;
    end;
  end;

  function HandleOtherTypes(PropInfo: PPropInfo; out ErrMsg: string) : Integer;
  Var
    V : Variant;
  begin
    try
      V := GetPythonEngine.PyObjectAsVariant(Value);
      if (PropInfo.PropType^.Kind = tkEnumeration) and (VarType(V) = varOleStr) then
        // Special case that occurs in Python3000
        V := VarAsType(V, varString);  //Downcast to string
      SetPropValue(DelphiObject, PropInfo, V);
      Result := 0;
    except
      on E: Exception do with GetPythonEngine do
      begin
        Result := -1;
        ErrMsg := E.Message;
      end;
    end;
  end;

var
  {$IFNDEF EXTENDED_RTTI}
  PropInfo: PPropInfo;
  {$ELSE}
  Context: TRttiContext;
  RttiType: TRttiStructuredType;
  {$ENDIF}
  KeyName: string;
  ErrMsg: string;
  PyEngine: TPythonEngine;
  PyObj: PPyobject;
begin
  Result := -1;
  PyEngine := GetPythonEngine;

  // should not happen
  if not (Assigned(DelphiObject) and
     CheckStrAttribute(Key, 'SetAttrO key parameter', KeyName))
  then
    Exit;

  // Only call the inherited method at this stage if the attribute exists
  PyObj := PyEngine.PyObject_GenericGetAttr(GetSelf, key);
  if Assigned(PyObj) then
  begin
    PyEngine.Py_DECREF(PyObj); // not needed
    Result := inherited SetAttrO(key, value);
    if Result = 0 then
      Exit;
  end;

  PyEngine.PyErr_Clear;
  {$IFDEF EXTENDED_RTTI}
  Context := TRttiContext.Create();
  try
    RttiType := Context.GetType(DelphiObject.ClassType) as TRttiStructuredType;
    if SetRttiAttr(DelphiObject, RttiType, KeyName, Value, PyDelphiWrapper, ErrMsg) then
    Result := 0;
  finally
    Context.Free;
  end;
  {$ELSE}
  PropInfo := GetPropInfo(DelphiObject, KeyName);
  if PropInfo <> nil then
  begin
    if PropInfo^.PropType^.Kind = tkMethod then
      Result := HandleEvent(PropInfo, ErrMsg)
    else if PropInfo^.PropType^.Kind = tkClass then
      Result := HandleClass(PropInfo, ErrMsg)
    else if PropInfo^.PropType^.Kind = tkSet then
      Result := HandleSet(PropInfo, ErrMsg)
    else
      Result := HandleOtherTypes(PropInfo, ErrMsg);
  end;
  {$ENDIF}
  //  Subclasses have a __dict__ and can set extra fields
  if Result <> 0 then
    Result := inherited SetAttrO(key, value);
  if Result <> 0 then
    with PyEngine do
      PyErr_SetObject(PyEngine.PyExc_AttributeError^, PyUnicodeFromString(
        Format(rs_ErrAttrSetr, [KeyName, ErrMsg])));
end;

procedure TPyDelphiObject.SetDelphiObject(const Value: TObject);
begin
  if fDelphiObject <> Value then
  begin
    if Assigned(Value) then
      Assert(Value.InheritsFrom(DelphiObjectClass));
    if Assigned(fDelphiObject)then
    begin
      UnSubscribeToFreeNotification;
      if Owned then
        fDelphiObject.Free;
    end;
    fDelphiObject := Value;
    if Assigned(fDelphiObject) then
      SubscribeToFreeNotification;
  end;
end;

function SetProperties(PyObject: PPyObject; keywords: PPyObject): PPyObject;
var
  i : Integer;
  _key : PPyObject;
  _keys : PPyObject;
  _value : PPyObject;
begin
  Result := nil;
  with GetPythonEngine do
  begin
    _keys := PyDict_Keys(keywords);
    try
      for i := 0 to PySequence_Length(_keys)-1 do
      begin
        _key := PySequence_GetItem(_keys, i);
        if _key = nil then
          Exit;
        try
          _value := PyDict_GetItem(keywords, _key); // returns a borrowed reference
          if PyObject_SetAttr(PyObject, _key, _value) = -1 then
            Exit;
        finally
          Py_DECREF(_key);
        end;
      end;
    finally
      Py_DECREF(_keys);
    end;
    Result := ReturnNone;
  end;
end;

function TPyDelphiObject.SetProps(args, keywords: PPyObject): PPyObject;
begin
  Adjust(@Self);
  Result := SetProperties(GetSelf, keywords);
end;

class procedure TPyDelphiObject.SetupType(PythonType: TPythonType);
var
  _ContainerAccessClass : TContainerAccessClass;
begin
  inherited;
  PythonType.TypeName := AnsiString(GetTypeName);
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Wrapper for Delphi ' + DelphiObjectClass.ClassName;
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
  _ContainerAccessClass := GetContainerAccessClass;
  if Assigned(_ContainerAccessClass) then
  begin
    PythonType.Services.Basic := PythonType.Services.Basic + [bsIter];
    PythonType.Services.Sequence := PythonType.Services.Sequence + [ssLength, ssItem];
    if _ContainerAccessClass.SupportsWrite then
      PythonType.Services.Sequence := PythonType.Services.Sequence + [ssAssItem];
    if _ContainerAccessClass.SupportsIndexOf then
      PythonType.Services.Sequence := PythonType.Services.Sequence + [ssContains];
  end;
end;

function TPyDelphiObject.Set_Owned(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, '__owned__', _value) then
  begin
    Owned := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiObject.SqAssItem(idx: NativeInt; obj: PPyObject): integer;
begin
  if HasContainerAccessClass then
  begin
    if not CheckIndex(idx, GetContainerAccess.GetSize) then
      Result := -1
    else
      if GetContainerAccess.SetItem(idx, obj) then
        Result := 0
      else
        Result := -1;
  end
  else
  begin
    Result := -1;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSqAss, [Self.ClassName])) );
  end;
end;

function TPyDelphiObject.SqContains(obj: PPyObject): integer;
begin
  if HasContainerAccessClass then
  begin
    if ContainerAccess.IndexOf(obj) > -1 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

function TPyDelphiObject.SqItem(idx: NativeInt): PPyObject;
begin
  if HasContainerAccessClass then
  begin
    if not CheckIndex(idx, ContainerAccess.GetSize) then
      Result := nil
    else
      Result := ContainerAccess.GetItem(idx);
  end
  else
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSequence, [Self.ClassName])) );
  end;
end;

function TPyDelphiObject.SqLength: NativeInt;
begin
  if HasContainerAccessClass then
    Result := ContainerAccess.GetSize
  else
    Result := 0;
end;

procedure TPyDelphiObject.SubscribeToFreeNotification;
var
  _FreeNotification : IFreeNotification;
begin
  Assert(Assigned(fDelphiObject));
  if fDelphiObject.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.Subscribe(Self);
end;

function TPyDelphiObject.ToList_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if PythonType.Services.Sequence * [ssItem, ssLength] <> [ssItem, ssLength] then
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSequence, [Self.ClassName])) );
  end
  else if GetPythonEngine.PyArg_ParseTuple( args, ':ToList' ) <> 0 then
    with GetPythonEngine do
    begin
      Result := PyList_New(SqLength);
      for i := 0 to PyList_Size(Result)-1 do
        PyList_SetItem(Result, i, SqItem(i));
    end
  else
    Result := nil;
end;

function TPyDelphiObject.ToTuple_Wrapper(args: PPyObject): PPyObject;
var
  i : Integer;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  if PythonType.Services.Sequence * [ssItem, ssLength] <> [ssItem, ssLength] then
  begin
    Result := nil;
    with GetPythonEngine do
      PyErr_SetObject( PyExc_SystemError^,
        PyUnicodeFromString(Format(rs_ErrSequence, [Self.ClassName])) );
  end
  else if GetPythonEngine.PyArg_ParseTuple( args, ':ToTuple' ) <> 0 then
    with GetPythonEngine do
    begin
      Result := PyTuple_New(SqLength);
      for i := 0 to PyTuple_Size(Result)-1 do
        PyTuple_SetItem(Result, i, SqItem(i));
    end
  else
    Result := nil;
end;

procedure TPyDelphiObject.UnSubscribeToFreeNotification;
var
  _FreeNotification : IFreeNotification;
begin
  Assert(Assigned(fDelphiObject));
  if fDelphiObject.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.UnSubscribe(Self);
end;

function TPyDelphiObject.Wrap(AObject: TObject;
  AOwnership: TObjectOwnership): PPyObject;
begin
  Result := PyDelphiWrapper.Wrap(AObject, AOwnership);
end;

{$IFNDEF FPC}
{ TPyDelphiMethodObject }

{$IFDEF EXTENDED_RTTI}
function TPyDelphiMethodObject.Call(ob1, ob2: PPyObject): PPyObject;
begin
  Result := RttiCall(ParentAddress, PythonType, fDelphiWrapper, MethName, ParentRtti, ob1, ob2);
end;

{$ELSE)}
function TPyDelphiMethodObject.Call(ob1, ob2: PPyObject): PPyObject;
Var
  V : Variant;
  ParamIndexes: array of Integer;
  Params: array of Variant;
  i, ArgCount: integer;
begin
  //  Ignore keyword arguments ob2
  // ob1 is a tuple with zero or more elements
  V := GetPythonEngine.PyObjectAsVariant(ob1);
  //  V is a VarArray

  ArgCount :=  VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
  SetLength(ParamIndexes, 0);
  SetLength(Params, ArgCount);

  // Params should contain arguments in reverse order!
  for i := 0 to ArgCount - 1 do
    Params[i] := VarArrayGet(V, [ArgCount - i - 1]);

  with GetPythonEngine do
    try
      Result := VariantAsPyObject(ObjectInvoke(DelphiObject,
        @MethodInfo, ParamIndexes, Params));
    except
      on E: Exception do
      begin
        Result := nil;
        PyErr_SetObject (PyExc_TypeError^,
          PyUnicodeFromString(Format(rs_ErrInvalidArgs,
          [MethodInfo.Name, E.Message])));
      end;
    end;
end;
{$ENDIF}

function TPyDelphiMethodObject.Repr: PPyObject;
begin
  with GetPythonEngine do
    Result := PyUnicodeFromString(
      Format('<Delphi method %s of type %s at %x>',
         [
           {$IFDEF EXTENDED_RTTI}
           MethName, ParentRtti.Name, NativeInt(Self)
           {$ELSE}
           MethodInfo.Name, DelphiObject.ClassName, NativeInt(Self)
           {$ENDIF}
         ]) );
end;

class procedure TPyDelphiMethodObject.SetupType( PythonType : TPythonType );
begin
  inherited;
  PythonType.Name := 'DelphiMethodType';
  PythonType.TypeName := 'DelphiMethod';
  PythonType.DocString.Text := 'Wrapper for Delphi methods';
  PythonType.Services.Basic := [bsRepr, bsStr, bsCall];
  PythonType.GenerateCreateFunction := False;
end;

{$ENDIF}

{ TPyDelphiVarParameter }

destructor TPyDelphiVarParameter.Destroy;
begin
  Value := nil;
  inherited;
end;

function TPyDelphiVarParameter.Get_Value(Acontext: Pointer): PPyObject;
begin
  Adjust(@Self);
  with GetPythonEngine do
  begin
    Result := Self.Value;
    if not Assigned(Result) then
      Result := Py_None;
    Py_XIncRef(Result);
  end;
end;

class procedure TPyDelphiVarParameter.RegisterGetSets(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Value', @TPyDelphiVarParameter.Get_Value, @TPyDelphiVarParameter.Set_Value,
        'Provides access to the Value associated with the Var parameter', nil);
    end;
end;

function TPyDelphiVarParameter.Repr: PPyObject;
var
  _value : PPyObject;
begin
  if not Assigned(Value) then
    _value := GetPythonEngine.ReturnNone
  else
    _value := GetPythonEngine.PyObject_Repr(Value);
  try
    Result :=
      GetPythonEngine.PyUnicodeFromString(Format('<VarParameter containing: %s>',
      [GetPythonEngine.PyObjectAsString(_value)]));
  finally
    GetPythonEngine.Py_DECREF(_value);
  end;
end;

function TPyDelphiVarParameter.RichCompare(obj: PPyObject;
  Op: TRichComparisonOpcode): PPyObject;
var
  _value : PPyObject;
begin
  with GetPythonEngine do
  begin
    if Self.Value = nil then
      _value := Py_None
    else
      _value := Self.Value;
    Result := PyObject_RichCompare(_value, obj, Ord(Op));
  end;
end;

class procedure TPyDelphiVarParameter.SetupType(PythonType: TPythonType);
begin
  inherited;
  PythonType.TypeName := 'VarParameter';
  PythonType.Name := string(PythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Container object allowing modification of Delphi var parameters from Python';
  PythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
end;

procedure TPyDelphiVarParameter.SetValue(const Value: PPyObject);
begin
  if (fValue <> Value) and (Value <> GetSelf) then
  begin
    if Assigned(Value) and (Value.ob_type = PythonType.TheTypePtr) then // don't let embedding a var param into another one.
      Exit;
    if Assigned(fValue) and PythonOK then
      GetPythonEngine.Py_DECREF(fValue);
    fValue := Value;
    if PythonOK then
      GetPythonEngine.Py_XINCREF(fValue);
  end;
end;

function TPyDelphiVarParameter.Set_Value(AValue: PPyObject;
  Acontext: Pointer): Integer;
begin
  Adjust(@Self);
  Self.Value := AValue;
  Result := 0;
end;

{ TEventHandler }

constructor TEventHandler.Create(PyDelphiWrapper : TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  _FreeNotification : IFreeNotification;
  _Valid : Boolean;
begin
  _Valid := (Component is TComponent) or Component.GetInterface(IFreeNotification, _FreeNotification);
  if not _Valid then
    raise Exception.CreateFmt('Class %s must inherit from TComponent or implement IFreeNotification to work with events', [Component.ClassName]);
  Self.fComponent := Component;
  Self.PropertyInfo := PropertyInfo;
  Self.Callable := Callable;
  Self.PyDelphiWrapper := PyDelphiWrapper;
  GetPythonEngine.Py_INCREF(Self.Callable);
  if Assigned(_FreeNotification) then
    _FreeNotification.Subscribe(PyDelphiWrapper)
  else
    (Component as TComponent).FreeNotification(PyDelphiWrapper);
end;

destructor TEventHandler.Destroy;
var
  Method : TMethod;
begin
  Method := GetMethodProp(Component, PropertyInfo);
  if Method.Data = Self then
  begin
    Method.Code := nil;
    Method.Data := nil;
    // Set the event property to nil, only if it we hooked it
    SetMethodProp(Component, PropertyInfo, Method);
    if PythonOK then
    begin
      if (csDestroying in PyDelphiWrapper.Engine.ComponentState) and IsLibrary then
        // Workarouond for exception during library finalization
        Dec(Self.Callable.ob_refcnt)
      else
        GetPythonEngine.Py_DECREF(Self.Callable);
    end;
  end;
  inherited;
end;

procedure TEventHandler.Unsubscribe;
var
  _FreeNotification : IFreeNotification;
begin
  if Component.GetInterface(IFreeNotification, _FreeNotification) then
    _FreeNotification.UnSubscribe(PyDelphiWrapper);
end;

{ TNotifyEventHandler }

constructor TNotifyEventHandler.Create(PyDelphiWrapper : TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TNotifyEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TNotifyEventHandler.DoEvent(Sender: TObject);
begin
  RaiseNotifyEvent(PyDelphiWrapper, Callable, Sender);
end;

class function TNotifyEventHandler.GetTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(TNotifyEvent);
end;

{ TEventHandlers }

function TEventHandlers.Add(AEventHandler: TEventHandler) : Boolean;
begin
  fItems.Add(AEventHandler);
  Result := True;
end;

procedure TEventHandlers.Clear;
begin
  fItems.Clear;
end;

constructor TEventHandlers.Create(APyDelphiWrapper : TPyDelphiWrapper);
begin
  inherited Create;
  fPyDelphiWrapper := APyDelphiWrapper;
  fItems := TObjectList.Create;
  fRegisteredClasses := TClassList.Create;
end;

procedure TEventHandlers.Delete(AIndex: Integer);
begin
  fItems.Delete(AIndex);
end;

destructor TEventHandlers.Destroy;
begin
  fRegisteredClasses.Free;
  fItems.Free;
  inherited;
end;

function TEventHandlers.GetCallable(AComponent: TObject;
  APropInfo: PPropInfo): PPyObject;
var
  _idx : Integer;
begin
  _idx := IndexOf(AComponent, APropInfo);
  if _idx > -1 then
  begin
    Result := Items[_idx].Callable;
    GetPythonEngine.Py_XIncRef(Result);
  end
  else
    Result := GetPythonEngine.ReturnNone;
end;

function TEventHandlers.FindHandler(ATypeInfo: PTypeInfo): TEventHandlerClass;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to RegisteredClassCount-1 do
    if RegisteredClasses[i].GetTypeInfo = ATypeInfo then
    begin
      Result := RegisteredClasses[i];
      Break;
    end;
end;

function TEventHandlers.GetCallable(AComponent: TObject;
  const APropName: string): PPyObject;
var
  _propInfo : PPropInfo;
begin
  _propInfo := GetPropInfo(AComponent, APropName, [tkMethod]);
  if Assigned(_propInfo) then
    Result := GetCallable(AComponent, _propInfo)
  else
    Result := GetPythonEngine.ReturnNone;
end;

function TEventHandlers.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TEventHandlers.GetItem(AIndex: Integer): TEventHandler;
begin
  Result := TEventHandler(fItems[AIndex]);
end;

function TEventHandlers.GetRegisteredClass(
  AIndex: Integer): TEventHandlerClass;
begin
  Result := TEventHandlerClass(fRegisteredClasses[AIndex]);
end;

function TEventHandlers.GetRegisteredClassCount: Integer;
begin
  Result := fRegisteredClasses.Count;
end;

function TEventHandlers.IndexOf(AComponent: TObject;
  APropInfo: PPropInfo): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if (Items[i].Component = AComponent) and (Items[i].PropertyInfo = APropInfo) then
    begin
      Result := i;
      Break;
    end;
end;

function TEventHandlers.Link(AComponent: TObject; APropInfo: PPropInfo;
  ACallable: PPyObject; out ErrMsg: string) : Boolean;
var
  _handlerClass : TEventHandlerClass;
begin
  Assert(Assigned(AComponent));
  Assert(Assigned(APropInfo));
  Assert(Assigned(ACallable));
  Result := False;
  with GetPythonEngine do
  begin
    if ACallable = Py_None then
    begin
      Unlink(AComponent, APropInfo); // it will assign nil to the event
      Result := True;
    end
    else if PyCallable_Check(ACallable) <> 0 then
    begin
      if SupportsFreeNotification(AComponent) then
      begin
        {$IFDEF FPC}
        _handlerClass := FindHandler(APropInfo^.PropType);
        {$ELSE FPC}
        _handlerClass := FindHandler(APropInfo^.PropType^);
        {$ENDIF FPC}
        if Assigned(_handlerClass) then
        begin
          Unlink(AComponent, APropInfo);
          Add(_handlerClass.Create(PyDelphiWrapper, AComponent, APropInfo, ACallable));
          Result := True;
        end
        else
          ErrMsg := Format(rs_ErrEventNotReg, [APropInfo^.PropType^.Name]);
      end
      else
        ErrMsg := Format(rs_ErrEventNoSuport, [AComponent.ClassName]);
    end
    else
      ErrMsg := Format(rs_ErrEventExpectCallable, [APropInfo^.Name])
  end;
end;

procedure TEventHandlers.RegisterHandler(AEventHandlerClass: TEventHandlerClass);
begin
  if fRegisteredClasses.IndexOf(AEventHandlerClass) < 0 then
    fRegisteredClasses.Add(AEventHandlerClass);
end;

function TEventHandlers.Unlink(AComponent: TObject;
  APropInfo: PPropInfo): Boolean;
var
  _idx : Integer;
begin
  _idx := IndexOf(AComponent, APropInfo);
  Result := _idx > -1;
  if Result then
    Delete(_idx);
end;

{ TRegisteredUnit }

procedure TRegisteredUnit.DefineFunctions(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

procedure TRegisteredUnit.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

procedure TRegisteredUnit.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
end;

{ TRegisteredUnits }

procedure TRegisteredUnits.Add(ARegisteredModule: TRegisteredUnit);
begin
  fItems.Add(ARegisteredModule);
end;

constructor TRegisteredUnits.Create;
begin
  inherited Create;
  fItems := TObjectList.Create;
end;

destructor TRegisteredUnits.Destroy;
begin
  fItems.Free;
  inherited;
end;

function TRegisteredUnits.GetCount: Integer;
begin
  Result := fItems.Count;
end;

function TRegisteredUnits.GetItem(AIndex: Integer): TRegisteredUnit;
begin
  Result := TRegisteredUnit(fItems[AIndex]);
end;

{ TPyDelphiWrapper }

constructor TPyDelphiWrapper.Create(AOwner: TComponent);
begin
  inherited;
  fClassRegister := TObjectList.Create(True);
  fHelperClassRegister := TStringList.Create;
  fEventHandlerList := TEventHandlers.Create(Self);

  if not (csDesigning in ComponentState) then
    CreateWrappers;

  // Enable sorting to benefit from binary search
  fHelperClassRegister.Sorted := True;
end;

function TPyDelphiWrapper.CreateComponent(pself, args: PPyObject): PPyObject;
//  Exposed function at the Module level
//  CreateComponent(ClassName, Owner)
var
  KlassName : PAnsiChar;
  _obj : PPyObject;
  OwnerComponent : TComponent;
  Klass : TClass;
  _comp : TObject;
  Component : TComponent;
  Ownership : TObjectOwnership;
begin
  Result := nil;
  CheckEngine;
  with Engine do begin
    if PyArg_ParseTuple( args, 'sO:CreateDelphiComponent',@KlassName, @_obj ) <> 0 then begin
      try
        Klass := GetClass(string(KlassName));
      except
        Klass := nil;
      end;
      if (Klass = nil) or not Klass.InheritsFrom(TComponent) then begin
        PyErr_SetObject(PyExc_TypeError^, PyUnicodeFromString(
          Format(rs_ErrInvalidArgs,
          ['CreateComponent', rs_InvalidClass])));
        Exit;
      end;

      if CheckObjAttribute(_obj, 'Owner', TComponent, _comp) then
        OwnerComponent := TComponent(_comp)
      else
        Exit;
      Component := TComponentClass(Klass).Create(OwnerComponent);
      if Assigned(OwnerComponent) then
        Ownership := soReference
      else
        Ownership := soOwned;
      Result := Self.Wrap(Component, Ownership);
    end else
      PyErr_SetObject(PyExc_TypeError^, PyUnicodeFromString(
        Format(rs_ErrInvalidArgs,
        ['CreateComponent', ''])));
  end;
end;

procedure TPyDelphiWrapper.CreateModuleFunctions;
var
  i : Integer;
begin
  if Assigned(FModule) then
  begin
    RegisterFunction(PAnsiChar('CreateComponent'), CreateComponent,
       PAnsiChar('CreateComponent(ComponentClass, Owner)'#10 +
       'Creates a component of type ComponentClass owned by Owner'));
    RegisterFunction(PAnsiChar('Abort'), Abort_Wrapper,
       PAnsiChar('Abort()'#10 +
       'Raises a silent exception.'));

    for i := 0 to RegisteredUnits.Count-1 do
      RegisteredUnits[i].DefineFunctions(Self);
  end;
end;

procedure TPyDelphiWrapper.CreateModuleVars;
var
  i : Integer;
begin
  Assert(Assigned(Module) and Module.Initialized);
  for i := 0 to RegisteredUnits.Count-1 do
    RegisteredUnits[i].DefineVars(Self);
end;

procedure TPyDelphiWrapper.CreatePyFunc(AModule: TPythonModule; AMethodDef: PPyMethodDef);
var
  d : PPyObject;
begin
  if Assigned(FModule) and FModule.Initialized then
    with GetPythonEngine do
    begin
      d := PyModule_GetDict(FModule.Module);
      Assert(Assigned(d));
      PyDict_SetItemString( d, AMethodDef^.ml_name, PyCFunction_NewEx(AMethodDef, nil, nil))
    end;
end;

procedure TPyDelphiWrapper.CreateWrappers;
var
  i : Integer;
begin
  // Helper Types
{$IFNDEF FPC}
  fDelphiMethodType := RegisterHelperType(TPyDelphiMethodObject);
{$ENDIF}
  fDefaultIterType      := RegisterHelperType(TPyDelphiIterator);
  fDefaultContainerType := RegisterHelperType(TPyDelphiContainer);
  fVarParamType         := RegisterHelperType(TPyDelphiVarParameter);
{$IFDEF EXTENDED_RTTI}
  fRecordType           := RegisterHelperType(TPyPascalRecord);
  fInterfaceType        := RegisterHelperType(TPyPascalInterface);
{$ENDIF}

  // Create and Register Wrapper for TObject
  RegisterDelphiWrapper(TPyDelphiObject);

  // Register Notify event Handler
  EventHandlers.RegisterHandler(TNotifyEventHandler);

  // Register wrappers for each Delphi unit
  for i := 0 to RegisteredUnits.Count-1 do
    RegisteredUnits[i].RegisterWrappers(Self);
end;

procedure TPyDelphiWrapper.DefineVar(const AName: string; const AValue: Variant);
var
  _obj : PPyObject;
begin
  CheckEngine;
  Assert(Assigned(Module));
  _obj := Engine.VariantAsPyObject(AValue);
  Module.SetVar(AnsiString(AName), _obj);
  Engine.Py_DECREF(_obj);
end;

procedure TPyDelphiWrapper.DefineVar(const AName: string; AValue: TObject);
var
  _obj : PPyObject;
begin
  Assert(Assigned(Module));
  _obj := Wrap(AValue);
  Module.SetVar(AnsiString(AName), _obj);
  Engine.Py_DECREF(_obj);
end;

destructor TPyDelphiWrapper.Destroy;
begin
  UnsubscribeFreeNotifications;
  // note that those pointers MUST be set to nil, otherwise it will crash
  // when calling inherited, as we have overridden SetEngine that tries to
  // assign the new engine value to the registered types.
  FreeAndNil(fClassRegister);
  FreeAndNil(fHelperClassRegister);
  FreeAndNil(fEventHandlerList);
  Module := nil;

  //No need to free PythonType objects since they are owned;
  inherited;
end;

procedure TPyDelphiWrapper.Finalize;
begin
  inherited;
  UnsubscribeFreeNotifications;
  if Assigned(fEventHandlerList) then
    fEventHandlerList.Clear;
end;

function TPyDelphiWrapper.GetHelperType(const TypeName: string): TPythonType;
var
  Index : integer;
begin
  Index := fHelperClassRegister.IndexOf(TypeName);
  if Index >= 0 then
    Result := fHelperClassRegister.Objects[Index] as TPythonType
  else
    Result := nil;
end;

procedure TPyDelphiWrapper.Initialize;
var
  i : integer;
begin
  if Initialized then
    Exit;
  inherited;
  // Initialize Wrapper Types
  for i := 0 to fClassRegister.Count - 1 do
    with TRegisteredClass(fClassRegister[i]).PythonType do
      if not Initialized then Initialize;
  // Initialize Helper Types
  for i := 0 to fHelperClassRegister.Count - 1 do
    with TPythonType(fHelperClassRegister.Objects[i]) do
      if not Initialized then Initialize;
  // Initialize module
  if Assigned(FModule) then begin
    if Module.Initialized then
    begin
      CreateModuleFunctions;
      CreateModuleVars;
    end
    else
      Module.AddClient( Self );
  end;
end;

procedure TPyDelphiWrapper.ModuleReady(Sender : TObject);
begin
  inherited;
  CreateModuleFunctions;
  CreateModuleVars;
end;

procedure TPyDelphiWrapper.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then begin
    if AComponent = FModule then
      FModule := nil
    else
      Notify(AComponent);
  end;
end;

procedure TPyDelphiWrapper.Notify(ADeletedObject: TObject);
Var
  i : integer;
begin
  //Free EventHandlers of Component
  if Assigned(fEventHandlerList) then
    for i := fEventHandlerList.Count - 1 downto 0 do
      if fEventHandlerList[i].Component = ADeletedObject then
        fEventHandlerList.Delete(i);
end;

procedure TPyDelphiWrapper.RegisterDelphiWrapper(
  AWrapperClass: TPyDelphiObjectClass);
var
  RegisteredClass : TRegisteredClass;
  Index: Integer;
begin
  Assert(Assigned(AWrapperClass));

  RegisteredClass := TRegisteredClass.Create;
  RegisteredClass.DelphiClass := AWrapperClass.DelphiObjectClass;
  RegisteredClass.PythonType := TPythonType.Create(Self);
  RegisteredClass.PythonType.Engine := Engine;
  RegisteredClass.PythonType.Module := fModule;
  RegisteredClass.PythonType.PyObjectClass := AWrapperClass;
  // Find nearest registered parent class and set it as base
  for Index := fClassRegister.Count - 1 downto 0 do
    with TRegisteredClass(fClassRegister[Index]) do
      if RegisteredClass.DelphiClass.InheritsFrom(DelphiClass) then
      begin
        RegisteredClass.PythonType.BaseType := PythonType;
        Break;
      end;

  fClassRegister.Add(RegisteredClass);
  if AWrapperClass.DelphiObjectClass.InheritsFrom(TPersistent) then
    Classes.RegisterClass(TPersistentClass(AWrapperClass.DelphiObjectClass));
end;

function TPyDelphiWrapper.RegisterFunction(AFuncName: PAnsiChar;
  AFunc: PyCFunction; ADocString: PAnsiChar): PPyMethodDef;
begin
  Assert(Assigned(Module));
  Result := FModule.AddMethod(AFuncName, AFunc, ADocString);
  CreatePyFunc(FModule, Result);
end;

function TPyDelphiWrapper.RegisterFunction(AFuncName : PAnsiChar; AFunc : TDelphiMethod; ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Assert(Assigned(Module));
  Result := FModule.AddDelphiMethod(AFuncName, AFunc, ADocString);
  CreatePyFunc(FModule, Result);
end;

function TPyDelphiWrapper.RegisterHelperType(APyObjectClass : TPyObjectClass) : TPythonType;
begin
  Result := TPythonType.Create(Self);
  Result.Engine := Engine;
  Result.Module := fModule;
  Result.PyObjectClass := APyObjectClass;
  fHelperClassRegister.AddObject(Result.Name, Result);
end;

procedure TPyDelphiWrapper.SetEngine(Value : TPythonEngine);
Var
  i : integer;
begin
  if Value <> Engine then begin
    inherited;
    // Delphi Types
    if Assigned(fClassRegister) then
      for i := 0 to fClassRegister.Count - 1 do
        TRegisteredClass(fClassRegister[i]).PythonType.Engine := Value;
    // Helper Types
    if Assigned(fHelperClassRegister) then
      for i := 0 to fHelperClassRegister.Count - 1 do
        TPythonType(fHelperClassRegister.Objects[i]).Engine := Value;
  end;
end;

procedure TPyDelphiWrapper.SetModule(const Value: TPythonModule);
Var
  i : integer;
begin
  if Value <> FModule then begin
    if Assigned(FModule) then
    begin
      FModule.RemoveFreeNotification(Self);
      FModule.RemoveClient(Self);
    end;
    FModule := Value;
    if Assigned(FModule) then
      FModule.FreeNotification(Self);
    if Assigned(fClassRegister) then
      for i := 0 to fClassRegister.Count - 1 do
        TRegisteredClass(fClassRegister[i]).PythonType.Module := Value;
    if Assigned(fHelperClassRegister) then
      for i := 0 to fHelperClassRegister.Count - 1 do
        TPythonType(fHelperClassRegister.Objects[i]).Module := Value;
    if Assigned(FModule) then
      if Initialized and (ComponentState * [csDesigning, csLoading] = []) then
        if FModule.Initialized then
        begin
          CreateModuleFunctions;
          CreateModuleVars;
        end
        else
          FModule.AddClient(Self);
  end;
end;

procedure TPyDelphiWrapper.UnsubscribeFreeNotifications;
Var
  i : integer;
begin
  if Assigned(fEventHandlerList) then
    for i := fEventHandlerList.Count - 1 downto 0 do
      fEventHandlerList[i].Unsubscribe;
end;

function TPyDelphiWrapper.Wrap(AObj: TObject;
  AOwnership: TObjectOwnership): PPyObject;
var
  i : integer;
  DelphiClass : TClass;
  Index : integer;
begin
  CheckEngine;
  // We cast the python object to the right delphi type
  if not Assigned(AObj) then
    Result := Engine.ReturnNone
  else begin
    // find nearest registered ancestor
    Index := -1;
    DelphiClass := AObj.ClassType;
    while Assigned(DelphiClass) do begin
      for i := 0 to fClassRegister.Count - 1 do
        if TRegisteredClass(fClassRegister[i]).DelphiClass = DelphiClass then begin
          Index := i;
          break;
        end;
      if Index >= 0 then break;
      DelphiClass := DelphiClass.ClassParent;
    end;
    Assert(Index >= 0, 'Internal Error in PyDelphiWrapper.Wrap'); // shouldn't happen

    Result := TRegisteredClass(fClassRegister[Index]).PythonType.CreateInstance;
    with PythonToDelphi(Result) as TPyDelphiObject do begin
      DelphiObject := AObj;
      PyDelphiWrapper := Self;
      Owned := AOwnership = soOwned;
    end;
  end;
end;

function TPyDelphiWrapper.WrapClass(AClass: TClass): PPyObject;
var
  I : integer;
  DelphiClass : TClass;
  Index : integer;
begin
  CheckEngine;
  if not Assigned(AClass) then
    Result := Engine.ReturnNone
  else begin
    // find nearest registered ancestor
    Index := -1;
    DelphiClass := AClass;
    while Assigned(DelphiClass) do begin
      for I := 0 to fClassRegister.Count - 1 do
        if TRegisteredClass(fClassRegister[I]).DelphiClass = DelphiClass then begin
          Index := I;
          break;
        end;
      if Index >= 0 then break;
      DelphiClass := DelphiClass.ClassParent;
    end;
    Assert(Index >= 0, 'Internal Error in PyDelphiWrapper.WrapClass'); // shouldn't happen

    Result := PPyObject(TRegisteredClass(fClassRegister[Index]).PythonType.TheTypePtr);
    Engine.Py_XINCREF(Result);
  end;
end;

{$IFDEF EXTENDED_RTTI}
function TPyDelphiWrapper.WrapRecord(Address: Pointer; Typ: TRttiStructuredType) : PPyObject;
var
  PythonType: TPythonType;
begin
  CheckEngine;
  PythonType := GetHelperType('PascalRecordType');
  if not Assigned(PythonType) or not Assigned(Address) then
  begin
    Result := Engine.ReturnNone;
    Exit;
  end;
  Result := PythonType.CreateInstance;
  with PythonToDelphi(Result) as TPyPascalRecord do begin
    SetAddrAndType(Address, Typ);
    PyDelphiWrapper := Self;
  end;
end;

function TPyDelphiWrapper.WrapInterface(const IValue: TValue): PPyObject;
var
  PythonType: TPythonType;
  Address: Pointer;
  Typ: TRttiStructuredType;
begin
  CheckEngine;
  if IValue.IsEmpty then begin
    Result := Engine.ReturnNone;
    Exit;
  end;
  PythonType := GetHelperType('PascalInterfaceType');
  if not Assigned(PythonType) or (IValue.Kind <> tkInterface) then
  begin
    Result := Engine.ReturnNone;
    Exit;
  end;
  Result := PythonType.CreateInstance;
  Typ := TRttiContext.Create.GetType(IValue.TypeInfo) as TRttiStructuredType;
  Address := Pointer(IValue.GetReferenceToRawData^);
  with PythonToDelphi(Result) as TPyPascalInterface do begin
    SetAddrAndType(Address, Typ);
    PyDelphiWrapper := Self;
  end;
end;

// To keep the RTTI Pool alive and avoid continuously creating/destroying it
// See also https://stackoverflow.com/questions/27368556/trtticontext-multi-thread-issue
Var
_RttiContext: TRttiContext;

procedure _InitRttiPool;
begin
 _RttiContext := TRttiContext.Create;
 _RttiContext.FindType('');
end;

initialization
  _InitRttiPool;
finalization
  _RttiContext.Free();
{$ELSE}

initialization
finalization
{$ENDIF}
  FreeAndNil(gRegisteredUnits);
end.

