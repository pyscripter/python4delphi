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

(*-----------------------------------------------------------------------------
 Purpose:   Provide automatic wrapping of Delphi variables utilising RTTI

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
    fContainerAccess: TContainerAccess;
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
    {$IFDEF EXTENDED_RTTI}
    class function ExcludedExposedMembers(APythonType: TPythonType): TArray<string>; virtual;
    class procedure ExposeMethods(AClass: TClass; NearestAncestorClass: TClass;
      APythonType: TPythonType; APyDelphiWrapper: TPyDelphiWrapper;
      AExcludedMethodNames: TArray<string> = nil);
    class procedure ExposeFields(AClass: TClass; NearestAncestorClass: TClass;
      APythonType: TPythonType; APyDelphiWrapper: TPyDelphiWrapper;
      AExcludedFieldNames: TArray<string> = nil);
    class procedure ExposeProperties(AClass: TClass; NearestAncestorClass: TClass;
      APythonType: TPythonType; APyDelphiWrapper: TPyDelphiWrapper;
      AExcludedPropertyNames: TArray<string> = nil);
    class procedure ExposeIndexedProperties(AClass: TClass; NearestAncestorClass: TClass;
      APythonType: TPythonType; APyDelphiWrapper: TPyDelphiWrapper;
      AExcludedPropertyNames: TArray<string> = nil);
    {$ENDIF EXTENDED_RTTI}
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    Owned: Boolean;

    constructor Create( APythonType : TPythonType ); override;
    // CreateWith raises a python TypeError 'Cannot create instances..'
    // Subclasses that can be instantiated need to overwrite this method and
    // a) Call the virtual constructor Create
    // b) Create the pascal object and assign it to DelphiObject
    constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); override;
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
    // Mapping services
    {$IFDEF EXTENDED_RTTI}
    function MpSubscript(obj: PPyObject) : PPyObject; override;
    function MpAssSubscript(obj1, obj2: PPyObject) : Integer; override;
    {$ENDIF EXTENDED_RTTI}

    class function  DelphiObjectClass : TClass; virtual;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    class procedure RegisterGetSets( PythonType : TPythonType ); override;
    class procedure SetupType(APythonType: TPythonType ); override;
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

  {
    Generic wrapper for pascal classes

    Can be used from unit wrappers as follows:
      PyDelphiWrapper1.RegisterDelphiWrapper(TPyClassWrapper<TMyClass>);
    or at runtime (e.g. inside the FormCreate handler:
      PyDelphiWrapper1.RegisterDelphiWrapper(TPyClassWrapper<TMyClass>).Initialize;

    if you want your class to capable of being instantiated from python then do:

    TMyClassWrapper = class(TPyClassWrapper<TMyClass>)
      constructor CreateWith(APythonType: TPythonType; args, kwds: PPyObject); overload; override;
    end;

    constuctor TMyClassWrapper.CreateWith(APythonType: TPythonType; args, kwds: PPyObject);
    begin
      Create(APythonType);
      DelphiObject := TMyClass.Create;
    end;

    PyDelphiWrapper1.RegisterDelphiWrapper(TMyClassWrapper).Initialize;
  }
  TPyClassWrapper<T: class> = class(TPyDelphiObject)
  private
    function GetDelphiObject: T;
    procedure SetDelphiObject(const Value: T);
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: T read GetDelphiObject write SetDelphiObject;
  end;

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
    fCopy: TValue;
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
    procedure SetupFromTValue(const AValue: TValue);

    function  GetAttrO( key: PPyObject) : PPyObject; override;
    function  SetAttrO( key, value: PPyObject) : Integer; override;
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
    function Repr : PPyObject; override;
    class procedure SetupType( PythonType : TPythonType ); override;
  end;

  TPyPascalInterface = class(TPyRttiObject)
  private
    function GetValue: TValue; override;
  public
    function Repr : PPyObject; override;
    class procedure SetupType( PythonType : TPythonType ); override;
  end;
  {$ENDIF}

  TBaseEventHandler = class
  private
    fComponent: TObject;
  public
    PyDelphiWrapper : TPyDelphiWrapper;
    PropertyInfo : PPropInfo;
    Callable : PPyObject;
    // connects to the event on creation
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); virtual;
    // Disconnects from the event on destruction
    destructor Destroy; override;
    // Disconnects from the free notification event now
    procedure Unsubscribe;
    // properties
    property Component : TObject read fComponent;
  end;

  TEventHandler = class(TBaseEventHandler)
  public
    // returns the type info of the supported event
    class function GetTypeInfo : PTypeInfo; virtual; abstract;
  end;
  TEventHandlerClass = class of TEventHandler;

  TEventHandlers = class
  private
    fItems : TObjectList;
    fRegisteredClasses : TClassList;
    fPyDelphiWrapper: TPyDelphiWrapper;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TBaseEventHandler;
    function GetRegisteredClass(AIndex: Integer): TEventHandlerClass;
    function GetRegisteredClassCount: Integer;
  protected
    function FindHandler(ATypeInfo : PTypeInfo) : TEventHandlerClass;
    property RegisteredClasses[AIndex : Integer] : TEventHandlerClass read GetRegisteredClass;
    property RegisteredClassCount : Integer read GetRegisteredClassCount;
  public
    constructor Create(APyDelphiWrapper : TPyDelphiWrapper);
    destructor Destroy; override;

    function  Add(AEventHandler : TBaseEventHandler) : Boolean;
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
    property Items[AIndex : Integer] : TBaseEventHandler read GetItem; default;
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
    // Stores created exposed class members
    fExposedMembers: TObjectList;
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
    procedure DefineVar(const AName : string; AValue : TObject; AOwnership: TObjectOwnership = soReference); overload;
    procedure DefineVar(const AName : string; AValue : TClass); overload;
    function RegisterDelphiWrapper(AWrapperClass : TPyDelphiObjectClass): TPythonType;
    function  RegisterHelperType(APyObjectClass : TPyObjectClass) : TPythonType;
    function  RegisterFunction(AFuncName : PAnsiChar; AFunc : PyCFunction; ADocString : PAnsiChar ): PPyMethodDef; overload;
    function  RegisterFunction(AFuncName : PAnsiChar; AFunc : TDelphiMethod; ADocString : PAnsiChar ): PPyMethodDef; overload;
    function  GetHelperType(const TypeName : string) : TPythonType;
    //  Function that provides a Python object wrapping an object
    function Wrap(AObj : TObject; AOwnership: TObjectOwnership = soReference) : PPyObject;
    function WrapClass(AClass: TClass): PPyObject;
    {$IFDEF EXTENDED_RTTI}
    //  Functions that provides a Python object wrapping a record
    //  The first overload wraps the record itself and the record needs to be kept alive.
    //  The second overload wraps a copy of the record contained in a TValue
    function WrapRecord(Address: Pointer; Typ: TRttiStructuredType): PPyObject; overload;
    function WrapRecord(const AValue: TValue): PPyObject; overload;
    //  Function that provides a Python object wrapping an interface
    //  Note the the interface must be compiled in {$M+} mode and have a guid
    //  The interface will be kept alive as long as python has areference to it.
    //  Usage: WrapInterface(TValue.From(YourInterfaceReference))
    function WrapInterface(const IValue: TValue): PPyObject;
    procedure DefineVar(const AName: string; AValue: TValue); overload;
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

{$IFDEF EXTENDED_RTTI}
  // Documentation hook interface
  // Implement to customize the creation of docstrings for exposed class members
  IDocServer = interface
    ['{4AF0D319-47E9-4F0A-9C71-97B8CBB559FF}']
    function ReadTypeDocStr(ATypeInfo: PTypeInfo; out ADocStr: string): Boolean;
    function ReadMemberDocStr(AMember: TRttiMember; out ADocStr: string): Boolean;
    procedure Initialize;
    procedure Finalize;
    function Initialized: Boolean;
  end;

  var
    PyDocServer: IDocServer = nil;
{$ENDIF}

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
  function  CheckEnum(const AEnumName : string; AValue, AMinValue, AMaxValue: Integer) : Boolean;
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; const AValue: Variant) : PPyObject; overload;
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper; AObject: TObject) : PPyObject; overload;
  function  CreateVarParam(PyDelphiWrapper: TPyDelphiWrapper; AClass: TClass): PPyObject; overload;
  function  SetToPython(ATypeInfo: PTypeInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(APropInfo: PPropInfo; AValue : Integer) : PPyObject; overload;
  function  SetToPython(AInstance: TObject; APropInfo: PPropInfo) : PPyObject; overload;
  function  PythonToSet(APropInfo: PPropInfo; ASet : PPyObject) : Integer; overload;
  function  PythonToSet(ATypeInfo: PTypeInfo; ASet : PPyObject) : Integer; overload;
  function  SupportsFreeNotification(AObject : TObject) : Boolean;
  procedure RaiseNotifyEvent(PyDelphiWrapper : TPyDelphiWrapper; ACallable : PPyObject; Sender: TObject);
  {Sets mulptiple properties of PyObject from keywords argument}
  function SetProperties(PyObject: PPyObject; keywords: PPyObject): PPyObject;
  function ValidateClassRef(PyValue: PPyObject; RefClass: TClass;
    out ClassRef: TClass; out ErrMsg: string): Boolean;
  procedure InvalidArguments(const MethName, ErrMsg : string);
{$IFDEF EXTENDED_RTTI}
  function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper;
    const AValue: TValue) : PPyObject; overload;
  function SimpleValueToPython(const Value: TValue;
    out ErrMsg: string): PPyObject;
  function TValueToPyObject(const Value: TValue;
    DelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject;
  function PyObjectToTValue(PyArg : PPyObject; ArgType: TRttiType;
   out Arg: TValue; out ErrMsg: string): Boolean;
{$ENDIF}


implementation

uses
  Math,
  StrUtils,
  RTLConsts,
  MethodCallback;

resourcestring
  rs_CannotCreate = 'Cannot create instances of class %s';
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
  rs_ErrInvalidRet = 'Call "%s" returned a value that could not be converted to Python'#$A'Error: %s';
  rs_IncompatibleArguments = 'Expected and actual arguments are incompatible';
  rs_ErrAttrGet = 'Error in getting property "%s".'#$A'Error: %s';
  rs_UnknownAttribute = 'Unknown attribute';
  rs_ErrIterSupport = 'Wrapper %s does not support iterators';
  rs_ErrAttrSet = 'Error in setting property %s'#$A'Error: %s';
  rs_ErrObjectDestroyed = 'Trying to access a destroyed pascal object';
  rs_IncompatibleClasses = 'Incompatible classes';
  rs_IncompatibleRecords = 'Incompatible record types';
  rs_IncompatibleInterfaces = 'Incompatible interfaces';
  rs_IncompatiblePythonType = 'Incompatible python value type';
  rs_NotPublished = 'Event handling is available only for published properties';
  rs_ExpectedObject = 'Expected a Pascal object';
  rs_ExpectedRecord = 'Expected a Pascal record';
  rs_ExpectedClass = 'Expected a Pascal class';
  rs_ExpectedNil = 'In static methods Self should be nil';
  rs_ExpectedInterface = 'Expected a Pascal interface';
  rs_ExpectedSequence = 'Expected a python sequence';
  rsExpectedPointer = 'Expected a Pointer';
  rs_InvalidClass = 'Invalid class';
  rs_ErrEventNotReg = 'No Registered EventHandler for events of type "%s';
  rs_ErrEventNoSuport = 'Class %s does not support events because it must '+
    'either inherit from TComponent or implement interface IFreeNotification';
  rs_ErrEventExpectCallable = 'You can only assign a callable to method property "%s"';
  rs_NotWritable = 'The class member is not writable';
  rs_NotReadable = 'The class member is not readable';
  rs_NoAccess = 'Private and protected class members cannot be accessed';
  rs_ErrValueToPython = 'Unsupported conversion from TValue to Python value';
  rs_ErrPythonToValue = 'Unsupported conversion from Python value to TValue';
  rs_ErrNoTypeInfo = 'TypeInfo is not available';
  rs_ErrUnexpected = 'Unexpected error';

{$REGION 'TRttiInvokableTypeHelper - "Lifted" from Spring4D"'}

{$IF not Defined(FPC) and Defined(EXTENDED_RTTI) and (CompilerVersion < 36)}
{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2018 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}
type
  TRttiInvokableTypeHelper = class helper for TRttiInvokableType
  public
    function CreateImplementation(AUserData: Pointer;
      const ACallback: TMethodImplementationCallback): TMethodImplementation;
  end;

  // this is the class used to create a TMethodImplementation for a
  // TRttiInvokableType by passing in an instance of TRttiInvokableType
  // and "overriding" its private virtual methods
  TRttiInvokableMethod = class(TRttiMethod)
  private
    FType: TRttiInvokableType;
    constructor Create(AType: TRttiInvokableType);
  end;

  // this classes is needed to access FParent
  // it needs to have the exact same fields as System.Rtti.TRttiObject
  TRttiObjectHack = class abstract
  protected
    FHandle: Pointer;
    FRttiDataSize: Integer;
    FPackage: TRttiPackage;
    FParent: TRttiObject;
  end;

  // this class is needed to "override" private virtual methods
  // it needs to have the exact same virtual methods as System.Rtti.TRttiMethod
  TRttiInvokableMethodHack = class(TRttiMember)
  protected
    FInvokeInfo: TObject; //TMethodImplementation.TInvokeInfo
    FType: TRttiInvokableType;
    function GetMethodKind: TMethodKind; virtual; abstract;
    function GetCallingConvention: TCallConv; virtual;
    function GetReturnType: TRttiType; virtual;
    function GetDispatchKind: TDispatchKind; virtual; abstract;
    function GetHasExtendedInfo: Boolean; virtual; abstract;
    function GetVirtualIndex: SmallInt; virtual; abstract;
    function GetCodeAddress: Pointer; virtual; abstract;
    function GetIsClassMethod: Boolean; virtual;
    function GetIsStatic: Boolean; virtual;
    function DispatchInvoke(Instance: TValue; const Args: array of TValue): TValue; virtual; abstract;
  public
    function GetParameters: TArray<TRttiParameter>; virtual;
  end;

  // this class is needed to "override" the destructor of
  // the TMethodImplementation instances that are created inside of
  // TRttiMethod.CreateImplementation
  TMethodImplementationHack = class(TMethodImplementation)
  public
    destructor Destroy; override;
  end;

function TRttiInvokableMethodHack.GetCallingConvention: TCallConv;
begin
  Result := FType.CallingConvention;
end;

function TRttiInvokableMethodHack.GetIsClassMethod: Boolean;
begin
  Result := False;
end;

function TRttiInvokableMethodHack.GetIsStatic: Boolean;
begin
  Result := FType is TRttiProcedureType;
end;

function TRttiInvokableMethodHack.GetParameters: TArray<TRttiParameter>;
begin
  Result := FType.GetParameters;
end;

function TRttiInvokableMethodHack.GetReturnType: TRttiType;
begin
  Result := FType.ReturnType;
end;

destructor TMethodImplementationHack.Destroy;
begin
  if FInvokeInfo <> nil then
    FInvokeInfo.Free;
  inherited Destroy;
end;

constructor TRttiInvokableMethod.Create(AType: TRttiInvokableType);
var
  ctx: TRttiContext;
begin
  // GetInvokeInfo need the Parent property
  TRttiObjectHack(Self).FParent := ctx.GetType(TObject);
  FType := AType;
  // change the type of this class to the class that has its private
  // methods "overridden"
  PPointer(Self)^ := TRttiInvokableMethodHack;
end;

function TRttiInvokableTypeHelper.CreateImplementation(AUserData: Pointer; //FI:O804
  const ACallback: TMethodImplementationCallback): TMethodImplementation;
var
  m: TRttiMethod;
begin
  {$WARN CONSTRUCTING_ABSTRACT OFF}
  m := TRttiInvokableMethod.Create(Self);
  try
    // there is no way to directly create a TMethodImplementation instance
    // because it requires an instance of the private TInvokeInfo class to be
    // passed which can only be produced by the private method GetInvokeInfo

    // since TRttiInvokableMethod has the necessary private virtual methods
    // "overridden" it will create the correct TMethodImplementation instance
    // for the given TRttiInvokableType
    Result := m.CreateImplementation(AUserData, ACallback);
    // "override" the destructor so FInvokeMethod which is not owned by the
    // TRttiInvokableMethod is properly destroyed at the end
    PPointer(Result)^ := TMethodImplementationHack;
  finally
    m.Free;
  end;
end;
{$IFEND not Defined(FPC) and Defined(EXTENDED_RTTI) and (CompilerVersion < 36)}

{$ENDREGION 'TRttiInvokableTypeHelper - "Lifted from Spring4D"'}


var
  gRegisteredUnits : TRegisteredUnits;

{$IFDEF EXTENDED_RTTI}
function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  MethName: string; ParentRtti: TRttiStructuredType; ob1, ob2: PPyObject;
  AParentAddrIsClass: Boolean = false): PPyObject; overload; forward;

function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  Method: TRttiMethod; ob1, ob2: PPyObject;
  AParentAddrIsClass: Boolean = false): PPyObject; overload; forward;

  function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  Method: TRttiMethod; const Args: TArray<TValue>;
  const  VarParamIndices: TArray<Integer>;
  AParentAddrIsClass: Boolean = false): PPyObject; overload; forward;

function GetRttiProperty(ParentAddr: Pointer; Prop: TRttiProperty;
 PyDelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject; forward;

function GetRttiField(ParentAddr: Pointer; Field: TRttiField;
 PyDelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject; forward;

function SetRttiProperty(const ParentAddr: Pointer;  Prop: TRttiProperty;
  Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean; forward;

function SetRttiField(const ParentAddr: Pointer;  Field: TRttiField;
  Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean; forward;

function ValidateClassProperty(PyValue: PPyObject; TypeInfo: PTypeInfo;
  out Obj: TObject; out ErrMsg: string): Boolean; forward;

type
  TAbstractExposedMember = class abstract
  protected
    FName: AnsiString;
    FDocString: AnsiString;
    FPyDelphiWrapper: TPyDelphiWrapper;
    FPythonType: TPythonType;
    FRttiMember: TRttiMember;
    FParentRtti: TRttiStructuredType;
    function GetDefaultDocString(): string; virtual; abstract;
  public
    constructor Create(ARttiMember: TRttiMember;
      APyDelphiWrapper: TPyDelphiWrapper; APythonType: TPythonType;
      AParentRtti: TRttiStructuredType);
    property DocString: AnsiString read FDocString write FDocString;
    property Name: AnsiString read FName;
  end;

  TExposedMethod = class(TAbstractExposedMember)
  private
    FCallback: Pointer;
    function GetRttiMethod: TRttiMethod;
    function GetCallback: Pointer;
  protected
    function GetDefaultDocString(): string; override;
  public
    destructor Destroy; override;
    function MethodWrapper(ASelf, Args, Kwds: PPyObject): PPyObject; cdecl;
    property RttiMethod: TRttiMethod read GetRttiMethod;
    property Callback: Pointer read GetCallback;
    class function MethodDocStr(ARttiMethod: TRttiMethod): string;
  end;

  TExposedGetSet = class(TAbstractExposedMember)
  private
    FGetterCallback: Pointer;
    FSetterCallback: Pointer;
    function GetGetterCallback: Pointer;
    function GetSetterCallback: Pointer;
  public
    destructor Destroy; override;
    function GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject; virtual; cdecl;
    function SetterWrapper(AObj, AValue: PPyObject; AContext: Pointer): Integer; virtual; cdecl;
    property GetterCallback: Pointer read GetGetterCallback;
    property SetterCallback: Pointer read GetSetterCallback;
  end;

  TExposedField = class(TExposedGetSet)
  protected
    function GetDefaultDocString(): string; override;
  end;

  TExposedProperty = class(TExposedGetSet)
  protected
    function GetDefaultDocString(): string; override;
  end;

  TExposedEvent = class(TExposedGetSet)
  protected
    function GetDefaultDocString(): string; override;
  public
    function GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject; override; cdecl;
    function SetterWrapper(AObj, AValue: PPyObject; AContext: Pointer): Integer; override; cdecl;
  end;

  TPyIndexedProperty = class(TPyObject)
  private
    FPyObj: PPyObject;
    FPyWrapper: TPyDelphiWrapper;
    FProperty: TRttiIndexedProperty;
  public
    destructor Destroy; override;
    procedure Setup(PyObj: PPyObject; Wrapper: TPyDelphiWrapper; Prop: TRttiIndexedProperty);
    class procedure SetupType(PythonType: TPythonType); override;
    // Mapping services
    function MpSubscript(obj: PPyObject) : PPyObject; override;
    function MpAssSubscript(obj1, obj2: PPyObject) : Integer; override;
  end;

  TExposedIndexedProperty = class(TExposedGetSet)
  protected
    function GetDefaultDocString(): string; override;
  public
    function GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject; override; cdecl;
  end;

  TRttiEventHandler = class(TBaseEventHandler)
  private
    FMethodImplementation: TMethodImplementation;
  public
    MethodType: TRttiMethodType;
    function CodeAddress: Pointer;
    constructor Create(PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
      PropertyInfo: PPropInfo; Callable: PPyObject;
      AMethodType: TRttiMethodType); reintroduce;
    destructor Destroy; override;
    class procedure ImplCallback(UserData: Pointer; const Args: TArray<TValue>;
    out Result: TValue); static;
  end;

{ TAbstractExposedMember }

constructor TAbstractExposedMember.Create(ARttiMember: TRttiMember;
  APyDelphiWrapper: TPyDelphiWrapper; APythonType: TPythonType;
  AParentRtti: TRttiStructuredType);
begin
  inherited Create;
  FRttiMember := ARttiMember;
  FName := AnsiString(ARttiMember.Name);
  FPyDelphiWrapper := APyDelphiWrapper;
  FPythonType := APythonType;
  FParentRtti := AParentRtti;
  FDocString := UTF8Encode(GetDefaultDocString);
end;

{ TExposedMethod }

function TExposedMethod.GetCallback: Pointer;
var
  Method: TDelphiMethodWithKW;
begin
  if FCallback = nil then
  begin
    Method := MethodWrapper;
    FCallBack := GetOfObjectCallBack(TCallBack(Method), 3, DEFAULT_CALLBACK_TYPE);
  end;
  Result := FCallback;
end;

function TExposedMethod.GetRttiMethod: TRttiMethod;
begin
  Result := FRttiMember as TRttiMethod;
end;

destructor TExposedMethod.Destroy;
begin
  if FCallback <> nil then
    DeleteCallback(FCallback);
  inherited;
end;

function TExposedMethod.GetDefaultDocString: string;
begin
  Result := Format('<Method %s of type %s at %x>', [
      RttiMethod.Name, FParentRtti.Name, NativeInt(RttiMethod.CodeAddress)]);
end;

function TExposedMethod.MethodWrapper(ASelf, Args, Kwds: PPyObject): PPyObject;
var
  ParentAddress: Pointer;
  RefClass, ClassRef: TClass;
  ErrMsg: string;
begin
  if RttiMethod.IsStatic then
  begin
    if ASelf <> nil then
    begin
      InvalidArguments(string(FName), rs_ExpectedNil);
      Exit(nil);
    end
    else
      ParentAddress := nil;
  end
  else if RttiMethod.IsClassMethod then
  begin
    // Class methods can be called from both instances and class references
    if IsDelphiObject(ASelf) then
      ParentAddress := TPyDelphiObject(PythonToDelphi(ASelf)).DelphiObject.ClassType
    else
    begin
      RefClass := (RttiMethod.Parent as TRttiInstanceType).MetaclassType;
      if ValidateClassRef(ASelf, RefClass, ClassRef, ErrMsg) then
        ParentAddress := ClassRef
      else
      begin
        InvalidArguments(string(FName), rs_ExpectedClass);
        Exit(nil);
      end;
    end;
  end
  else
  begin
    if IsDelphiObject(ASelf) then
      ParentAddress := TPyDelphiObject(PythonToDelphi(ASelf)).DelphiObject
    else
    begin
      InvalidArguments(string(FName), rs_ExpectedObject);
      Exit(nil);
    end;
  end;

  // TODO: Optimize out the method search in RttiCall, by passing the
  // RttiMethod directly to an overload of Rtti, when the method
  // is not overloaded

  Result := RttiCall(
    ParentAddress,
    FPyDelphiWrapper,
    string(FName),
    FParentRtti,
    Args,
    Kwds,
    RttiMethod.IsStatic or RttiMethod.IsClassMethod);
end;

class function TExposedMethod.MethodDocStr(ARttiMethod: TRttiMethod): string;
const
  METHOD_DOC_STR_PATTERN = '%s.%s(%s)';
var
  LArgsStr: string;
  LRttiParameter: TRttiParameter;
begin
  LArgsStr := '';
  for LRttiParameter in ARttiMethod.GetParameters do begin
    if LArgsStr <> '' then
      LArgsStr := LArgsStr + ', ';

    LArgsStr := LArgsStr + LRttiParameter.Name;
    if Assigned(LRttiParameter.ParamType) then
      LArgsStr := LArgsStr + ': ' +
        LRttiParameter.ParamType.Name.Replace('T', '', []);
  end;

  Result := Format(METHOD_DOC_STR_PATTERN,
    [ARttiMethod.Parent.Name, ARttiMethod.Name, LArgsStr]);

  if Assigned(ARttiMethod.ReturnType) then
    Result := Result
      + ' -> '
      + ARttiMethod.ReturnType.Name.Replace('T', '', []);
end;

{ TExposedGetSet }

destructor TExposedGetSet.Destroy;
begin
  if FGetterCallback <> nil then
    DeleteCallback(FGetterCallback);
  if FSetterCallback <> nil then
    DeleteCallback(FSetterCallback);
  inherited;
end;

function TExposedGetSet.GetGetterCallback: Pointer;
var
  Method: function (AObj: PPyObject; AContext : Pointer): PPyObject of object; cdecl;
begin
  if FGetterCallback = nil then
  begin
    Method := GetterWrapper;
    FGetterCallback := GetOfObjectCallBack(TCallBack(Method), 2, DEFAULT_CALLBACK_TYPE);
  end;
  Result := FGetterCallback;
end;

function TExposedGetSet.GetSetterCallback: Pointer;
var
  Method: function(AObj, AValue: PPyObject; AContext: Pointer): Integer of object; cdecl;
begin
  if FSetterCallback = nil then
  begin
    Method := SetterWrapper;
    FSetterCallback := GetOfObjectCallBack(TCallBack(Method), 3, DEFAULT_CALLBACK_TYPE);
  end;
  Result := FSetterCallback;
end;

function TExposedGetSet.GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject; cdecl;
var
  Obj: TObject;
  LOutMsg: string;
begin
  Result := nil;
  if ValidateClassProperty(AObj, FParentRtti.Handle, Obj, LOutMsg) then
  try
    if FRttiMember is TRttiProperty then
      Result := GetRttiProperty(Obj, TRttiProperty(FRttiMember), FPyDelphiWrapper, LOutMsg)
    else if FRttiMember is TRttiField then
      Result := GetRttiField(Obj, TRttiField(FRttiMember), FPyDelphiWrapper, LOutMsg);
  except
    on E: Exception do
      LOutMsg := E.Message;
  end;

  if not Assigned(Result) then
    with GetPythonEngine do
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrAttrGet, [FRttiMember.Name, LOutMsg]))));
end;

function TExposedGetSet.SetterWrapper(AObj, AValue: PPyObject; AContext: Pointer): Integer; cdecl;
var
  Obj: TObject;
  ErrMsg: string;
begin
  Result := -1;
  if ValidateClassProperty(AObj, FParentRtti.Handle, Obj, ErrMsg) then
  try
    if ((FRttiMember is TRttiProperty) and SetRttiProperty(Obj,
      TRttiProperty(FRttiMember), AValue, FPyDelphiWrapper, ErrMsg)) or
      ((FRttiMember is TRttiField) and SetRttiField(Obj,
      TRttiField(FRttiMember), AValue, FPyDelphiWrapper, ErrMsg))
    then
      Result := 0
  except
    on E: Exception do
      ErrMsg := E.Message;
  end;

  if Result <> 0 then
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrAttrSet, [FRttiMember.Name, ErrMsg]))));
end;

{ TExposedField }

function TExposedField.GetDefaultDocString: string;
var
  FieldType: string;
begin
  if Assigned((FRttiMember as TRttiField).FieldType) then
    FieldType := TRttiField(FRttiMember).FieldType.Name;

  Result := Format('<Field %s.%s of type %s>', [
    FParentRtti.Name, FRttiMember.Name, FieldType]);
end;

{ TExposedProperty }

function TExposedProperty.GetDefaultDocString: string;
var
  PropertyType: string;
begin
  if Assigned((FRttiMember as TRttiProperty).PropertyType) then
    PropertyType := TRttiProperty(FRttiMember).PropertyType.Name;

  Result := Format('<Property %s.%s of type %s>', [
    FParentRtti.Name, FRttiMember.Name, PropertyType]);
end;

{ TExposedEvent }

function TExposedEvent.GetDefaultDocString(): string;
begin
  Result := Format('<Event property %s.%s>', [FParentRtti.Name, FRttiMember.Name])
    + #10 +(FRttiMember as TRttiProperty).PropertyType.ToString;
end;

function TExposedEvent.GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject;
var
  Obj: TObject;
  RttiProp: TRttiInstanceProperty;
  LOutMsg: string;
begin
  RttiProp := FRttiMember as TRttiInstanceProperty;
  if ValidateClassProperty(AObj, FParentRtti.Handle, Obj, LOutMsg) then
    Result := FPyDelphiWrapper.EventHandlers.GetCallable(Obj, RttiProp.PropInfo)
  else
    Result := FPyDelphiWrapper.Engine.ReturnNone;
end;

function TExposedEvent.SetterWrapper(AObj, AValue: PPyObject; AContext: Pointer): Integer;
var
  Obj: TObject;
  RttiProp: TRttiInstanceProperty;
  ErrMsg: string;
  EventHandler: TRttiEventHandler;
  Method: TMethod;
begin
  Result := -1;
  if not CheckCallableAttribute(AValue, FRttiMember.Name) then
    Exit;

  if ValidateClassProperty(AObj, FParentRtti.Handle, Obj, ErrMsg) then
  begin
    try
      RttiProp := FRttiMember as TRttiInstanceProperty;

      // Remove handler if it exists
      fPyDelphiWrapper.EventHandlers.Unlink(Obj, RttiProp.PropInfo);

      if AValue = GetPythonEngine.Py_None then
      begin
        Method.Code := nil;
        Method.Data := nil;
      end
      else
      begin
        EventHandler := TRttiEventHandler.Create(FPyDelphiWrapper, Obj,
          RttiProp.PropInfo, AValue, RttiProp.PropertyType as TRttiMethodType);

        FPyDelphiWrapper.EventHandlers.Add(EventHandler);

        Method.Code := EventHandler.CodeAddress;
        Method.Data := EventHandler;
      end;
      SetMethodProp(Obj, RttiProp.PropInfo, Method);

      Result := 0;
    except
      ErrMsg := rs_ErrUnexpected;
    end;
  end;

  if Result <> 0 then
    with GetPythonEngine do
      PyErr_SetString (PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrAttrSet, [FRttiMember.Name, ErrMsg]))));
end;

{ TExposedIndexedProperty }
function TExposedIndexedProperty.GetDefaultDocString(): string;
var
  PropertyType: string;
begin
  if Assigned((FRttiMember as TRttiIndexedProperty).PropertyType) then
    PropertyType := TRttiIndexedProperty(FRttiMember).PropertyType.Name;

  Result := Format('<Indexed property %s.%s of type %s>', [
    FParentRtti.Name, FRttiMember.Name, PropertyType]);
end;

function TExposedIndexedProperty.GetterWrapper(AObj: PPyObject; AContext : Pointer): PPyObject;
var
  HelperType: TPythonType;
begin
   HelperType := FPyDelphiWrapper.GetHelperType('IndexedPropertyType');
   Assert(HelperType <> nil);

   Result := HelperType.CreateInstance;
   (PythonToDelphi(Result) as TPyIndexedProperty).Setup(AObj, FPyDelphiWrapper,
     FRttiMember as TRttiIndexedProperty);
end;

{ TPyIndexedProperty }

destructor TPyIndexedProperty.Destroy;
begin
  if Assigned(FPyWrapper) then
    FPyWrapper.Engine.Py_XDECREF(FPyObj);
  inherited;
end;

procedure TPyIndexedProperty.Setup(PyObj: PPyObject; Wrapper: TPyDelphiWrapper; Prop: TRttiIndexedProperty);
begin
  FPyObj:= PyObj;
  Wrapper.Engine.Py_XINCREF(FPyObj);
  FPyWrapper := Wrapper;
  FProperty := Prop;
end;

class procedure TPyIndexedProperty.SetupType(PythonType : TPythonType);
begin
  inherited;
  PythonType.Name := 'IndexedPropertyType';
  PythonType.TypeName := 'IndexedProperty';
  PythonType.GenerateCreateFunction := False;
  PythonType.DocString.Text := 'Indexed property wrapper';
  PythonType.Services.Basic := [bsRepr, bsStr];

  PythonType.Services.Mapping := [msSubscript, msAssSubscript]
end;

function TPyIndexedProperty.MpSubscript(obj: PPyObject) : PPyObject;
var
  PyArgs: PPyObject;
  PascalObject: TObject;
  ErrMsg: string;
begin
  if not ValidateClassProperty(FPyObj, FProperty.Parent.Handle, PascalObject, ErrMsg) then
  begin
    InvalidArguments(FProperty.Name, ErrMsg);
    Exit(nil);
  end;

  // obj is a tuple only if we have more than one arguments
  if FPyWrapper.Engine.PyTuple_Check(obj) then
    PyArgs := obj
  else
    PyArgs := FPyWrapper.Engine.MakePyTuple([obj]);

  Result := RttiCall(PascalObject, FPyWrapper, FProperty.ReadMethod,
    PyArgs, nil);

  if not FPyWrapper.Engine.PyTuple_Check(obj) then
    FPyWrapper.Engine.Py_DECREF(PyArgs); // release created tuple
end;

function TPyIndexedProperty.MpAssSubscript(obj1, obj2: PPyObject) : Integer;
var
  Engine: TPythonEngine;
  PyArgs: PPyObject;
  PascalObject: TObject;
  TempPy: PPyObject;
  Count, Index: Integer;
  ErrMsg: string;
begin
  Result := -1; // Signals failure

  Engine := FPyWrapper.Engine;
  if not FProperty.IsWritable then
  begin
    with Engine do
      PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(rs_NotWritable)));
    Exit;
  end;

  if not ValidateClassProperty(FPyObj, FProperty.Parent.Handle, PascalObject, ErrMsg) then
  begin
    InvalidArguments(FProperty.Name, ErrMsg);
    Exit;
  end;

  // obj is a tuple only if we have more than one arguments
  if Engine.PyTuple_Check(obj1) then
  begin
    Count := Engine.PyTuple_Size(obj1);
    PyArgs := Engine.PyTuple_New(Count + 1);
    for Index := 0 to Count - 1 do
    begin
      TempPy := Engine.PyTuple_GetItem(obj1, Index);
      Engine.Py_XINCREF(TempPy);
      Engine.PyTuple_SetItem(PyArgs, Index, TempPy);
    end;
    Engine.Py_XINCREF(obj2);
    Engine.PyTuple_SetItem(PyArgs, Count, obj2);
  end
  else
    PyArgs := Engine.MakePyTuple([obj1, obj2]);

  TempPy := RttiCall(PascalObject, FPyWrapper, FProperty.WriteMethod,
    PyArgs, nil);

  Engine.Py_DECREF(PyArgs);  // release created tuple

  if TempPy <> nil then
  begin
    Engine.Py_DECREF(TempPy);  //Should be Py_None
    Result := 0; // Signal success
  end;
end;

{ TRttiEventHandler }
constructor TRttiEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject;
  AMethodType: TRttiMethodType);
begin
  inherited Create(PyDelphiWrapper, Component, PropertyInfo, Callable);
  MethodType := AMethodType;
  FMethodImplementation := AMethodType.CreateImplementation(nil, ImplCallback);
end;

destructor TRttiEventHandler.Destroy;
begin
  FMethodImplementation.Free;
  inherited;
end;

function TRttiEventHandler.CodeAddress: Pointer;
begin
  if Assigned(FMethodImplementation) then
    Result := FMethodImplementation.CodeAddress
  else
    Result := nil;
end;

class procedure TRttiEventHandler.ImplCallback(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
var
  EventHandler: TRttiEventHandler;
  Params: TArray<TRttiParameter>;
  PyArgs: PPyObject;
  PyResult: PPyObject;
  TempPy: PPyObject;
  Index: Integer;
  Engine: TPythonEngine;
  ErrMsg: string;
begin
  EventHandler := Args[0].AsObject as TRttiEventHandler;
  Params := EventHandler.MethodType.GetParameters;

  if Length(Args) <> Length(Params) + 1 then  // +1 for Self
  begin
    InvalidArguments(string(EventHandler.PropertyInfo.Name), rs_IncompatibleArguments);
    Exit;
  end;

  Engine := EventHandler.PyDelphiWrapper.Engine;

  // Set up the python arguments
  PyArgs := Engine.PyTuple_New(Length(Args) - 1);  //Ignore Self
  try
    for Index := 1 to Length(Args) - 1 do
    begin
      if Params[Index - 1].Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> [] then
        TempPy := CreateVarParam(EventHandler.PyDelphiWrapper, Args[Index])
      else
        TempPy := TValueToPyObject(Args[Index], EventHandler.PyDelphiWrapper, ErrMsg);
      if TempPy <> nil then
        Engine.PyTuple_SetItem(PyArgs, Index - 1, TempPy)
      else
      begin
        InvalidArguments(string(EventHandler.PropertyInfo.Name), rs_IncompatibleArguments);
        Engine.CheckError;  // will raise an Exception
      end;
    end;

    // Make the call
    PyResult := Engine.PyObject_CallObject(EventHandler.Callable, PyArgs);

    // deal with var/out parameters
    for Index := 1 to Length(Args) - 1 do
      if Params[Index - 1].Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> [] then
      begin
        TempPy := Engine.PyTuple_GetItem(PyArgs, Index - 1);

        if not PyObjectToTValue((PythonToDelphi(TempPy) as TPyDelphiVarParameter).Value,
          Params[Index- 1].ParamType, Args[Index], ErrMsg) then
        begin
          InvalidArguments(string(EventHandler.PropertyInfo.Name), rs_IncompatibleArguments);
          Engine.CheckError;  // will raise an Exception
        end;
      end;

    if Assigned(PyResult) and (EventHandler.MethodType.ReturnType <> nil) and
      not PyObjectToTValue(PyResult, EventHandler.MethodType.ReturnType, Result, ErrMsg)
    then
      Engine.PyErr_SetString(Engine.PyExc_TypeError^, PAnsiChar(Engine.EncodeString(
        Format(rs_ErrInvalidRet, [string(EventHandler.PropertyInfo.Name), ErrMsg]))));
    Engine.Py_XDECREF(PyResult);
  finally
    Engine.Py_XDECREF(PyArgs);
  end;
  Engine.CheckError;
end;

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

procedure InvalidArguments(const MethName, ErrMsg : string);
begin
  with GetPythonEngine do
    PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(
      Format(rs_ErrInvalidArgs, [MethName, ErrMsg]))));
end;

function ValidateClassRef(PyValue: PPyObject; RefClass: TClass;
  out ClassRef: TClass; out ErrMsg: string): Boolean;
var
  LPythonType: TPythonType;
begin
  ClassRef := nil;
  if (PyValue = GetPythonEngine.Py_None) then begin
    Result := True;
    Exit;
  end;

  Result := False;
  // Is PyValue a Python type?
  if not GetPythonEngine.PyClass_Check(PyValue) then
  begin
    ErrMsg := rs_ExpectedClass;
    Exit;
  end;

  LPythonType := FindPythonType(PPyTypeObject(PyValue));
  if Assigned(LPythonType) then
  begin
    if Assigned(LPythonType) and LPythonType.PyObjectClass.InheritsFrom(TPyDelphiObject) then
    begin
      ClassRef := TPyDelphiObjectClass(LPythonType.PyObjectClass).DelphiObjectClass;
      if ClassRef.InheritsFrom(RefClass) then
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

{$IFDEF EXTENDED_RTTI}
function DynArrayToPython(const Value: TValue; DelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): PPyObject;
var
  I: Integer;
  PyEngine: TPythonEngine;
  PyObj: PPyObject;
begin
  PyEngine := GetPythonEngine();
  Result := PyEngine.PyList_New(Value.GetArrayLength);
  for I := 0 to Value.GetArrayLength() - 1 do
  begin
    PyObj := TValueToPyObject(Value.GetArrayElement(i), DelphiWrapper, ErrMsg);
    if not Assigned(PyObj) then
    begin
      PyEngine.Py_DECREF(Result);
      Result := nil;
      Break;
    end;
    PyEngine.PyList_SetItem(Result, I, PyObj);
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
    else
      ErrMsg := rs_ErrValueToPython;
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
      tkString, tkLString, tkChar:
        begin
          if GetPythonEngine.PyBytes_Check(PyValue) then
            V := TValue.From(GetPythonEngine.PyBytesAsAnsiString(PyValue))
          else
            V := GetPythonEngine.PyObjectAsString(PyValue);
          Value := V.Cast(TypeInfo);
          Result := True;
        end;
      tkWString, tkUString, tkWChar:
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
      tkRecord, tkInterface,{$IFDEF MANAGED_RECORD} tkMRecord,{$ENDIF}
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

function ValidateDynArray(PyValue: PPyObject; const RttiType: TRttiType;
  out ParamValue: TValue; out ErrMsg: string): Boolean;
var
  Arr: array of TValue;
  I: Integer;
  elType: PPTypeInfo;
  PyEngine: TPythonEngine;
  RttiContext: TRttiContext;
  ElementType: TRttiType;
  ArrElem: PPyObject;
begin
  Result := False;
  PyEngine := GetPythonEngine;

  if not PyEngine.PySequence_Check(PyValue) = 0 then
  begin
    ErrMsg := rs_ExpectedSequence;
    Exit;
  end;

  if (RttiType = nil) or (RttiType.Handle = nil) or (GetTypeData(RttiType.Handle) = nil) then
    Exit;
  elType := GetTypeData(RttiType.Handle).elType;
  if elType = nil then
    elType := GetTypeData(RttiType.Handle).elType2;
  if elType = nil then
    Exit;

  ElementType := RttiContext.GetType(elType^);
  if ElementType = nil then
    Exit;

  try
    SetLength(Arr, PyEngine.PySequence_Length(PyValue));
    for I := 0 to PyEngine.PySequence_Length(PyValue) - 1 do
    begin
      ArrElem := PyEngine.PySequence_GetItem(PyValue, I);
      if not PyObjectToTValue(ArrElem, ElementType, Arr[I], ErrMsg) then
        Break;
    end;
    ParamValue := TValue.FromArray(RttiType.Handle, Arr);
    Result := True;
  except
    ErrMsg := rs_IncompatiblePythonType;
  end;
end;

function ValidatePointer(PyValue: PPyObject; const RttiType: TRttiType;
  out ParamValue: TValue; out ErrMsg: string): Boolean;
var
  RefType: TRttiType;
  PyEngine: TPythonEngine;
  P: Pointer;
begin
  Result := False;
  PyEngine := GetPythonEngine;

  if (RTTIType is TRttiPointerType) then
  begin
    RefType := TRttiPointerType(RTTIType).ReferredType;
    if Assigned(RefType) and (RefType.Name = 'PyObject') then
    begin
      Result := True;
      ParamValue := TValue.From<PPyObject>(PyValue);
    end
    else if PyEngine.PyLong_Check(PyValue) then
    begin
      P := PyEngine.PyLong_AsVoidPtr(PyValue);
      if PyEngine.PyErr_Occurred = nil then
      begin
        Result := True;
        ParamValue := TValue.From<Pointer>(P);
      end
      else
        PyEngine.PyErr_Clear;
    end;
  end;
  if not Result then
    ErrMsg := rsExpectedPointer;
end;

function PyObjectToTValue(PyArg: PPyObject; ArgType: TRttiType;
  out Arg: TValue; out ErrMsg: string): Boolean;
var
  Obj: TObject;
  ClassRef: TClass;
begin
  if ArgType = nil then
    Exit(False);

  case ArgType.TypeKind of
    tkClass:
      begin
        Result := ValidateClassProperty(PyArg, ArgType.Handle, Obj, ErrMsg);
        if Result then
          Arg := Obj;
      end;
    tkClassRef:
      begin
        Result := ValidateClassRef(PyArg,
          (ArgType as TRttiClassRefType).MetaclassType, ClassRef, ErrMsg);
        if Result then
          Arg := ClassRef;
      end;
    tkInterface:
      Result :=  ValidateInterfaceProperty(PyArg,
        ArgType as TRttiInterfaceType, Arg, ErrMsg);
    tkRecord{$IFDEF MANAGED_RECORD}, tkMRecord{$ENDIF}:
      Result := ValidateRecordProperty(PyArg, ArgType.Handle, Arg, ErrMsg);
    tkDynArray:
      Result := ValidateDynArray(PyArg, ArgType, Arg, ErrMsg);
    tkPointer:
      Result := ValidatePointer(PyArg, ArgType, Arg, ErrMsg);
  else
    Result := SimplePythonToValue(PyArg, ArgType.Handle, Arg, ErrMsg);
  end;
end;


function PyArgsToValues(PyArgs: PPyObject; Method: TRttiMethod;
  out Args: array of TValue; out VarParamIndices: TArray<Integer>): Boolean;
var
  Index: Integer;
  PyArg : PPyObject;
  Param: TRttiParameter;
  Engine: TPythonEngine;
  ErrMsg: string;
  Params : TArray<TRttiParameter>;
begin
  Params := Method.GetParameters;
  if Length(Args) <> Length(Params) then
    Exit(False);

  VarParamIndices := [];
  Engine := GetPythonEngine;
  for Index := 0 to Length(Params) - 1 do
  begin
    Param := Params[Index];
    PyArg := Engine.PyTuple_GetItem(PyArgs, Index);
    if not PyObjectToTValue(PyArg, Param.ParamType, Args[Index], ErrMsg) then
      Exit(False);
    if (Param.Flags * [TParamFlag.pfVar, TParamFlag.pfOut] <> []) then
      VarParamIndices := VarParamIndices + [Index];
  end;
  Result := True;
end;

function TValueToPyObject(const Value: TValue;
   DelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject;
begin
  if Value.IsEmpty then
    Result := DelphiWrapper.Engine.ReturnNone
  else
    case Value.Kind of
      tkClass: Result := DelphiWrapper.Wrap(Value.AsObject);
      tkClassRef: Result := DelphiWrapper.WrapClass(Value.AsClass);
      tkInterface: Result := DelphiWrapper.WrapInterface(Value);
      tkRecord{$IFDEF MANAGED_RECORD},tkMRecord{$ENDIF}:
        Result := DelphiWrapper.WrapRecord(Value);
      tkArray, tkDynArray:
        Result := DynArrayToPython(Value, DelphiWrapper, ErrMsg);
      tkPointer:
        if Value.TypeInfo = TypeInfo(PPyObject) then
          Result := Value.AsType<PPyObject>
        else
          Result := DelphiWrapper.Engine.PyLong_FromVoidPtr(Value.AsType<Pointer>);
    else
      Result := SimpleValueToPython(Value, ErrMsg);
    end;
end;

function  CreateVarParam(PyDelphiWrapper : TPyDelphiWrapper;
  const AValue: TValue) : PPyObject;
var
  tmp : PPyObject;
  _varParam : TPyDelphiVarParameter;
  ErrMsg: string;
begin
  tmp := TValueToPyObject(AValue, PyDelphiWrapper, ErrMsg);
  if tmp = nil then
    Exit(nil);

  Result := PyDelphiWrapper.VarParamType.CreateInstance;
  _varParam := PythonToDelphi(Result) as TPyDelphiVarParameter;
  _varParam.Value := tmp; // refcount was incremented
  GetPythonEngine.Py_DECREF(tmp);
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
      PyErr_SetString(PyExc_IndexError^, PAnsiChar(EncodeString(
          Format(rs_ErrCheckIndex,[AIndexName, AIndex]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckInt, [AAttributeName]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckFloat, [AAttributeName]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckStr, [AAttributeName]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckCallable, [AAttributeName]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckEnum,
        [AEnumName, AMinValue, AMaxValue, AValue]))));
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
        PyErr_SetString(PyExc_AttributeError^,
          PAnsiChar(EncodeString(Format(rs_ErrCheckObjOfType, [AAttributeName, AExpectedClass.ClassName]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckObj, [AAttributeName]))));
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

function CreateVarParam(PyDelphiWrapper: TPyDelphiWrapper; AClass: TClass): PPyObject; overload;
var
  LTmp: PPyObject;
begin
  Result := PyDelphiWrapper.VarParamType.CreateInstance;
  LTmp := PyDelphiWrapper.WrapClass(AClass);
  (PythonToDelphi(Result) as TPyDelphiVarParameter).Value := LTmp;
  GetPythonEngine.Py_DECREF(LTmp);
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

{$HINTS OFF}
function Abort_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
begin
  Result := nil;
  Abort;
end;
{$HINTS ON}

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
      PyErr_SetString( PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSqAss, [fContainerAccess.Name]))));
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
      PyErr_SetString(PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSqContains, [fContainerAccess.Name]))));
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrCheckBound, [ClassName]))));
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
  // PyObjects created by python should be owned by python.
  // PyObjects created by wrapping pascal object be default are not
  Owned := True;
  if Assigned(APythonType) and (APythonType.Owner is TPyDelphiWrapper) then
    PyDelphiWrapper := TPyDelphiWrapper(APythonType.Owner);
end;

constructor TPyDelphiObject.CreateWith(APythonType: TPythonType; args, kwds: PPyObject);
begin
    with APythonType.Engine do
      PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(
        Format(rs_CannotCreate, [APythonType.TypeName]))));
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

function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  MethName: string; ParentRtti: TRttiStructuredType; ob1, ob2: PPyObject;
  AParentAddrIsClass: Boolean): PPyObject;

  function FindMethod(const MethName:string; RttiType : TRttiType;
    PyArgs: PPyObject; var Args: array of TValue;
    out VarParamIndices: TArray<Integer>; out ErrMsg: string):TRttiMethod;
  // Deals with overloaded methods
  // Constructs the Arg Array
  // PyArgs is a Python tuple
  var
    Method: TRttiMethod;
  begin
    Result := nil;
    ErrMsg := rs_UnknownAttribute;
    for Method in RttiType.GetMethods do
      if SameText(Method.Name, MethName) then begin
        if PyArgsToValues(PyArgs, Method, Args, VarParamIndices) then
        begin
          Result := Method;
          Break;
        end
        else
          ErrMsg := rs_IncompatibleArguments;
      end;
  end;

var
  LArgs: TArray<TValue>;
  LVarParamIndices: TArray<Integer>;
  LArgCount: Integer;
  Method: TRttiMethod;
  ErrMsg : string;

begin
  Result := nil;

  LArgCount := DelphiWrapper.Engine.PyTuple_Size(ob1);
  SetLength(LArgs, LArgCount);

  Method := FindMethod(MethName, ParentRtti, ob1, LArgs, LVarParamIndices, ErrMsg);

  if not Assigned(Method) then begin
    InvalidArguments(MethName, ErrMsg);
    Exit;
  end;

  Result := RttiCall(ParentAddress, DelphiWrapper, Method, LArgs,
    LVarParamIndices, AParentAddrIsClass);
end;

function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  Method: TRttiMethod; ob1, ob2: PPyObject;
  AParentAddrIsClass: Boolean = false): PPyObject;
var
  LArgs: TArray<TValue>;
  LVarParamIndices: TArray<Integer>;
begin
  Result := nil;

  // Ignore keyword arguments ob2
  // ob1 is a tuple with zero or more elements
  SetLength(LArgs, DelphiWrapper.Engine.PyTuple_Size(ob1));
  if not PyArgsToValues(ob1, Method, LArgs, LVarParamIndices) then
  begin
    InvalidArguments(Method.Name, rs_IncompatibleArguments);
    Exit;
  end;

  Result := RttiCall(ParentAddress, DelphiWrapper, Method, LArgs,
    LVarParamIndices, AParentAddrIsClass);
end;


function RttiCall(ParentAddress: pointer; DelphiWrapper: TPyDelphiWrapper;
  Method: TRttiMethod; const Args: TArray<TValue>;
  const  VarParamIndices: TArray<Integer>;
  AParentAddrIsClass: Boolean = false): PPyObject;
var
  Addr: TValue;
  ReturnValue: TValue;
  ErrMsg: string;
  TempPy: PPyObject;
  Index, Pos: Integer;
begin
  // Args and VarParamIndices are already setup and validated
  try
    if Method.Parent is TRttiInstanceType then
      if Method.IsClassMethod or Method.IsStatic then
        if AParentAddrIsClass then
          Addr := TValue.From(TClass(ParentAddress))
        else
          Addr := TValue.From(TObject(ParentAddress).ClassType)
      else
        Addr := TValue.From(TObject(ParentAddress))
    else if Method.Parent is TRttiInterfaceType then
       TValue.Make(@ParentAddress, Method.Parent.Handle, Addr)
    else
      Addr := TValue.From(ParentAddress);

    ReturnValue := Method.Invoke(Addr, Args);

    { Deal with var/out arguments
      e.g.
        procedure Test(var I: Integer)
        use in python:
        ivalue = objref.Test(param)

        procedures/functions with var parameters should return a tuple
        function Test(var I: Integer): Integer
        use in python:
        res, ivalue = objref.Test(ivalue)

        procedure Test(var I: Integer; var S: string);
        use in python:
        ivalue, svalue = objref.Test(ivalue, svalue) }


    if Length(VarParamIndices) = 0 then
      Result := TValueToPyObject(ReturnValue, DelphiWrapper, ErrMsg)
    else if (Method.ReturnType = nil) and (Length(VarParamIndices) = 1) then
      Result := TValueToPyObject(Args[VarParamIndices[0]], DelphiWrapper, ErrMsg)
    else
    begin
      // we return a tuple - start with the return value
      if Method.ReturnType = nil then
        Pos := 0
      else
        Pos := 1;

      Result := DelphiWrapper.Engine.PyTuple_New(Length(VarParamIndices) + Pos);
      if Method.ReturnType <> nil then
      begin
        TempPy := TValueToPyObject(ReturnValue, DelphiWrapper, ErrMsg);

        if TempPy = nil then
        begin
          DelphiWrapper.Engine.Py_DECREF(Result);
          Result := nil;
        end
        else
          DelphiWrapper.Engine.PyTuple_SetItem(Result, 0, TempPy);
      end;

      if Result <> nil then
        for Index in VarParamIndices do
        begin
          TempPy := TValueToPyObject(Args[Index], DelphiWrapper, ErrMsg);
          if TempPy = nil then
          begin
            DelphiWrapper.Engine.Py_DECREF(Result);
            Result := nil;
            Break;
          end
          else
          begin
            DelphiWrapper.Engine.PyTuple_SetItem(Result, Pos, TempPy);
            Inc(Pos);
          end;
        end;
    end;

    if Result = nil then
      with DelphiWrapper.Engine do
        PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(
          Format(rs_ErrInvalidRet, [Method.Name, ErrMsg]))));
  except
    on E: Exception do begin
      Result := nil;
      InvalidArguments(Method.Name, E.Message);
    end;
  end;
end;

function GetRttiProperty(ParentAddr: Pointer; Prop: TRttiProperty;
 PyDelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject;
begin
  Result := nil;
  if Ord(Prop.Visibility) < Ord(mvPublic) then
    ErrMsg := rs_NoAccess
  else if not Prop.IsReadable then
    ErrMsg := rs_NotReadable
  else if Prop.PropertyType = nil then
    ErrMsg := rs_ErrNoTypeInfo
  else if Prop.PropertyType.TypeKind = tkMethod then
  begin
    if (Prop is TRttiInstanceProperty) and  (Prop.Visibility = mvPublished) then
      Result := PyDelphiWrapper.fEventHandlerList.GetCallable(TObject(ParentAddr),
        TRttiInstanceProperty(Prop).PropInfo);
  end
  else
    Result := TValueToPyObject(Prop.GetValue(ParentAddr), PyDelphiWrapper, ErrMsg);
end;

function GetRttiField(ParentAddr: Pointer; Field: TRttiField;
 PyDelphiWrapper: TPyDelphiWrapper; out ErrMsg: string): PPyObject;
begin
  Result := nil;
  if Ord(Field.Visibility) < Ord(mvPublic) then
    ErrMsg := rs_NoAccess
  else if Field.FieldType = nil then
    ErrMsg := rs_ErrNoTypeInfo
  else if Field.FieldType.TypeKind in [tkRecord{$IFDEF MANAGED_RECORD},tkMRecord{$ENDIF}] then
    //Potentially dangerous as the returned value, which is a pointer into the object,
    //could be stored on the python side, then the object freed, and the stored pointer later
    //used to access no longer allocated memory
    //But I can't see any good alternative if Python should be able to write directly into
    //fields of a record that's part of an object.
    Result := PyDelphiWrapper.WrapRecord(PByte(ParentAddr) + Field.Offset, TRttiStructuredType(Field.FieldType))
  else
    Result := TValueToPyObject(Field.GetValue(ParentAddr), PyDelphiWrapper, ErrMsg);
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
        Result := GetRttiProperty(ParentAddr, Prop, PyDelphiWrapper, ErrMsg)
      else
      begin
        Field := ParentType.GetField(AttrName);
        if Field <> nil then
          Result := GetRttiField(ParentAddr, Field, PyDelphiWrapper, ErrMsg)
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

function SetRttiProperty(const ParentAddr: Pointer;  Prop: TRttiProperty;
  Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean;
var
  AttrValue: TValue;
begin
  Result := False;
  if Ord(Prop.Visibility) < Ord(mvPublic) then
    ErrMsg := rs_NoAccess
  else if not Prop.IsWritable then
    ErrMsg := rs_NotWritable
  else if Prop.PropertyType = nil then
    ErrMsg := rs_ErrNoTypeInfo
  else if Prop.PropertyType.TypeKind = tkMethod then
  begin
    if (Prop is TRttiInstanceProperty) and  (Prop.Visibility = mvPublished) then
      Result := PyDelphiWrapper.EventHandlers.Link(TObject(ParentAddr),
        (Prop as TRttiInstanceProperty).PropInfo, Value, ErrMsg)
    else
      ErrMsg := rs_NotPublished;
  end
  else if PyObjectToTValue(Value, Prop.PropertyType, AttrValue, ErrMsg) then
  begin
    Prop.SetValue(ParentAddr, AttrValue);
    Result := True;
  end;
end;

function SetRttiField(const ParentAddr: Pointer;  Field: TRttiField;
  Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean;
var
  AttrValue: TValue;
begin
  Result := False;
  if Ord(Field.Visibility) < Ord(mvPublic) then
    ErrMsg := rs_NoAccess
  else if Field.FieldType = nil then
    ErrMsg := rs_ErrNoTypeInfo
  else if PyObjectToTValue(Value, Field.FieldType, AttrValue, ErrMsg) then
  begin
    Field.SetValue(ParentAddr, AttrValue);
    Result := True;
  end;
end;

function SetRttiAttr(const ParentAddr: Pointer;  ParentType: TRttiStructuredType;
  const AttrName: string; Value: PPyObject;  PyDelphiWrapper: TPyDelphiWrapper;
  out ErrMsg: string): Boolean;
var
  Prop: TRttiProperty;
  Field: TRttiField;
begin
  Result := False;

  try
    Prop := ParentType.GetProperty(AttrName);
    if Prop <> nil then
      Result := SetRttiProperty(ParentAddr, Prop, Value, PyDelphiWrapper, ErrMsg)
    else
    begin
      Field := ParentType.GetField(AttrName);
      if Field <> nil then
        Result := SetRttiField(ParentAddr, Field, Value, PyDelphiWrapper, ErrMsg)
      else
        ErrMsg := rs_UnknownAttribute;
    end;
  except
    on E: Exception do begin
      Result := False;
      ErrMsg := E.Message;
    end;
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
      PyErr_SetString(PyExc_AttributeError^,
        PAnsiChar(EncodeString(Format(rs_ErrAttrGet,[KeyName, ErrMsg]))));
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
      PyErr_SetString(PyExc_AttributeError^, PAnsiChar(EncodeString(
        Format(rs_ErrAttrSet, [KeyName, ErrMsg]))));
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

procedure TPyRttiObject.SetUpFromTValue(const AValue: TValue);
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiStructuredType;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(AValue.TypeInfo) as TRttiStructuredType;
  finally
    LRttiCtx.Free();
  end;

  FCopy := AValue;
  if LRttiType.TypeKind in [tkRecord{$IFDEF MANAGED_RECORD}, tkMRecord{$ENDIF}] then
    SetAddrAndType(FCopy.GetReferenceToRawData(), LRttiType)
  else if LRttiType.TypeKind = tkInterface then
    SetAddrAndType(Pointer(FCopy.GetReferenceToRawData()^), LRttiType)
end;

{ TPyPascalRecord }

function TPyPascalRecord.GetValue: TValue;
begin
  if FCopy.IsEmpty then
    TValue.Make(fAddr, RttiType.Handle, Result)
  else
    Result := FCopy;
end;

function TPyPascalRecord.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(
   Format('<Pascal record of type %s at %x>',
   [RttiType.Name, NativeInt(fAddr)]) )
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
  if FCopy.IsEmpty then
    TValue.Make(@fAddr, RttiType.Handle, Result)
  else
    Result := FCopy;
end;

function TPyPascalInterface.Repr: PPyObject;
begin
  Result := GetPythonEngine.PyUnicodeFromString(
   Format('<Pascal interface of type %s at %x>',
   [RttiType.Name, NativeInt(fAddr)]) )
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
  PyEngine: TPythonEngine;
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
  Result := nil;
  PyEngine := GetPythonEngine;

  // If DelphiObject is nil exit immediately with an error
  if not Assigned(DelphiObject) then
  begin
    PyEngine.PyErr_SetString(PyEngine.PyExc_AttributeError^,
      PAnsiChar(PyEngine.EncodeString(rs_ErrObjectDestroyed)));
    Exit;
  end;

  if not CheckStrAttribute(Key, 'GetAttrO key parameter', KeyName) then
    Exit; // should not happen

  Result := inherited GetAttrO(key);
  if PyEngine.PyErr_Occurred = nil then Exit;  // We found what we wanted

  PyEngine.PyErr_Clear;
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
          Result := PyEngine.VariantAsPyObject(Boolean(GetOrdProp(Self.DelphiObject, PropInfo)))
        else
        {$IFDEF FPC}
          Result := PyEngine.PyUnicodeFromString(GetEnumName(PropInfo^.PropType,
        {$ELSE FPC}
          Result := PyEngine.PyUnicodeFromString(GetEnumName(PropInfo^.PropType^,
        {$ENDIF FPC}
            GetOrdProp(Self.DelphiObject, PropInfo)));
      end
      end else
         Result := PyEngine.VariantAsPyObject(GetPropValue(DelphiObject, PropInfo));
    end;
  except
    on E: Exception do begin
      Result := nil;
      ErrMsg := E.Message;
    end;
  end;
{$ENDIF}
  if not Assigned(Result) then
    PyEngine.PyErr_SetString(PyEngine.PyExc_AttributeError^,
      PAnsiChar(PyEngine.EncodeString(Format(rs_ErrAttrGet,[KeyName, ErrMsg]))));
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
  i: Integer;
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
      RttiType := Context.GetType(DelphiObject.ClassType());
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
  Result := Copy(DelphiObjectClass.ClassName, 2);
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
      PyErr_SetString(PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrIterSupport,
        [Self.ClassName]))));
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
  PythonType.AddGetSet('ClassName', @TPyDelphiObject.Get_ClassName, nil,
    'Returns the TObject.ClassName', nil);
  PythonType.AddGetSet('__bound__', @TPyDelphiObject.Get_Bound, nil,
    'Returns True if the wrapper is still bound to the Delphi instance.', nil);
  PythonType.AddGetSet('__owned__', @TPyDelphiObject.Get_Owned, @TPyDelphiObject.Set_Owned,
    'Boolean read/write property that determines weather the wrapper owns the Delphi instance.', nil);
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

  // If DelphiObject is nil exit immediately with an error
  if not Assigned(DelphiObject) then
  begin
    PyEngine.PyErr_SetString(PyEngine.PyExc_AttributeError^,
      PAnsiChar(PyEngine.EncodeString(rs_ErrObjectDestroyed)));
    Exit;
  end;

  if not CheckStrAttribute(Key, 'SetAttrO key parameter', KeyName) then
    Exit; // should not happen

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

class procedure TPyDelphiObject.SetupType(APythonType: TPythonType);
var
  _ContainerAccessClass : TContainerAccessClass;
  TypeName: string;
  PyWrapper: TPyDelphiWrapper;
  NearestAncestorClass: TClass;
  RegisteredClass: TRegisteredClass;
  Index: Integer;
  {$IFDEF EXTENDED_RTTI}
  LDocStr: string;
  ExcludedMembers: TArray<string>;
  {$ENDIF EXTENDED_RTTI}
begin
  inherited;
  // Deal with generics
  TypeName := GetTypeName;
  if Pos('<', TypeName) > 0 then
  begin
    TypeName := StringReplace(TypeName, '<', '_', [rfReplaceAll]);
    TypeName := StringReplace(TypeName, '>', '_', [rfReplaceAll]);
    TypeName := StringReplace(TypeName, '.', '_', [rfReplaceAll]);
    TypeName := StringReplace(TypeName, ',', '_', [rfReplaceAll]);
  end;

  APythonType.TypeName := AnsiString(TypeName);
  APythonType.Name := string(APythonType.TypeName) + TPythonType.TYPE_COMP_NAME_SUFFIX;
  APythonType.GenerateCreateFunction := False;
  APythonType.DocString.Text := 'Wrapper for Pascal class ' + DelphiObjectClass.ClassName;
  APythonType.Services.Basic := [bsGetAttrO, bsSetAttrO, bsRepr, bsStr, bsRichCompare];
  _ContainerAccessClass := GetContainerAccessClass;
  if Assigned(_ContainerAccessClass) then
  begin
    APythonType.Services.Basic := APythonType.Services.Basic + [bsIter];
    APythonType.Services.Sequence := APythonType.Services.Sequence + [ssLength, ssItem];
    if _ContainerAccessClass.SupportsWrite then
      APythonType.Services.Sequence := APythonType.Services.Sequence + [ssAssItem];
    if _ContainerAccessClass.SupportsIndexOf then
      APythonType.Services.Sequence := APythonType.Services.Sequence + [ssContains];
  end;

  // Find nearest registered ancestor class and set it as base
  PyWrapper := APythonType.Owner as TPyDelphiWrapper;
  NearestAncestorClass := nil;
  for Index := PyWrapper.fClassRegister.Count - 1 downto 0 do
  begin
    RegisteredClass := PyWrapper.fClassRegister[Index] as TRegisteredClass;
    if DelphiObjectClass.InheritsFrom(RegisteredClass.DelphiClass) then
    begin
      APythonType.BaseType := RegisteredClass.PythonType;
      NearestAncestorClass := RegisteredClass.DelphiClass;
      Break;
    end;
  end;
  {$IFDEF EXTENDED_RTTI}
  //Try to load the class doc string from doc server
  if Assigned(PyDocServer) and PyDocServer.Initialized and
    PyDocServer.ReadTypeDocStr(DelphiObjectClass.ClassInfo, LDocStr)
  then
    APythonType.DocString.Text := LDocStr;

  ExcludedMembers := ExcludedExposedMembers(APythonType);
  ExposeMethods(DelphiObjectClass, NearestAncestorClass, APythonType,
    PyWrapper,  ExcludedMembers);
  ExposeFields(DelphiObjectClass, NearestAncestorClass, APythonType,
    PyWrapper, ExcludedMembers);
  ExposeProperties(DelphiObjectClass, NearestAncestorClass, APythonType,
    PyWrapper, ExcludedMembers);
  ExposeIndexedProperties(DelphiObjectClass, NearestAncestorClass, APythonType,
    PyWrapper, ExcludedMembers);
  {$ENDIF EXTENDED_RTTI}
end;

{$IFDEF EXTENDED_RTTI}
class function TPyDelphiObject.ExcludedExposedMembers(APythonType: TPythonType): TArray<string>;
var
  I, MethodCount: Integer;
begin
  MethodCount := APythonType.MethodCount;
  SetLength(Result, MethodCount + APythonType.GetSetCount);

  for I := 0 to MethodCount - 1 do
    Result[I] := string(APythonType.Methods[I].ml_name);

  for I := 0 to APythonType.GetSetCount - 1 do
    Result[MethodCount + I] := string(APythonType.GetSet[I].name);
end;

class procedure TPyDelphiObject.ExposeMethods(AClass: TClass;
  NearestAncestorClass: TClass; APythonType: TPythonType;
  APyDelphiWrapper: TPyDelphiWrapper; AExcludedMethodNames: TArray<string>);
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiStructuredType;
  LRttiMethod: TRttiMethod;
  AddedMethods: TArray<string>;
  LExposedMethod: TExposedMethod;
  LClass: TClass;
  LDocStr: string;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(AClass) as TRttiStructuredType;

    AddedMethods := [];
    for LRttiMethod in LRttiType.GetMethods do begin
      if (Ord(LRttiMethod.Visibility) < Ord(TMemberVisibility.mvPublic)) then
        Continue;

      // Ingnore constructors, destructors and operator overloads
      if LRttiMethod.MethodKind in [mkConstructor, mkDestructor,
        mkClassConstructor, mkClassDestructor, mkOperatorOverload]
      then
        Continue;

      // Ignore methods with unhandled return type
      if Assigned(LRttiMethod.ReturnType) and (LRttiMethod.ReturnType.TypeKind
        in [tkUnknown, tkMethod, tkProcedure])
      then
        Continue;

      // Skip methods declared in NearestAncestorClass and its ancestors
      LClass := (LRttiMethod.Parent as TRttiInstanceType).MetaclassType;
      if (NearestAncestorClass <> nil) and ((LClass = NearestAncestorClass) or
        not (LClass.InheritsFrom(NearestAncestorClass)))
      then
        Continue;

      // Ignore excluded methods
      if MatchStr(LRttiMethod.Name, AExcludedMethodNames) then
        Continue;

      // Ignore duplicate methods
      if MatchStr(LRttiMethod.Name, AddedMethods) then
        Continue;

      AddedMethods := AddedMethods + [LRttiMethod.Name];

      // Create the exposed method
      LExposedMethod := TExposedMethod.Create(LRttiMethod,
        APyDelphiWrapper, APythonType, LRttiType);

      //Try to load the method doc string from doc server
      if Assigned(PyDocServer) and PyDocServer.Initialized and
        PyDocServer.ReadMemberDocStr(LRttiMethod, LDocStr)
      then
        LExposedMethod.DocString := Utf8Encode(LDocStr)
      else
        //Build the DocStr including method args
        LExposedMethod.DocString :=
          Utf8Encode(TExposedMethod.MethodDocStr(LRttiMethod));

      // Keep it alive until the Wrapper is Finalized
      APyDelphiWrapper.fExposedMembers.Add(LExposedMethod);

      //Adds the Python method
      if LRttiMethod.IsStatic then
        APythonType.AddStaticMethodWithKeywords(
          PAnsiChar(LExposedMethod.Name),
          LExposedMethod.Callback,
          PAnsiChar(LExposedMethod.DocString))
      else if LRttiMethod.IsClassMethod then
        APythonType.AddClassMethodWithKeywords(
          PAnsiChar(LExposedMethod.Name),
          LExposedMethod.Callback,
          PAnsiChar(LExposedMethod.DocString))
      else
        APythonType.AddMethodWithKeywords(
          PAnsiChar(LExposedMethod.Name),
          LExposedMethod.Callback,
          PAnsiChar(LExposedMethod.DocString));
    end;
  finally
    LRttiCtx.Free;
  end;
end;

class procedure TPyDelphiObject.ExposeFields(AClass: TClass;
  NearestAncestorClass: TClass; APythonType: TPythonType;
  APyDelphiWrapper: TPyDelphiWrapper; AExcludedFieldNames: TArray<string>);
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiStructuredType;
  LRttiField: TRttiField;
  AddedFields: TArray<string>;
  LExposedField: TExposedField;
  LClass: TClass;
  LDocStr: string;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(AClass) as TRttiStructuredType;

    AddedFields := [];
    for LRttiField in LRttiType.GetFields do begin
      // Ignore methods with visibility lower than public
      if (Ord(LRttiField.Visibility) < Ord(TMemberVisibility.mvPublic)) then
        Continue;

      // Skip methods declared in NearestAncestorClass and its ancestors
      LClass := (LRttiField.Parent as TRttiInstanceType).MetaclassType;
      if (NearestAncestorClass <> nil) and ((LClass = NearestAncestorClass) or
        not (LClass.InheritsFrom(NearestAncestorClass)))
      then
        Continue;

      // Ignore excluded fields
      if MatchStr(LRttiField.Name, AExcludedFieldNames) then
        Continue;

      // Ignore duplicate fields
      if MatchStr(LRttiField.Name, AddedFields) then
        Continue;

      // Skip if the FieldType is missing
      if LRttiField.FieldType = nil then
        Continue;

      // Skip if the type cannot be handled
      if LRttiField.FieldType.TypeKind  in [tkUnknown, tkMethod, tkProcedure] then
        Continue;

      AddedFields := AddedFields + [LRttiField.Name];

      // Create the exposed method
      LExposedField := TExposedField.Create(LRttiField,
        APyDelphiWrapper, APythonType, LRttiType);

      //Try to load the method doc string from doc server
      if Assigned(PyDocServer) and PyDocServer.Initialized and
        PyDocServer.ReadMemberDocStr(LRttiField, LDocStr)
      then
        LExposedField.DocString := Utf8Encode(LDocStr);

      // Keep it alive until the Wrapper is Finalized
      APyDelphiWrapper.fExposedMembers.Add(LExposedField);

      //Adds the Python getset
      APythonType.AddGetSet(
        PAnsiChar(LExposedField.Name),
        LExposedField.GetterCallback,
        LExposedField.SetterCallback,
        PAnsiChar(LExposedField.DocString),
        nil);
    end;
  finally
    LRttiCtx.Free;
  end;
end;

class procedure TPyDelphiObject.ExposeProperties(AClass: TClass;
  NearestAncestorClass: TClass; APythonType: TPythonType;
  APyDelphiWrapper: TPyDelphiWrapper; AExcludedPropertyNames: TArray<string>);
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiStructuredType;
  LRttiProperty: TRttiProperty;
  AddedProperties: TArray<string>;
  LExposedProperty: TExposedGetSet;
  LClass: TClass;
  LSetter: Pointer;
  LDocStr: string;
begin
  LRttiCtx := TRttiContext.Create();
  try
    LRttiType := LRttiCtx.GetType(AClass) as TRttiStructuredType;

    AddedProperties := [];
    for LRttiProperty in LRttiType.GetProperties do begin
      // Ignore methods with visibility lower than public
      if (Ord(LRttiProperty.Visibility) < Ord(TMemberVisibility.mvPublic)) then
        Continue;

      // Skip methods declared in NearestAncestorClass and its ancestors
      LClass := (LRttiProperty.Parent as TRttiInstanceType).MetaclassType;
      if (NearestAncestorClass <> nil) and ((LClass = NearestAncestorClass) or
        not (LClass.InheritsFrom(NearestAncestorClass)))
      then
        Continue;

      // Ignore excluded properties
      if MatchStr(LRttiProperty.Name, AExcludedPropertyNames) then
        Continue;

      // Ignore duplicate properties
      if MatchStr(LRttiProperty.Name, AddedProperties) then
        Continue;

      // Skip if the PropertyType is missing
      if LRttiProperty.PropertyType = nil then
        Continue;

      // Skip non readable properties
      if not LRttiProperty.IsReadable then
        Continue;

      if (LRttiProperty.PropertyType is TRttiMethodType) and
        (LRttiProperty.Visibility = TMemberVisibility.mvPublished) and
        (APyDelphiWrapper.EventHandlers.FindHandler(LRttiProperty.PropertyType.Handle) = nil)
      then
        LExposedProperty := TExposedEvent.Create(LRttiProperty,
        APyDelphiWrapper, APythonType, LRttiType)
      else
      begin
        // Skip if the type cannot be handled
        if LRttiProperty.PropertyType.TypeKind in [tkUnknown, tkMethod, tkProcedure] then
          Continue;

        // Create the exposed property
        LExposedProperty := TExposedProperty.Create(LRttiProperty,
          APyDelphiWrapper, APythonType, LRttiType);
      end;

      AddedProperties := AddedProperties + [LRttiProperty.Name];

      //Try to load the method doc string from doc server
      if Assigned(PyDocServer) and PyDocServer.Initialized and
        PyDocServer.ReadMemberDocStr(LRttiProperty, LDocStr)
      then
        LExposedProperty.DocString := Utf8Encode(LDocStr);

      // Keep it alive until the Wrapper is Finalized
      APyDelphiWrapper.fExposedMembers.Add(LExposedProperty);

      if LRttiProperty.IsWritable then
        LSetter := LExposedProperty.SetterCallback
      else
        LSetter := nil;

      //Adds the Python getset
      APythonType.AddGetSet(
        PAnsiChar(LExposedProperty.Name),
        LExposedProperty.GetterCallback,
        LSetter,
        PAnsiChar(LExposedProperty.DocString),
        nil);
    end;
  finally
    LRttiCtx.Free;
  end;
end;

class procedure TPyDelphiObject.ExposeIndexedProperties(AClass: TClass;
  NearestAncestorClass: TClass; APythonType: TPythonType;
  APyDelphiWrapper: TPyDelphiWrapper; AExcludedPropertyNames: TArray<string>);
var
  LRttiCtx: TRttiContext;
  LRttiType: TRttiStructuredType;
  LRttiProperty: TRttiIndexedProperty;
  AddedProperties: TArray<string>;
  LExposedProperty: TExposedIndexedProperty;
  LClass: TClass;
  LDocStr: string;
begin
  LRttiCtx := TRttiContext.Create();
  APythonType.Tag := 0;  // may be assigned a default indexed property
  try
    LRttiType := LRttiCtx.GetType(AClass) as TRttiStructuredType;

    AddedProperties := [];
    for LRttiProperty in LRttiType.GetIndexedProperties do begin
      // Ignore methods with visibility lower than public
      if (Ord(LRttiProperty.Visibility) < Ord(TMemberVisibility.mvPublic)) then
        Continue;

      // Skip methods declared in NearestAncestorClass and its ancestors
      LClass := (LRttiProperty.Parent as TRttiInstanceType).MetaclassType;
      if (NearestAncestorClass <> nil) and ((LClass = NearestAncestorClass) or
        not (LClass.InheritsFrom(NearestAncestorClass)))
      then
        Continue;

      // Ignore excluded properties
      if MatchStr(LRttiProperty.Name, AExcludedPropertyNames) then
        Continue;

      // Ignore duplicate properties
      if MatchStr(LRttiProperty.Name, AddedProperties) then
        Continue;

      // Skip if the PropertyType is missing
      if LRttiProperty.PropertyType = nil then
        Continue;

      // Skip non readable properties
      if not LRttiProperty.IsReadable then
        Continue;

      // Skip if the type cannot be handled
      if LRttiProperty.PropertyType.TypeKind  in [tkUnknown, tkMethod, tkProcedure] then
        Continue;

      AddedProperties := AddedProperties + [LRttiProperty.Name];

      // Create the exposed method
      LExposedProperty := TExposedIndexedProperty.Create(LRttiProperty,
        APyDelphiWrapper, APythonType, LRttiType);

      //Try to load the method doc string from doc server
      if Assigned(PyDocServer) and PyDocServer.Initialized and
        PyDocServer.ReadMemberDocStr(LRttiProperty, LDocStr)
      then
        LExposedProperty.DocString := Utf8Encode(LDocStr);

      // Keep it alive until the Wrapper is Finalized
      APyDelphiWrapper.fExposedMembers.Add(LExposedProperty);

      //Adds the Python getset
      APythonType.AddGetSet(
        PAnsiChar(LExposedProperty.Name),
        LExposedProperty.GetterCallback,
        nil,
        PAnsiChar(LExposedProperty.DocString),
        nil);

      // Store the default property in the type
      if LRttiProperty.IsDefault and (APythonType.Tag = 0) then
      begin
        APythonType.Tag := NativeInt(LRttiProperty);
        if LRttiProperty.IsWritable then
          APythonType.Services.Mapping := [msSubscript, msAssSubscript]
        else
          APythonType.Services.Mapping := [msSubscript];
      end;
    end;
  finally
    LRttiCtx.Free;
  end;
end;

function TPyDelphiObject.MpSubscript(obj: PPyObject) : PPyObject;
var
  PyArgs: PPyObject;
  Prop: TRttiIndexedProperty;
begin
  Assert(PythonType.Tag <> 0);
  Prop := TRttiIndexedProperty(PythonType.Tag);

  // obj is a tuple only if we have more than one arguments
  if PyDelphiWrapper.Engine.PyTuple_Check(obj) then
    PyArgs := obj
  else
    PyArgs := PyDelphiWrapper.Engine.MakePyTuple([obj]);

  Result := RttiCall(DelphiObject, PyDelphiWrapper, Prop.ReadMethod,
    PyArgs, nil);

  if not PyDelphiWrapper.Engine.PyTuple_Check(obj) then
    PyDelphiWrapper.Engine.Py_DECREF(PyArgs); // release created tuple
end;

function TPyDelphiObject.MpAssSubscript(obj1, obj2: PPyObject) : Integer;
var
  Engine: TPythonEngine;
  Prop: TRttiIndexedProperty;
  PyArgs: PPyObject;
  TempPy: PPyObject;
  Count, Index: Integer;
begin
  Result := -1; // Signals failure

  Assert(PythonType.Tag <> 0);
  Prop := TRttiIndexedProperty(PythonType.Tag);

  Engine := PyDelphiWrapper.Engine;
  if not Prop.IsWritable then
  begin
    with Engine do
      PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(rs_NotWritable)));
    Exit;
  end;

  // obj is a tuple only if we have more than one arguments
  if Engine.PyTuple_Check(obj1) then
  begin
    Count := Engine.PyTuple_Size(obj1);
    PyArgs := Engine.PyTuple_New(Count + 1);
    for Index := 0 to Count - 1 do
    begin
      TempPy := Engine.PyTuple_GetItem(obj1, Index);
      Engine.Py_XINCREF(TempPy);
      Engine.PyTuple_SetItem(PyArgs, Index, TempPy);
    end;
    Engine.Py_XINCREF(obj2);
    Engine.PyTuple_SetItem(PyArgs, Count, obj2);
  end
  else
    PyArgs := Engine.MakePyTuple([obj1, obj2]);

  TempPy := RttiCall(DelphiObject, PyDelphiWrapper, Prop.WriteMethod,
    PyArgs, nil);

  Engine.Py_DECREF(PyArgs);  // release created tuple

  if TempPy <> nil then
  begin
    Engine.Py_DECREF(TempPy);  //Should be Py_None
    Result := 0; // Signal success
  end;
end;


{$ENDIF EXTENDED_RTTI}

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
      PyErr_SetString( PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSqAss, [Self.ClassName]))));
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
      PyErr_SetString(PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSequence, [Self.ClassName]))));
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
      PyErr_SetString(PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSequence, [Self.ClassName]))));
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
      PyErr_SetString( PyExc_SystemError^,
        PAnsiChar(EncodeString(Format(rs_ErrSequence, [Self.ClassName]))));
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
  Result := RttiCall(ParentAddress, fDelphiWrapper, MethName, ParentRtti, ob1, ob2);
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
        PyErr_SetString(PyExc_TypeError^,
          PAnsiChar(EncodeString(Format(rs_ErrInvalidArgs,
          [MethodInfo.Name, E.Message]))));
      end;
    end;
end;
{$ENDIF}

{ TPyDelphiMethodObject }

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


{ TPyClassWrapper<T> }

class function TPyClassWrapper<T>.DelphiObjectClass: TClass;
begin
  Result := T;
end;

function TPyClassWrapper<T>.GetDelphiObject: T;
begin
  Result := T(inherited DelphiObject)
end;

class procedure TPyClassWrapper<T>.RegisterMethods(PythonType: TPythonType);
begin
  // Do not call inherited;
end;

procedure TPyClassWrapper<T>.SetDelphiObject(const Value: T);
begin
  inherited DelphiObject := Value;
end;

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

constructor TBaseEventHandler.Create(PyDelphiWrapper : TPyDelphiWrapper;
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

destructor TBaseEventHandler.Destroy;
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

procedure TBaseEventHandler.Unsubscribe;
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

function TEventHandlers.Add(AEventHandler: TBaseEventHandler) : Boolean;
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

function TEventHandlers.GetItem(AIndex: Integer): TBaseEventHandler;
begin
  Result := TBaseEventHandler(fItems[AIndex]);
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
  fExposedMembers := TObjectList.Create(True);

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
        PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(
          Format(rs_ErrInvalidArgs, ['CreateComponent', rs_InvalidClass]))));
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
      PyErr_SetString(PyExc_TypeError^, PAnsiChar(EncodeString(
        Format(rs_ErrInvalidArgs, ['CreateComponent', '']))));
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
  if Assigned(PyDocServer) then
    PyDocServer.Initialize;
  fRecordType           := RegisterHelperType(TPyPascalRecord);
  fInterfaceType        := RegisterHelperType(TPyPascalInterface);
  RegisterHelperType(TPyIndexedProperty);
{$ENDIF}

  // Create and Register Wrapper for TObject
  RegisterDelphiWrapper(TPyDelphiObject);

  // Register Notify event Handler
  EventHandlers.RegisterHandler(TNotifyEventHandler);

  // Register wrappers for each Delphi unit
  for i := 0 to RegisteredUnits.Count-1 do
    RegisteredUnits[i].RegisterWrappers(Self);
{$IFDEF EXTENDED_RTTI}
  if Assigned(PyDocServer) then
    PyDocServer.Finalize;
{$ENDIF}
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

procedure TPyDelphiWrapper.DefineVar(const AName: string; AValue: TObject;
  AOwnership: TObjectOwnership);
var
  _obj : PPyObject;
begin
  Assert(Assigned(Module));
  _obj := Wrap(AValue, AOwnership);
  Module.SetVar(AnsiString(AName), _obj);
  Engine.Py_DECREF(_obj);
end;

procedure TPyDelphiWrapper.DefineVar(const AName: string; AValue: TClass);
var
  LObj: PPyObject;
begin
  Assert(Assigned(Module));
  LObj := WrapClass(AValue);
  Module.SetVar(AnsiString(AName), LObj);
  Engine.Py_DECREF(LObj);
end;

destructor TPyDelphiWrapper.Destroy;
begin
  if Initialized then Finalize;
  UnsubscribeFreeNotifications;
  // note that those pointers MUST be set to nil, otherwise it will crash
  // when calling inherited, as we have overridden SetEngine that tries to
  // assign the new engine value to the registered types.
  FreeAndNil(fClassRegister);
  FreeAndNil(fHelperClassRegister);
  FreeAndNil(fEventHandlerList);
  FreeAndNil(fExposedMembers);
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
  fExposedMembers.Clear;
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
  if Assigned(FModule) then
  begin
    CreateModuleFunctions;
    if Module.Initialized then
      CreateModuleVars
    else
      Module.AddClient( Self );
  end;
end;

procedure TPyDelphiWrapper.ModuleReady(Sender : TObject);
begin
  inherited;
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

function TPyDelphiWrapper.RegisterDelphiWrapper(
  AWrapperClass: TPyDelphiObjectClass): TPythonType;
var
  RegisteredClass : TRegisteredClass;
begin
  Assert(Assigned(AWrapperClass));

  RegisteredClass := TRegisteredClass.Create;
  RegisteredClass.DelphiClass := AWrapperClass.DelphiObjectClass;
  RegisteredClass.PythonType := TPythonType.Create(Self);
  RegisteredClass.PythonType.Engine := Engine;
  RegisteredClass.PythonType.Module := fModule;
  RegisteredClass.PythonType.PyObjectClass := AWrapperClass;
  fClassRegister.Add(RegisteredClass);
  Result := RegisteredClass.PythonType;

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
      begin
        CreateModuleFunctions;
        if FModule.Initialized then
          CreateModuleVars
        else
          FModule.AddClient(Self);
      end;
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

function TPyDelphiWrapper.WrapRecord(const AValue: TValue): PPyObject;
var
  LPythonType: TPythonType;
begin
  CheckEngine();

  if AValue.IsEmpty then begin
    Result := Engine.ReturnNone;
    Exit;
  end;

  LPythonType := GetHelperType('PascalRecordType');
  if not Assigned(LPythonType) or not (AValue.Kind in [tkRecord{$IFDEF MANAGED_RECORD}, tkMRecord{$ENDIF}]) then
  begin
    Result := Engine.ReturnNone;
    Exit;
  end;

  Result := LPythonType.CreateInstance();
  with PythonToDelphi(Result) as TPyPascalRecord do begin
    SetupFromTValue(AValue);
    PyDelphiWrapper := Self;
  end;
end;

function TPyDelphiWrapper.WrapInterface(const IValue: TValue): PPyObject;
var
  LPythonType: TPythonType;
begin
  CheckEngine;

  if IValue.IsEmpty then begin
    Result := Engine.ReturnNone;
    Exit;
  end;

  LPythonType := GetHelperType('PascalInterfaceType');
  if not Assigned(LPythonType) or (IValue.Kind <> tkInterface) then
  begin
    Result := Engine.ReturnNone;
    Exit;
  end;

  Result := LPythonType.CreateInstance;
  with PythonToDelphi(Result) as TPyPascalInterface do begin
    SetupFromTValue(IValue);
    PyDelphiWrapper := Self;
  end;
end;

procedure TPyDelphiWrapper.DefineVar(const AName: string; AValue: TValue);
var
  _obj : PPyObject;
  ErrMsg: string;
begin
  Assert(Assigned(Module));
  _obj := TValueToPyObject(AValue, Self, ErrMsg);
  if Assigned(_obj) then
  begin
    Module.SetVar(AnsiString(AName), _obj);
    Engine.Py_DECREF(_obj);
  end
  else
    raise Exception.Create(ErrMsg);
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
