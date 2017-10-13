unit Unit1;

{.$I Definition.Inc}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  PythonEngine, PythonGUIInputOutput, WrapDelphi, ActnList;

type
  TForm1 = class(TForm)
    Splitter1: TSplitter;
    Memo1: TMemo;
    PyEngine: TPythonEngine;
    PythonModule: TPythonModule;
    Panel1: TPanel;
    Button1: TButton;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    ActionList1: TActionList;
    actTest: TAction;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PythonModuleEvents0Execute(Sender: TObject; PSelf,
      Args: PPyObject; var Result: PPyObject);
    procedure actTestExecute(Sender: TObject);
  private
  public
    PyDelphiWrapper : TPyDelphiWrapper;
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

Uses
  TypInfo,
  Variants,
  VarPyth;


//////////////////////////////////////////////////////////////////////////
// Using TPyDelphiObject you can wrap any Delphi object exposing published
// properties and methods.  Note that the conditional defines TYPEINFO and
// METHODINFO need to be on
//////////////////////////////////////////////////////////////////////////

{$TYPEINFO OFF}
{$IFNDEF FPC}{$METHODINFO OFF}{$ENDIF}
Type
TTestBase = class
  fdouble : double;
  function DoubleDValue : double;   // not visible in subclasses
published
  property DValue : Double read fdouble write fdouble;  // will be visible in subclasses
end;

function TTestBase.DoubleDValue : double;
begin
  Result := 2 * fdouble;
end;

type
{$TYPEINFO ON}
{$IFNDEF FPC}{$METHODINFO ON}{$ENDIF}
TTestClass = class(TTestBase, IFreeNotification)
private
  fSValue : string;
  fIValue : integer;
  fSL : TStrings;
  fOnChange: TNotifyEvent;
  fFreeNotifImpl : IFreeNotification;
  // implementation of interface IUnknown
  function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  function _AddRef: Integer; stdcall;
  function _Release: Integer; stdcall;
protected
  property FreeNotifImpl : IFreeNotification read fFreeNotifImpl implements IFreeNotification;
public
  constructor Create;
  destructor Destroy; override;
  procedure SetMeUp(S: string; I : Integer);
  function DescribeMe(): string;
  procedure TriggerChange;
published
  property SL : TStrings read fSL;
  property SValue : string read fSValue write fSValue;
  property IValue : integer read fIValue write fIValue;
  property OnChange : TNotifyEvent read fOnChange write fOnChange;
end;
{$TYPEINFO OFF}
{$IFNDEF FPC}{$METHODINFO OFF}{$ENDIF}

constructor TTestClass.Create;
begin
  inherited;
  fFreeNotifImpl := TFreeNotificationImpl.Create(Self);
  fSL := TStringList.Create;
  fSL.AddObject('Form1', Form1);
  fSL.AddObject('Form1.Button1', Form1.Button1);
end;

destructor TTestClass.Destroy;
begin
  fSL.Free;
  inherited;
end;

procedure TTestClass.SetMeUp(S: string; I : Integer);
begin
  SValue := S;
  IValue := I;
end;

function TTestClass.Describeme() : string;
begin
  Result := fSValue + ' : ' + IntToStr(IValue);
end;

procedure TTestClass.TriggerChange;
begin
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TTestClass._AddRef: Integer;
begin
  Result := -1; // we don't want reference counting
end;

function TTestClass._Release: Integer;
begin
  Result := -1; // we don't want reference counting
end;

function TTestClass.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  p : PPyObject;
begin
  actTest.OnExecute := actTestExecute;

  // Wrap the Form itself.
  p := PyDelphiWrapper.Wrap(Self);
  PythonModule.SetVar( 'MainForm', p );
  PyEngine.Py_DecRef(p);

  //  Now wrap the an instance our TestClass
  //  This time we would like the object to be destroyed when the PyObject
  //  is destroyed, so we need to set its Owned property to True;
  p := PyDelphiWrapper.Wrap(TTestClass.Create, soOwned);
  PythonModule.SetVar( 'DVar', p );
  PyEngine.Py_DecRef(p);

  {$IF DEFINED(FPC_VER) and (FPC_VER >= 5)}
  p := PyEngine.PyInt_FromLong(FPC_VER);
  {ELSE}
  p := PyEngine.PyInt_FromLong(4);
  {$IFEND}
  PythonModule.SetVar( 'DelphiVersion', p );
  PyEngine.Py_DecRef(p);

  // Excecute the script
  PyEngine.ExecStrings( memo1.Lines );
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  PyDelphiWrapper := TPyDelphiWrapper.Create(Self);  // no need to destroy
  PyDelphiWrapper.Engine := PyEngine;
  PyDelphiWrapper.Module := PythonModule;
  PyDelphiWrapper.Initialize;  // Should only be called if PyDelphiWrapper is created at run time
end;

procedure TForm1.PythonModuleEvents0Execute(Sender: TObject; PSelf,
  Args: PPyObject; var Result: PPyObject);
begin
  ShowMessage(VarPythonCreate(Args).GetItem(0));
  Result :=  PyEngine.ReturnNone;
end;

procedure TForm1.actTestExecute(Sender: TObject);
begin
  Memo2.Lines.Add('Delphi event actTestExecute fired');
  Import('spam').DVar.IValue := 1;
end;

end.
