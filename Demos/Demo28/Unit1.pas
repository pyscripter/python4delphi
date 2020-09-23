unit Unit1;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  PythonEngine, PythonGUIInputOutput;

type
  TPyStringList = class(TPyObject)
  private
    fStrings: TStringList;
    procedure SetStrings(const Value: TStringList);
  public
    // Constructors & Destructors
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
    destructor Destroy; override;

    // Basic services
    function  Iter : PPyObject; override;

    // Sequence services
    function  SqLength : Integer; override;
    function  SqItem( idx : Integer ) : PPyObject; override;
    function  SqAssItem( idx : integer; obj : PPyObject) : Integer; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Exposed methods
    function  add(args : PPyObject) : PPyObject; cdecl;

    // properties
    property Strings : TStringList read fStrings write SetStrings;
  end;

  TPyStringListIterator = class(TPyObject)
  private
    fStringList: TPyStringList;
    fCurrentIndex: Integer;
    procedure SetStringList(const Value: TPyStringList);
  public
    constructor Create( APythonType : TPythonType ); override;
    constructor CreateWith( PythonType : TPythonType; args : PPyObject ); override;
    destructor Destroy; override;

    // Basic services
    function  Iter : PPyObject; override;
    function  IterNext : PPyObject; override;

    // Class methods
    class procedure RegisterMethods( PythonType : TPythonType ); override;

    // Exposed methods
    function  next(args : PPyObject) : PPyObject; cdecl;

    // properties
    property StringList : TPyStringList read fStringList write SetStringList;
  end;

  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    ptStringList: TPythonType;
    pmP4D: TPythonModule;
    ptStringListIterator: TPythonType;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ptStringListCreate(Sender: TObject);
    procedure ptStringListIteratorCreate(Sender: TObject);
  private
  public
  end;



var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        Memo1.Lines.LoadFromFile( FileName );
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      if Execute then
        Memo1.Lines.SaveToFile( FileName );
    end;
end;

{ TPyStringList }

function TPyStringList.add(args: PPyObject): PPyObject;
var
  _obj : PPyObject;
begin
  with GetPythonEngine do
  begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, 'O:add',@_obj ) <> 0 then
    begin
      Result := PyInt_FromLong(Strings.Add(PyObjectAsString(_obj)));
    end
    else
      Result := nil;
  end;
end;

constructor TPyStringList.Create(APythonType: TPythonType);
begin
  inherited;
  Assert(not Assigned(fStrings));
  fStrings := TStringList.Create;
end;

constructor TPyStringList.CreateWith(PythonType: TPythonType;
  args: PPyObject);
var
  i : Integer;
begin
  inherited;
  with GetPythonEngine do
  begin
    for i := 0 to PyTuple_Size(args)-1 do
    begin
      Strings.Add(PyObjectAsString(PyTuple_GetItem(args, i)));
    end;
  end;
end;

destructor TPyStringList.Destroy;
begin
  fStrings.Free;
  inherited;
end;

function TPyStringList.Iter: PPyObject;
var
//  _iter : TPyStringListIterator;
  _args : PPyObject;
begin
  _args := GetPythonEngine.MakePyTuple([Self.GetSelf]);
  try
    Result := Form1.ptStringListIterator.CreateInstanceWith(_args);
  finally
    GetPythonEngine.Py_DECREF(_args);
  end;
  {_iter := Form1.ptStringListIterator.CreateInstance as TPyStringListIterator;
  _iter.StringList := Self;
  Result := _iter.GetSelf;}
end;

class procedure TPyStringList.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  with PythonType do
  begin
    AddMethod( 'add', @TPyStringList.add, 'add a new item to the list and returns the index position' );
  end;
end;

procedure TPyStringList.SetStrings(const Value: TStringList);
begin
  fStrings.Assign(Value);
end;

function TPyStringList.SqAssItem(idx: integer; obj: PPyObject): Integer;
begin
  with GetPythonEngine do
  begin
    if idx < Strings.Count then
    begin
      Strings[idx] := PyObjectAsString(obj);
      Result := 0;
    end
    else
    begin
      PyErr_SetString(PyExc_IndexError^, 'list index out of range');
      Result := -1;
    end;
  end;
end;

function TPyStringList.SqItem(idx: Integer): PPyObject;
begin
  with GetPythonEngine do
  begin
    if idx < Strings.Count then
      Result := PyString_FromString(PAnsiChar(AnsiString(Strings[idx])))
    else
    begin
      PyErr_SetString(PyExc_IndexError^, 'list index out of range');
      Result := nil;
    end;
  end;
end;

function TPyStringList.SqLength: Integer;
begin
  Result := Strings.Count;
end;

{ TPyStringListIterator }

constructor TPyStringListIterator.Create(APythonType: TPythonType);
begin
  inherited;
end;

constructor TPyStringListIterator.CreateWith(PythonType: TPythonType;
  args: PPyObject);
var
  _obj : PPyObject;
  _stringList : TPyStringList;
begin
  inherited;
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple( args, 'O:TPyStringListIterator constructor',@_obj ) <> 0 then
    begin
      _stringList := PythonToDelphi(_obj) as TPyStringList;
      StringList := _stringList;
    end;
  end;
end;

destructor TPyStringListIterator.Destroy;
begin
  StringList := nil;
  inherited;
end;

function TPyStringListIterator.Iter: PPyObject;
begin
  Result := Self.GetSelf;
  GetPythonEngine.Py_INCREF(Result);
end;

function TPyStringListIterator.IterNext: PPyObject;
begin
  Inc(fCurrentIndex);
  with GetPythonEngine do
  begin
    if fCurrentIndex >= StringList.Strings.Count then
    begin
      //PyErr_SetString(PyExc_StopIteration^, 'Stop iteration');
      Result := nil;
    end
    else
    begin
      Result := PyString_FromString(PAnsiChar(AnsiString(StringList.Strings[fCurrentIndex])));
    end;
  end;
end;

function TPyStringListIterator.next(args: PPyObject): PPyObject;
begin
  with GetPythonEngine do
  begin
    // We adjust the transmitted self argument
    Adjust(@Self);
    Result := Self.IterNext;
    if not Assigned(Result) and (PyErr_Occurred = nil) then
      PyErr_SetString(PyExc_StopIteration^, 'Stop iteration');
  end;
end;

class procedure TPyStringListIterator.RegisterMethods(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
  begin
    AddMethod( 'next', @TPyStringListIterator.next, 'Returns the next value from the iterable container' );
  end;
end;

procedure TPyStringListIterator.SetStringList(const Value: TPyStringList);
begin
  if fStringList <> Value then
  begin
    if Assigned(fStringList) then
      GetPythonEngine.Py_DECREF(fStringList.GetSelf);
    fStringList := Value;
    if Assigned(fStringList) then
      GetPythonEngine.Py_INCREF(fStringList.GetSelf);
    fCurrentIndex := -1;
  end;
end;

procedure TForm1.ptStringListCreate(Sender: TObject);
begin
  with Sender as TPythonType do
    PyObjectClass := TPyStringList;
end;

procedure TForm1.ptStringListIteratorCreate(Sender: TObject);
begin
  with Sender as TPythonType do
    PyObjectClass := TPyStringListIterator;
end;

end.
