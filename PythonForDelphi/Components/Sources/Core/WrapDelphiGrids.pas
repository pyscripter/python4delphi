{$I Definition.Inc}

unit WrapDelphiGrids;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses,
  WrapDelphiControls, Windows, Grids, TypInfo;

type
  TDrawCellEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; ACol, ARow: Longint;
                      Rect: TRect; State: TGridDrawState);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TSelectCellEventHandler = class(TEventHandler)
  protected
    procedure DoEvent(Sender: TObject; ACol, ARow: Longint; var CanSelect: Boolean);
  public
    constructor Create(PyDelphiWrapper : TPyDelphiWrapper; Component : TObject;
      PropertyInfo : PPropInfo; Callable : PPyObject); override;
    class function GetTypeInfo : PTypeInfo; override;
  end;

  TGridColWidthsAccess = class(TContainerAccess)
  private
    function GetContainer: TCustomDrawGrid;
  public
    function GetItem(AIndex : Integer) : PPyObject; override;
    function GetSize : Integer; override;
    function SetItem(AIndex : Integer; AValue : PPyObject) : Boolean; override;

    class function ExpectedContainerClass : TClass; override;
    class function SupportsWrite : Boolean; override;
    class function Name : String; override;

    property Container : TCustomDrawGrid read GetContainer;
  end;

  TPyDelphiCustomGrid = class (TPyDelphiWinControl)
  private
    function  GetDelphiObject: TCustomGrid;
    procedure SetDelphiObject(const Value: TCustomGrid);
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TCustomGrid read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomDrawGrid = class (TPyDelphiCustomGrid)
  private
    function  GetDelphiObject: TCustomDrawGrid;
    procedure SetDelphiObject(const Value: TCustomDrawGrid);
  protected
    // methods
    //function CellRect(ACol, ARow: Longint): TRect;
    //procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    class function GetTypeName : String; override;

    // property getters
    function Get_Canvas(AContext : Pointer): PPyObject; cdecl;
    function Get_Col(AContext : Pointer): PPyObject; cdecl;
    function Get_ColWidths( AContext : Pointer) : PPyObject; cdecl;
    function Get_EditorMode(AContext : Pointer): PPyObject; cdecl;
    function Get_GridHeight(AContext : Pointer): PPyObject; cdecl;
    function Get_GridWidth(AContext : Pointer): PPyObject; cdecl;
    function Get_LeftCol(AContext : Pointer): PPyObject; cdecl;
    function Get_Selection(AContext : Pointer): PPyObject; cdecl;
    function Get_Row(AContext : Pointer): PPyObject; cdecl;
    //function Get_RowHeights(AContext : Pointer): PPyObject; cdecl;
    //function Get_TabStops(AContext : Pointer): PPyObject; cdecl;
    function Get_TopRow(AContext : Pointer): PPyObject; cdecl;

    // property setters
    function Set_Col(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_EditorMode(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_LeftCol(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Selection(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_Row(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
    function Set_TopRow(AValue : PPyObject; AContext : Pointer): Integer; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;

    // Class methods
    class procedure RegisterGetSets( PythonType : TPythonType ); override;

    // Properties
    property DelphiObject: TCustomDrawGrid read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiDrawGrid = class (TPyDelphiCustomDrawGrid)
  private
    function  GetDelphiObject: TDrawGrid;
    procedure SetDelphiObject(const Value: TDrawGrid);
  protected
    class function GetTypeName : String; override;
  public
    class function  DelphiObjectClass : TClass; override;
    // Properties
    property DelphiObject: TDrawGrid read GetDelphiObject write SetDelphiObject;
  end;

  {$IFDEF FPC}
  TPyDelphiStringGrid = class (TPyDelphiCustomDrawGrid)
  {$ELSE FPC}
  TPyDelphiStringGrid = class (TPyDelphiDrawGrid)
  {$ENDIF FPC}
  private
    function  GetDelphiObject: TStringGrid;
    procedure SetDelphiObject(const Value: TStringGrid);
  protected
    class function GetTypeName : String; override;
    function GetCell(args : PPyObject): PPyObject; cdecl;
    function SetCell(args : PPyObject): PPyObject; cdecl;
  public
    class function  DelphiObjectClass : TClass; override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TStringGrid read GetDelphiObject write SetDelphiObject;
  end;

implementation

uses
  WrapDelphiTypes;

{ Register the wrappers, the globals and the constants }
type
  TGridsRegistration = class(TRegisteredUnit)
  public
    function Name : String; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TGridsRegistration }

procedure TGridsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.DefineVar('gdSelected', 'gdSelected');
  APyDelphiWrapper.DefineVar('gdFocused', 'gdFocused');
  APyDelphiWrapper.DefineVar('gdFixed', 'gdFixed');
end;

function TGridsRegistration.Name: String;
begin
  Result := 'Grids';
end;

procedure TGridsRegistration.RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomGrid);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomDrawGrid);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDrawGrid);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiStringGrid);

  APyDelphiWrapper.EventHandlers.RegisterHandler(TDrawCellEventHandler);
  APyDelphiWrapper.EventHandlers.RegisterHandler(TSelectCellEventHandler);
end;

{ Helper functions }

function MakeGridDrawState(AState : TGridDrawState) : PPyObject;
var
  _item : PPyObject;
begin
  with GetPythonEngine do
  begin
    Result := PyList_New(0);
    if gdSelected in AState then
    begin
      _item := PyString_FromDelphiString('gdSelected');
      PyList_Append(Result, _item);
      Py_DecRef(_item);
    end;
    if gdFocused in AState then
    begin
      _item := PyString_FromDelphiString('gdFocused');
      PyList_Append(Result, _item);
      Py_DecRef(_item);
    end;
    if gdFixed in AState then
    begin
      _item := PyString_FromDelphiString('gdFixed');
      PyList_Append(Result, _item);
      Py_DecRef(_item);
    end;
  end;
end;

{ TDrawCellEventHandler }

constructor TDrawCellEventHandler.Create(PyDelphiWrapper: TPyDelphiWrapper;
  Component: TObject; PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TDrawCellEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TDrawCellEventHandler.DoEvent(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  PyObject, PyTuple, PyResult, PyCol, PyRow, PyRect, PyState: PPyObject;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyCol := PyInt_FromLong(ACol);
      PyRow := PyInt_FromLong(ARow);
      PyRect := WrapRect(PyDelphiWrapper, Rect);
      PyState := MakeGridDrawState(State);

      PyTuple := PyTuple_New(5);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyCol);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 2, PyRow);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 3, PyRect);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 4, PyState);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        Py_XDECREF(PyResult);
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TDrawCellEventHandler.GetTypeInfo: PTypeInfo;
begin
  {$IFDEF FPC}
  Result := System.TypeInfo(TOnDrawCell);
  {$ELSE FPC}
  Result := System.TypeInfo(TDrawCellEvent);
  {$ENDIF FPC}
end;

{ TSelectCellEventHandler }

constructor TSelectCellEventHandler.Create(
  PyDelphiWrapper: TPyDelphiWrapper; Component: TObject;
  PropertyInfo: PPropInfo; Callable: PPyObject);
var
  Method : TMethod;
begin
  inherited;
  Method.Code := @TSelectCellEventHandler.DoEvent;
  Method.Data := Self;
  SetMethodProp(Component, PropertyInfo, Method);
end;

procedure TSelectCellEventHandler.DoEvent(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
Var
  PyObject, PyTuple, PyResult, PyCol, PyRow, PyCanSelect: PPyObject;
  _varParam : TPyDelphiVarParameter;
begin
  Assert(Assigned(PyDelphiWrapper));
  if Assigned(Callable) and PythonOK then
    with GetPythonEngine do begin
      PyObject := PyDelphiWrapper.Wrap(Sender);
      PyCol := PyInt_FromLong(ACol);
      PyRow := PyInt_FromLong(ARow);
      PyCanSelect := CreateVarParam(PyDelphiWrapper, CanSelect);
      _varParam := PythonToDelphi(PyCanSelect) as TPyDelphiVarParameter;

      PyTuple := PyTuple_New(4);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 0, PyObject);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 1, PyCol);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 2, PyRow);
      GetPythonEngine.PyTuple_SetItem(PyTuple, 3, PyCanSelect);
      try
        PyResult := PyObject_CallObject(Callable, PyTuple);
        if Assigned(PyResult) then
        begin
          Py_DECREF(PyResult);
          CanSelect := PyObject_IsTrue(_varParam.Value) = 1;
        end;
      finally
        Py_DECREF(PyTuple);
      end;
      CheckError;
    end;
end;

class function TSelectCellEventHandler.GetTypeInfo: PTypeInfo;
begin
  {$IFDEF FPC}
  Result := System.TypeInfo(TOnSelectCellEvent);
  {$ELSE FPC}
  Result := System.TypeInfo(TSelectCellEvent);
  {$ENDIF FPC}
end;

{ TGridColWidthsAccess }

class function TGridColWidthsAccess.ExpectedContainerClass: TClass;
begin
  result:=TCustomGrid;
end;

function TGridColWidthsAccess.GetContainer: TCustomDrawGrid;
begin
  result:=TCustomDrawGrid(inherited Container);
end;

function TGridColWidthsAccess.GetItem(AIndex: Integer): PPyObject;
begin
  with GetPythonEngine do
    Result:=PyInt_FromLong(Container.ColWidths[AIndex]);
end;

type
  TCustomDrawGridAccess = class(TCustomDrawGrid);

function TGridColWidthsAccess.GetSize: Integer;
begin
  result:=TCustomDrawGridAccess(Container).ColCount;
end;

class function TGridColWidthsAccess.Name: String;
begin
  result:='TCustomGrid.ColWidths';
end;

function TGridColWidthsAccess.SetItem(AIndex: Integer;
  AValue: PPyObject): Boolean;
begin
  result:=True;
  with GetPythonEngine do
    Container.ColWidths[AIndex]:=PyInt_AsLong(AValue);
end;

class function TGridColWidthsAccess.SupportsWrite: Boolean;
begin
  result:=True;
end;

{ TPyDelphiCustomGrid }

class function TPyDelphiCustomGrid.DelphiObjectClass: TClass;
begin
  Result := TCustomGrid;
end;

function TPyDelphiCustomGrid.GetDelphiObject: TCustomGrid;
begin
  Result := TCustomGrid(inherited DelphiObject);
end;

procedure TPyDelphiCustomGrid.SetDelphiObject(const Value: TCustomGrid);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomDrawGrid }

class function TPyDelphiCustomDrawGrid.DelphiObjectClass: TClass;
begin
  Result := TCustomDrawGrid;
end;

function TPyDelphiCustomDrawGrid.GetDelphiObject: TCustomDrawGrid;
begin
  Result := TCustomDrawGrid(inherited DelphiObject);
end;

function TPyDelphiCustomDrawGrid.Get_Canvas(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Wrap(DelphiObject.Canvas);
end;

function TPyDelphiCustomDrawGrid.Get_ColWidths(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := Self.PyDelphiWrapper.DefaultContainerType.CreateInstance;
  with PythonToDelphi(Result) as TPyDelphiContainer do
    Setup(Self.PyDelphiWrapper, TGridColWidthsAccess.Create(Self.PyDelphiWrapper, Self.DelphiObject));
end;

function TPyDelphiCustomDrawGrid.Get_Col(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Col);
end;

function TPyDelphiCustomDrawGrid.Get_EditorMode(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.EditorMode);
end;

function TPyDelphiCustomDrawGrid.Get_GridHeight(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.GridHeight);
end;

function TPyDelphiCustomDrawGrid.Get_GridWidth(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.GridWidth);
end;

function TPyDelphiCustomDrawGrid.Get_LeftCol(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.LeftCol);
end;

function TPyDelphiCustomDrawGrid.Get_Row(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.Row);
end;

function TPyDelphiCustomDrawGrid.Get_Selection(
  AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := WrapRect(PyDelphiWrapper, TRect(DelphiObject.Selection));
end;

function TPyDelphiCustomDrawGrid.Get_TopRow(AContext: Pointer): PPyObject;
begin
  Adjust(@Self);
  Result := GetPythonEngine.PyInt_FromLong(DelphiObject.TopRow);
end;

class function TPyDelphiCustomDrawGrid.GetTypeName : String;
begin
  Result := 'CustomDrawGrid';
end;

class procedure TPyDelphiCustomDrawGrid.RegisterGetSets(
  PythonType: TPythonType);
begin
  inherited;
  with PythonType do
    begin
      AddGetSet('Canvas', @TPyDelphiCustomDrawGrid.Get_Canvas, nil,
        'Specifies the TCanvas object that presents a drawing surface for the control.', nil);
      AddGetSet('Col', @TPyDelphiCustomDrawGrid.Get_Col, @TPyDelphiCustomDrawGrid.Set_Col,
        'Specifies the index of the column that contains the selected cell.', nil);
      AddGetSet('ColWidths', @TPyDelphiCustomDrawGrid.Get_ColWidths, nil,
        'Specifies column widths of the grid', nil);
      AddGetSet('EditorMode', @TPyDelphiCustomDrawGrid.Get_EditorMode, @TPyDelphiCustomDrawGrid.Set_EditorMode,
        'Determines whether the current cell can be edited.', nil);
      AddGetSet('GridHeight', @TPyDelphiCustomDrawGrid.Get_GridHeight, nil,
        'Specifies the height of the grid in pixels.', nil);
      AddGetSet('GridWidth', @TPyDelphiCustomDrawGrid.Get_GridWidth, nil,
        'Specifies the width of the grid in pixels.', nil);
      AddGetSet('LeftCol', @TPyDelphiCustomDrawGrid.Get_LeftCol, @TPyDelphiCustomDrawGrid.Set_LeftCol,
        'Specifies the index of the first visible scrollable column in the grid.', nil);
      AddGetSet('Selection', @TPyDelphiCustomDrawGrid.Get_Selection, @TPyDelphiCustomDrawGrid.Set_Selection,
        'Indicates the boundaries of the current selection.', nil);
      AddGetSet('Row', @TPyDelphiCustomDrawGrid.Get_Row, @TPyDelphiCustomDrawGrid.Set_Row,
        'Specifies the index of the row that contains the selected cell.', nil);
      AddGetSet('TopRow', @TPyDelphiCustomDrawGrid.Get_TopRow, @TPyDelphiCustomDrawGrid.Set_TopRow,
        'Specifies the index of the first visible scrollable row in the grid.', nil);
    end;
end;

procedure TPyDelphiCustomDrawGrid.SetDelphiObject(
  const Value: TCustomDrawGrid);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiCustomDrawGrid.Set_Col(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Col', _value) then
  begin
    DelphiObject.Col := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCustomDrawGrid.Set_EditorMode(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Boolean;
begin
  Adjust(@Self);
  if CheckBoolAttribute(AValue, 'EditorMode', _value) then
  begin
    DelphiObject.EditorMode := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCustomDrawGrid.Set_LeftCol(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'LeftCol', _value) then
  begin
    DelphiObject.LeftCol := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCustomDrawGrid.Set_Row(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'Row', _value) then
  begin
    DelphiObject.Row := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCustomDrawGrid.Set_Selection(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : TRect;
begin
  Adjust(@Self);
  if CheckRectAttribute(AValue, 'Selection', _value) then
  begin
    DelphiObject.Selection := TGridRect(_value);
    Result := 0;
  end
  else
    Result := -1;
end;

function TPyDelphiCustomDrawGrid.Set_TopRow(AValue: PPyObject;
  AContext: Pointer): Integer;
var
  _value : Integer;
begin
  Adjust(@Self);
  if CheckIntAttribute(AValue, 'TopRow', _value) then
  begin
    DelphiObject.TopRow := _value;
    Result := 0;
  end
  else
    Result := -1;
end;

{ TPyDelphiDrawGrid }

class function TPyDelphiDrawGrid.DelphiObjectClass: TClass;
begin
  Result := TDrawGrid;
end;

function TPyDelphiDrawGrid.GetDelphiObject: TDrawGrid;
begin
  Result := TDrawGrid(inherited DelphiObject);
end;

class function TPyDelphiDrawGrid.GetTypeName : String;
begin
  Result := 'DrawGrid';
end;

procedure TPyDelphiDrawGrid.SetDelphiObject(const Value: TDrawGrid);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiStringGrid }

class function TPyDelphiStringGrid.DelphiObjectClass: TClass;
begin
  Result := TStringGrid;
end;

function TPyDelphiStringGrid.GetDelphiObject: TStringGrid;
begin
  Result := TStringGrid(inherited DelphiObject);
end;

class procedure TPyDelphiStringGrid.RegisterMethods( PythonType : TPythonType );
begin
  inherited;
  PythonType.AddMethod('GetCell', @TPyDelphiStringGrid.GetCell,
    'TStringGrid.GetCell(col, row)'#10 +
    'Returns the content of a cell');
  PythonType.AddMethod('SetCell', @TPyDelphiStringGrid.SetCell,
    'TStringGrid.SetCell(col, row, value)'#10 +
    'Sets the content of a cell');
end;

class function TPyDelphiStringGrid.GetTypeName : String;
begin
  Result := 'StringGrid';
end;

procedure TPyDelphiStringGrid.SetDelphiObject(const Value: TStringGrid);
begin
  inherited DelphiObject := Value;
end;

function TPyDelphiStringGrid.GetCell(args: PPyObject): PPyObject;
var
  col, row: integer;
begin
  // adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, 'ii:GetCell',@col, @row ) <> 0 then
      Result := PyString_FromDelphiString(DelphiObject.Cells[col, row])
    else
      Result := nil;
  end;
end;

function TPyDelphiStringGrid.SetCell(args: PPyObject): PPyObject;
var
  col, row: integer;
  value: PPyObject;
begin
  with GetPythonEngine do begin
    // adjust the transmitted self argument
    Adjust(@Self);
    if PyArg_ParseTuple( args, 'iiO:GetCell',@col, @row, @value ) <> 0 then
    begin
      DelphiObject.Cells[col, row]:= PyString_AsDelphiString(value);
      result:=ReturnNone;
    end
    else
      Result := nil;
  end;

end;

initialization
  RegisteredUnits.Add( TGridsRegistration.Create );
  Classes.RegisterClasses([TDrawGrid, TStringGrid]);
end.
