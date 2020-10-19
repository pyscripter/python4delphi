unit PythonAction;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ActnList, PythonEngine;

type
	TPythonAction = class(TAction)
	private
		fRegisteredMethods: TList;
		fPythonModule: TPythonModule;
    fClearname: string;
    fRegistername: string;
    fUnregistername: string;

    procedure ClearMethods;
	protected
		function PythonClear(pself, args: PPyObject): PPyObject; cdecl;
		function PythonRegister(pself, args: PPyObject): PPyObject; cdecl;
		function PythonUnregister(pself, args: PPyObject): PPyObject; cdecl;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function HandlesTarget(Target: TObject): Boolean; override;
		function Execute: Boolean; override;
		procedure UpdateTarget(Target: TObject); override;
		procedure InitializeAction;
		property RegisteredMethods: TList read fRegisteredMethods;
	published
		property PythonModule: TPythonModule read fPythonModule write fPythonModule;
	end;

procedure Register;

implementation

procedure Register;
begin
	RegisterActions('Python', [TPythonAction], TComponent);
end;

{ TPythonAction }

procedure TPythonAction.ClearMethods;
var
	i: integer;
begin
  if PythonOK then
  with GetPythonEngine, RegisteredMethods do
    begin
      for i:=0 to Count-1 do
        Py_XDECREF(PPyObject(Items[i]));
      Clear;
    end;
end;

constructor TPythonAction.Create(AOwner: TComponent);
begin
  inherited;
	fRegisteredMethods := TList.Create;
end;

destructor TPythonAction.Destroy;
begin
  ClearMethods;
	fRegisteredMethods.Free;
	inherited;
end;

function TPythonAction.Execute: Boolean;
var
	i: integer;
  func: PPyObject;
begin
	Result := inherited Execute;
	with GetPythonEngine, RegisteredMethods do
	begin
    i:=0;
    while i < Count do
		begin
      func := Items[i];
      // The function could be deleted by reimport of the Python module
      // or other reasons!
      try
        EvalFunctionNoArgs(func);
        Inc(i);
      except
        Py_XDECREF(func);
        Delete(i);
      end;
		end;
	end;
end;

function TPythonAction.HandlesTarget(Target: TObject): Boolean;
begin
	Result := True;
end;

procedure TPythonAction.InitializeAction;
var
        docString:  string;
begin
	if not (csDesigning in ComponentState) and Assigned(PythonModule) then
	begin
		fClearname := 'Clear' + Name;
		docString := 'Claer all events of "' + Owner.Name + '.' + Name + '" action';
		PythonModule.AddDelphiMethod(PChar(fClearname), PythonClear, PChar(docString));

		fRegistername := 'Register' + Name;
		docString := 'Register an event againt the "' + Owner.Name + '.' + Name + '" action';
		PythonModule.AddDelphiMethod(PChar(fRegistername), PythonRegister, PChar(docString));

		fUnregistername := 'Unregister' + Name;
		docString := 'Unregister an event againt the "' + Owner.Name + '.' + Name + '" action';
		PythonModule.AddDelphiMethod(PChar(fUnregistername), PythonUnregister, PChar(docString));
	end;
end;

function TPythonAction.PythonClear(pself, args: PPyObject): PPyObject;
begin
  ClearMethods;
  with GetPythonEngine, RegisteredMethods do
    begin
      Result := PyLong_FromLong(Count);
    end;
end;

function TPythonAction.PythonRegister(pself, args: PPyObject): PPyObject;
var
	func: PPyObject;
        s: string;
begin
  Result := nil;
  with GetPythonEngine do
    if (PyArg_ParseTuple( args, 'O',@func) = 0) or
       ( not PyFunction_Check(func)) then
    begin
      s := fRegistername + '(function)';
      PyErr_SetString(PyExc_TypeError^,PChar(s));
    end
    else
      with RegisteredMethods do
        begin
          Py_XINCREF(func);
          Add(func);
          Result := PyLong_FromLong(Count);
        end;
end;

function TPythonAction.PythonUnregister(pself, args: PPyObject): PPyObject;
var
	func: PPyObject;
        s: string;
begin
  Result := nil;
  with GetPythonEngine do
    if (PyArg_ParseTuple( args, 'O',@func) = 0) or
       (RegisteredMethods.IndexOf(func) = -1) then
    begin
      s := fUnregistername + '(function)';
      PyErr_SetString(PyExc_TypeError^,PChar(s));
    end
    else
      with RegisteredMethods do
        begin
          Py_XDECREF(func);
          Remove(func);
          Result := PyLong_FromLong(Count);
        end;
end;

procedure TPythonAction.UpdateTarget(Target: TObject);
begin
	Enabled := (RegisteredMethods.Count > 0) or Assigned(OnExecute);
end;

end.
