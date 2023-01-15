{$I ..\Definition.Inc}

unit WrapVclDialogs;

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses,
  WrapVclControls, Windows, Dialogs, TypInfo, Winapi.ActiveX;

type
  TPyDelphiOpenDialog = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TOpenDialog;
    procedure SetDelphiObject(const Value: TOpenDialog);
  protected
    // Exposed Methods
    function Execute_Wrapper(args: PPyObject): PPyObject; cdecl;
    // Property Getters
    function Get_filename(AContext: Pointer): PPyObject; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TOpenDialog read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiFileOpenDialog = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TFileOpenDialog;
    procedure SetDelphiObject(const Value: TFileOpenDialog);
  protected
    // Exposed Methods
    function Execute_Wrapper(args: PPyObject): PPyObject; cdecl;
    // Property Getters
    function Get_filename(AContext: Pointer): PPyObject; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods( PythonType : TPythonType ); override;
    // Properties
    property DelphiObject: TFileOpenDialog read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiSaveDialog = class(TPyDelphiOpenDialog)
  private
    function GetDelphiObject: TSaveDialog;
    procedure SetDelphiObject(const Value: TSaveDialog);
  public
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TSaveDialog read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

uses
  WrapDelphiTypes;

{ Global Functions }
function ShowMessage_Wrapper(pself, args: PPyObject): PPyObject; cdecl;
var
  LMsg: PAnsiChar;
begin
  with GetPythonEngine do
  begin
    if PyArg_ParseTuple(args, 's:ShowMessage', @LMsg) <> 0 then
    begin
      ShowMessage(string(LMsg));
      Result := GetPythonEngine.ReturnNone;
    end else
      Result := nil;
  end;
end;

{ Register the wrappers, the globals and the constants }
type
  TDialogRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

  { TDialogRegistration }
procedure TDialogRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterFunction(PAnsiChar('ShowMessage'), ShowMessage_Wrapper,
    PAnsiChar('ShowMessage_Wrapper()'#10 +
    'Show a custom message as a dialog box.'));
end;

procedure TDialogRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TDialogRegistration.Name: string;
begin
  Result := 'Dialog';
end;

procedure TDialogRegistration.RegisterWrappers(APyDelphiWrapper
  : TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiOpenDialog);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFileOpenDialog);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSaveDialog);
end;

{ TPyDelphiOpenDialog }

class function TPyDelphiOpenDialog.DelphiObjectClass: TClass;
begin
  Result := TOpenDialog;
end;

function TPyDelphiOpenDialog.GetDelphiObject: TOpenDialog;
begin
  Result := TOpenDialog(inherited DelphiObject);
end;

function TPyDelphiOpenDialog.Execute_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Execute') <> 0 then
      Result := VariantAsPyObject(DelphiObject.Execute())
    else
      Result := nil;
  end;
end;

function TPyDelphiOpenDialog.Get_filename(AContext: Pointer): PPyObject;
begin
  Adjust(@self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.FileName);
end;

class procedure TPyDelphiOpenDialog.RegisterGetSets(PythonType: TPythonType);
begin
  PythonType.AddGetSet('FileName', @TPyDelphiOpenDialog.Get_filename,
    nil, '', nil);
end;

class procedure TPyDelphiOpenDialog.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('Execute', @TPyDelphiOpenDialog.Execute_Wrapper,
    'TOpenDialog.Execute()'#10 +
    'Displays the dialog');
end;

procedure TPyDelphiOpenDialog.SetDelphiObject(const Value: TOpenDialog);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFileOpenDialog }

class function TPyDelphiFileOpenDialog.DelphiObjectClass: TClass;
begin
  Result := TFileOpenDialog;
end;

function TPyDelphiFileOpenDialog.Execute_Wrapper(args: PPyObject): PPyObject;
begin
  // We adjust the transmitted self argument
  Adjust(@Self);
  with GetPythonEngine do begin
    if PyArg_ParseTuple( args, ':Execute') <> 0 then
      Result := VariantAsPyObject(DelphiObject.Execute())
    else
      Result := nil;
  end;
end;

function TPyDelphiFileOpenDialog.GetDelphiObject: TFileOpenDialog;
begin
  Result := TFileOpenDialog(inherited DelphiObject);
end;

function TPyDelphiFileOpenDialog.Get_filename(AContext: Pointer): PPyObject;
begin
  Adjust(@self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.FileName);
end;

class procedure TPyDelphiFileOpenDialog.RegisterGetSets(
  PythonType: TPythonType);
begin
  PythonType.AddGetSet('FileName', @TPyDelphiFileOpenDialog.Get_filename,
    nil, '', nil);
end;

class procedure TPyDelphiFileOpenDialog.RegisterMethods(
  PythonType: TPythonType);
begin
  PythonType.AddMethod('Execute', @TPyDelphiFileOpenDialog.Execute_Wrapper,
    'TFileOpenDialog.Execute()'#10 +
    'Displays the dialog');
end;

procedure TPyDelphiFileOpenDialog.SetDelphiObject(const Value: TFileOpenDialog);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiSaveDialog }

class function TPyDelphiSaveDialog.DelphiObjectClass: TClass;
begin
  Result := TSaveDialog;
end;

function TPyDelphiSaveDialog.GetDelphiObject: TSaveDialog;
begin
  Result := TSaveDialog(inherited DelphiObject);
end;

procedure TPyDelphiSaveDialog.SetDelphiObject(const Value: TSaveDialog);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TDialogRegistration.Create);
  CoInitialize(nil);

finalization
  CoUninitialize();

end.
