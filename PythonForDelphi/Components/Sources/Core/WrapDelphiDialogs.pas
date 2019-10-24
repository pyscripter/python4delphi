unit WrapDelphiDialogs;
{$I Definition.Inc}

interface

uses
  Classes, SysUtils, PythonEngine, WrapDelphi, WrapDelphiClasses,
  WrapDelphiControls, Windows, Dialogs, TypInfo;

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

implementation

uses
  WrapDelphiTypes;

{ Register the wrappers, the globals and the constants }
type
  TDialogRegistration = class(TRegisteredUnit)
  public
    function Name: String; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

  { TDialogRegistration }
procedure TDialogRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TDialogRegistration.Name: String;
begin
  Result := 'Dialog';
end;

procedure TDialogRegistration.RegisterWrappers(APyDelphiWrapper
  : TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiOpenDialog);
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
  inherited;
  PythonType.AddGetSet('FileName', @TPyDelphiOpenDialog.Get_filename,
    nil, '', nil);
end;

class procedure TPyDelphiOpenDialog.RegisterMethods(PythonType: TPythonType);
begin
  inherited;
  PythonType.AddMethod('Execute', @TPyDelphiOpenDialog.Execute_Wrapper,
    'TOpenDialog.Execute()'#10 +
    'Displays the dialog');
end;

procedure TPyDelphiOpenDialog.SetDelphiObject(const Value: TOpenDialog);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TDialogRegistration.Create);
end.
