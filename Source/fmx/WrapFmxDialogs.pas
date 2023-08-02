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

unit WrapFmxDialogs;

interface

uses
  FMX.Dialogs, FMX.DialogService,
  WrapDelphi, WrapFmxTypes, PythonEngine;


type
  TPyDelphiOpenDialog = class(TPyDelphiFmxObject)
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

  TPyDelphiSaveDialog = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TSaveDialog;
    procedure SetDelphiObject(const Value: TSaveDialog);
  protected
    // Exposed Methods
    function Execute_Wrapper(args: PPyObject): PPyObject; cdecl;
    // Property Getters
    function Get_filename(AContext: Pointer): PPyObject; cdecl;
  public
    class function DelphiObjectClass: TClass; override;
    class procedure RegisterGetSets(PythonType: TPythonType); override;
    class procedure RegisterMethods(PythonType: TPythonType); override;
    // Properties
    property DelphiObject: TSaveDialog read GetDelphiObject
      write SetDelphiObject;
  end;

  TPyDelphiDialogService = class(TPyDelphiObject)
  private
    function GetDelphiObject: TDialogService;
    procedure SetDelphiObject(const Value: TDialogService);
  public
    class function DelphiObjectClass: TClass; override;
  public
    property DelphiObject: TDialogService read GetDelphiObject
      write SetDelphiObject;
  end;

implementation

{ Register the wrappers, the globals and the constants }
type
  TDialogRegistration = class(TRegisteredUnit)
  public
    function Name: string; override;
    procedure RegisterWrappers(APyDelphiWrapper: TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper: TPyDelphiWrapper); override;
  end;

{ TDialogRegistration }

procedure TDialogRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

function TDialogRegistration.Name: string;
begin
  Result := 'Dialog';
end;

procedure TDialogRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiOpenDialog);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiSaveDialog);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiDialogService);
end;

{ TPyDelphiOpenDialog }

class function TPyDelphiOpenDialog.DelphiObjectClass: TClass;
begin
  Result := TOpenDialog;
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

function TPyDelphiOpenDialog.GetDelphiObject: TOpenDialog;
begin
  Result := TOpenDialog(inherited DelphiObject);
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

{ TPyDelphiSaveDialog }

class function TPyDelphiSaveDialog.DelphiObjectClass: TClass;
begin
  Result := TSaveDialog;
end;

function TPyDelphiSaveDialog.Execute_Wrapper(args: PPyObject): PPyObject;
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

function TPyDelphiSaveDialog.GetDelphiObject: TSaveDialog;
begin
  Result := TSaveDialog(inherited DelphiObject);
end;

function TPyDelphiSaveDialog.Get_filename(AContext: Pointer): PPyObject;
begin
  Adjust(@self);
  Result := GetPythonEngine.VariantAsPyObject(DelphiObject.FileName);
end;

class procedure TPyDelphiSaveDialog.RegisterGetSets(PythonType: TPythonType);
begin
  PythonType.AddGetSet('FileName', @TPyDelphiOpenDialog.Get_filename,
    nil, '', nil);
end;

class procedure TPyDelphiSaveDialog.RegisterMethods(PythonType: TPythonType);
begin
  PythonType.AddMethod('Execute', @TPyDelphiOpenDialog.Execute_Wrapper,
    'TOpenDialog.Execute()'#10 +
    'Displays the dialog');
end;

procedure TPyDelphiSaveDialog.SetDelphiObject(const Value: TSaveDialog);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiDialogService }

class function TPyDelphiDialogService.DelphiObjectClass: TClass;
begin
  Result := TDialogService;
end;

function TPyDelphiDialogService.GetDelphiObject: TDialogService;
begin
  Result := TDialogService(inherited DelphiObject);
end;

procedure TPyDelphiDialogService.SetDelphiObject(const Value: TDialogService);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TDialogRegistration.Create);

end.
