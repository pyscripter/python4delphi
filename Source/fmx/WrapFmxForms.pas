{$I ..\Definition.Inc}

unit WrapFmxForms;

interface

uses
  System.Classes, System.SysUtils, FMX.Forms,
  PythonEngine, WrapFmxTypes, WrapDelphiClasses, WrapFmxControls;

type
  TPyDelphiApplication = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TApplication;
    procedure SetDelphiObject(const Value: TApplication);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TApplication read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCommonCustomForm = class(TPyDelphiFmxObject)
  private
    function GetDelphiObject: TCommonCustomForm;
    procedure SetDelphiObject(const Value: TCommonCustomForm);
    function HasFormRes(const AClass: TClass): boolean;
  public
    function CreateComponent(AOwner: TComponent): TComponent; override;
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCommonCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomForm = class(TPyDelphiCommonCustomForm)
  private
    function GetDelphiObject: TCustomForm;
    procedure SetDelphiObject(const Value: TCustomForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiCustomPopupForm = class(TPyDelphiCustomForm)
  private
    function GetDelphiObject: TCustomPopupForm;
    procedure SetDelphiObject(const Value: TCustomPopupForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TCustomPopupForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiForm = class(TPyDelphiCustomForm)
  private
    function GetDelphiObject: TForm;
    procedure SetDelphiObject(const Value: TForm);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TForm read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiFrame = class(TPyDelphiControl)
  private
    function GetDelphiObject: TFrame;
    procedure SetDelphiObject(const Value: TFrame);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TFrame read GetDelphiObject write SetDelphiObject;
  end;

  TPyDelphiScreen = class(TPyDelphiComponent)
  private
    function GetDelphiObject: TScreen;
    procedure SetDelphiObject(const Value: TScreen);
  public
    // Class methods
    class function DelphiObjectClass: TClass; override;
    // Properties
    property DelphiObject: TScreen read GetDelphiObject write SetDelphiObject;
  end;

  EInvalidFormClass = class(Exception);

implementation

uses
  WrapDelphi, System.Types;

{ Register the wrappers, the globals and the constants }
type
  TFormsRegistration = class(TRegisteredUnit)
  public
    function Name : string; override;
    procedure RegisterWrappers(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineVars(APyDelphiWrapper : TPyDelphiWrapper); override;
    procedure DefineFunctions(APyDelphiWrapper : TPyDelphiWrapper); override;
  end;

{ TFormsRegistration }

procedure TFormsRegistration.DefineFunctions(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
end;

procedure TFormsRegistration.DefineVars(APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  // Singletons
  APyDelphiWrapper.DefineVar('Application', Application);
  APyDelphiWrapper.DefineVar('Screen', Screen);
end;

function TFormsRegistration.Name: string;
begin
  Result := 'Forms';
end;

procedure TFormsRegistration.RegisterWrappers(
  APyDelphiWrapper: TPyDelphiWrapper);
begin
  inherited;
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiApplication);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCommonCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiCustomPopupForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiForm);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiFrame);
  APyDelphiWrapper.RegisterDelphiWrapper(TPyDelphiScreen);
end;

{ TPyDelphiApplication }

class function TPyDelphiApplication.DelphiObjectClass: TClass;
begin
  Result := TApplication;
end;

function TPyDelphiApplication.GetDelphiObject: TApplication;
begin
  Result := TApplication(inherited DelphiObject);
end;

procedure TPyDelphiApplication.SetDelphiObject(const Value: TApplication);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCommonCustomForm }

function TPyDelphiCommonCustomForm.CreateComponent(
  AOwner: TComponent): TComponent;
type
  TCommonCustomFormClass = class of TCommonCustomForm;
var
  LClass: TClass;
  LFormClass: TCommonCustomFormClass;
  LClassName: string;
begin
  LFormClass := nil;
  //get de default form class
  if DelphiObjectClass.InheritsFrom(TCommonCustomForm) then
    LFormClass := TCommonCustomFormClass(DelphiObjectClass);

  //if we have a subclass of our Form wrapper, then check if we can find a
  //Delphi class that would have the same name as the Python class.
  //This would allow Python to instanciate an existing Delphi form class,
  //insted of only using a blank form.
  if (ob_type <> PythonType.TheTypePtr) then begin
    LClassName := string(ob_type.tp_name);
    LClass := GetClass(LClassName);
    if not Assigned(LClass) then
      LClass := GetClass('T' + LClassName);
    if Assigned(LClass) and LClass.InheritsFrom(TCommonCustomForm) then
      LFormClass := TCommonCustomFormClass(LClass);
  end;

  if not Assigned(LFormClass) then
    raise EInvalidFormClass.CreateFmt('Type %s is not a valid form class', [
      DelphiObjectClass.ClassName]);

  //if it's not a design form, so we create it as a non-resourced form,
  //using the non-resourced constructor.
  //if the Owner is TApplication, then we have to call its CreateForm method,
  //otherwise we won't have a mian form defined, as the main form is the first
  //created form. Of course, this is a concern only when Python controls all the
  //GUI and calls Apllication.Run by itself.
  if not HasFormRes(LFormClass) then
    Result := LFormClass.CreateNew(AOwner)
  else if (AOwner = Application) then
    Application.CreateForm(LFormClass, Result)
  else
    Result := LFormClass.Create(AOwner);
end;

class function TPyDelphiCommonCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCommonCustomForm;
end;

function TPyDelphiCommonCustomForm.GetDelphiObject: TCommonCustomForm;
begin
  Result := TCommonCustomForm(inherited DelphiObject);
end;

function TPyDelphiCommonCustomForm.HasFormRes(const AClass: TClass): boolean;
begin
  Result := FindResource(
    FindResourceHInstance(FindClassHInstance(AClass)),
    PChar(AClass.ClassName), PChar(RT_RCDATA)) <> 0;
end;

procedure TPyDelphiCommonCustomForm.SetDelphiObject(
  const Value: TCommonCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomForm }

class function TPyDelphiCustomForm.DelphiObjectClass: TClass;
begin
  Result := TCustomForm;
end;

function TPyDelphiCustomForm.GetDelphiObject: TCustomForm;
begin
  Result := TCustomForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomForm.SetDelphiObject(const Value: TCustomForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiCustomPopupForm }

class function TPyDelphiCustomPopupForm.DelphiObjectClass: TClass;
begin
  Result := TCustomPopupForm;
end;

function TPyDelphiCustomPopupForm.GetDelphiObject: TCustomPopupForm;
begin
  Result := TCustomPopupForm(inherited DelphiObject);
end;

procedure TPyDelphiCustomPopupForm.SetDelphiObject(
  const Value: TCustomPopupForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiForm }

class function TPyDelphiForm.DelphiObjectClass: TClass;
begin
  Result := TForm;
end;

function TPyDelphiForm.GetDelphiObject: TForm;
begin
  Result := TForm(inherited DelphiObject);
end;

procedure TPyDelphiForm.SetDelphiObject(const Value: TForm);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiFrame }

class function TPyDelphiFrame.DelphiObjectClass: TClass;
begin
  Result := TFrame;
end;

function TPyDelphiFrame.GetDelphiObject: TFrame;
begin
  Result := TFrame(inherited DelphiObject);
end;

procedure TPyDelphiFrame.SetDelphiObject(const Value: TFrame);
begin
  inherited DelphiObject := Value;
end;

{ TPyDelphiScreen }

class function TPyDelphiScreen.DelphiObjectClass: TClass;
begin
  Result := TScreen;
end;

function TPyDelphiScreen.GetDelphiObject: TScreen;
begin
  Result := TScreen(inherited DelphiObject);
end;

procedure TPyDelphiScreen.SetDelphiObject(const Value: TScreen);
begin
  inherited DelphiObject := Value;
end;

initialization
  RegisteredUnits.Add(TFormsRegistration.Create);

end.
