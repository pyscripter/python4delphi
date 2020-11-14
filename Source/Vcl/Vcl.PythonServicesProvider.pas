unit Vcl.PythonServicesProvider;

interface

uses
  System.Classes;

type
  TPythonServicesProvider = class(TComponent)
  private
    procedure RegisterServices();
    procedure UnRegisterServices();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

implementation

uses
  ServicesProvider
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  , System.SysUtils;

type
  TPythonEngineServices = class(TInterfacedObject, IPythonEngineServices)
  public
    destructor Destroy(); override;

    procedure InvalidDllFatalError(const ADllName: string);
    procedure QuitApplication(const AErrorMessage: string);
  end;

{ TServicesProvider }

constructor TPythonServicesProvider.Create(AOwner: TComponent);
begin
  inherited;
  TServicesProvider.Instance.Loaded := true;
  RegisterServices();
end;

destructor TPythonServicesProvider.Destroy;
begin
  UnRegisterServices();
  TServicesProvider.Instance.Loaded := false;
  inherited;
end;

procedure TPythonServicesProvider.RegisterServices();
begin
  TServicesProvider.Instance.AddService(IPythonEngineServices, TPythonEngineServices.Create());
end;

procedure TPythonServicesProvider.UnRegisterServices();
begin
  TServicesProvider.Instance.RemoveService(IPythonEngineServices);
end;

{ TPythonEngineServices }

destructor TPythonEngineServices.Destroy;
begin

  inherited;
end;

procedure TPythonEngineServices.InvalidDllFatalError(
  const ADllName: string);
begin
  {$IFDEF MSWINDOWS}
    MessageBox( GetActiveWindow, PChar(Format('Error %d: Could not open Dll "%s"',[GetLastError, ADllName])), 'Error', MB_TASKMODAL or MB_ICONSTOP );
  {$ELSE}
    WriteLn(ErrOutput, Format('Error: Could not open Dll "%s"',[ADllName]);
  {$ENDIF}
end;

procedure TPythonEngineServices.QuitApplication(const AErrorMessage: string);
begin
  {$IFDEF MSWINDOWS}
    MessageBox(GetActiveWindow, PChar(AErrorMessage), 'Error', MB_TASKMODAL or MB_ICONSTOP);
    ExitProcess(1);
  {$ELSE}
    WriteLn(ErrOutput, GetQuitMessage);
    Halt( 1 );
  {$ENDIF}
end;

end.
