unit Vcl.PythonServicesProvider;

interface

uses
  System.Classes;

type
  TPythonServicesProvider = class(TComponent)
  public
    class procedure RegisterServices();
    class procedure UnRegisterServices();
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
    procedure InvalidDllFatalError(const ADllName: string);
    procedure QuitApplication(const AErrorMessage: string);
  end;

{ TServicesProvider }

class procedure TPythonServicesProvider.RegisterServices();
begin
  if TServicesProvider.Instance.Loaded then
    Exit;
  TServicesProvider.Instance.Loaded := true;
  TServicesProvider.Instance.AddService(IPythonEngineServices, TPythonEngineServices.Create());
end;

class procedure TPythonServicesProvider.UnRegisterServices();
begin
  TServicesProvider.Instance.RemoveService(IPythonEngineServices);
  TServicesProvider.Instance.Loaded := false;
end;

{ TPythonEngineServices }

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

initialization
  TPythonServicesProvider.RegisterServices();

finalization
  TPythonServicesProvider.UnRegisterServices();

end.
