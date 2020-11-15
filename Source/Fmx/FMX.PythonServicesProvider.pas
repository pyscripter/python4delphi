unit FMX.PythonServicesProvider;

interface

uses
  Classes;

type
  TPythonServicesProvider = class(TComponent)
  public
    class procedure RegisterServices();
    class procedure UnRegisterServices();
  end;

implementation

uses
  System.SysUtils, System.UITypes, FMX.Platform, FMX.DialogService, ServicesProvider;

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
  var LLastErrorCode := System.GetLastError();
  TDialogService.ShowMessage(Format('Could not open Dll "%s"'
                                  + #13#10
                                  + #13#10
                                  + 'Error (%d) - %s', [
                                    ADllName, LLastErrorCode,
                                    SysErrorMessage(LLastErrorCode)]));
end;

procedure TPythonEngineServices.QuitApplication(const AErrorMessage: string);
begin
  TDialogService.MessageDialog(AErrorMessage,
    TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1,
    procedure(const AResult: TModalResult) begin
      Halt(1);
    end);
end;

initialization
  TPythonServicesProvider.RegisterServices();

finalization
  TPythonServicesProvider.UnRegisterServices();

end.
