unit FMX.PythonServicesProvider;

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
  System.SysUtils, System.UITypes, FMX.DialogService, ServicesProvider, FMX.Platform;

type
  TPythonEngineServices = class(TInterfacedObject, IPythonEngineServices)
  public
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

procedure TPythonEngineServices.InvalidDllFatalError(
  const ADllName: string);
begin
  var LLastErrorCode := System.GetLastError();
  TDialogService.ShowMessage(Format('Could not open Dll "%s"'
                                  + #13#10
                                  + #13#10
                                  + 'Error (%d) - %s',[ADllName, LLastErrorCode, SysErrorMessage(LLastErrorCode)]));
end;

procedure TPythonEngineServices.QuitApplication(const AErrorMessage: string);
begin
  TDialogService.MessageDialog(AErrorMessage,
    TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, -1,
    procedure(const AResult: TModalResult) begin
      Halt(1);
    end);
end;

end.
