unit ServicesProvider;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  IPythonEngineServices = interface
    ['{84E9AC99-96F0-4FE2-87A9-6477CB69435F}']
    procedure InvalidDllFatalError(const ADllName: string);
    procedure QuitApplication(const AErrorMessage: string);
  end;

  TServicesProvider = class
  private
    class var FInstance: TServicesProvider;
    class procedure DestroyInstance();
    class function GetInstance: TServicesProvider; static;
  private
    FLoaded: boolean;
    FServices: TDictionary<TGUID, IInterface>;
    procedure CheckLoaded;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure AddService(const AType: TGUID; const AService: IInterface);
    procedure RemoveService(const AType: TGUID);
    function SupportsService(const AType: TGUID; out AService): boolean;

    property Loaded: boolean read FLoaded write FLoaded;
    class property Instance: TServicesProvider read GetInstance;
  end;

  EPlatformServicesNotLoaded = class(Exception)
  end;

implementation

{ TPlatformServices }

constructor TServicesProvider.Create;
begin
  FServices := TDictionary<TGUID, IInterface>.Create();
end;

destructor TServicesProvider.Destroy;
begin
  FServices.Free();
  inherited;
end;

class function TServicesProvider.GetInstance: TServicesProvider;
begin
  if not Assigned(FInstance) then
    FInstance := TServicesProvider.Create();
  Result := FInstance;
end;

class procedure TServicesProvider.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TServicesProvider.AddService(const AType: TGUID;
  const AService: IInterface);
begin
  RemoveService(AType);
  FServices.Add(AType, AService);
end;

procedure TServicesProvider.RemoveService(const AType: TGUID);
begin
  if FServices.ContainsKey(AType) then
    FServices.Remove(AType);
end;

function TServicesProvider.SupportsService(const AType: TGUID;
  out AService): boolean;
begin
  CheckLoaded();
  Pointer(AService) := nil;
  Result := FServices.ContainsKey(AType);
  if Result then
    Result := Supports(FServices.Items[AType], AType, AService);
end;

procedure TServicesProvider.CheckLoaded;
begin
  if not FLoaded then
    raise EPlatformServicesNotLoaded.Create('Drop a TPythonServicesProvider component to project');
end;

initialization
finalization
  TServicesProvider.DestroyInstance();

end.
