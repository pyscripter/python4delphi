unit PythonPlatformServices;

interface

uses
  System.Generics.Collections, SysUtils, PythonException;

type
  IPythonLoaderService = interface
    ['{C223FB58-E5FC-407F-A061-8E6DABEF1DB7}']
    function IsHandleValid(const AHandle: THandle): boolean;
    function LoadDll(const ADllPath: string; out AHandle: THandle): boolean;
    procedure UnloadDll(var AHandle: THandle);
    function Import(const AHandle: THandle; const AFuncName: AnsiString; ACanFail: boolean = true): pointer;

    procedure FatalMsgDlg(const AMsg: string);
    procedure FatalAbort(const AMsg: string);
  end;

  TPythonPlatformServices = class
  private
    class var FInstance: TPythonPlatformServices;
    class procedure CreateInstance();
    class procedure DestroyInstance();
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
    class property Instance: TPythonPlatformServices read FInstance;
  end;

implementation

{ TPythonPlatformServices }

constructor TPythonPlatformServices.Create;
begin
  FServices := TDictionary<TGUID, IInterface>.Create();
end;

destructor TPythonPlatformServices.Destroy;
begin
  FServices.Free();
  inherited;
end;

class procedure TPythonPlatformServices.CreateInstance;
begin
  FInstance := TPythonPlatformServices.Create();
end;

class procedure TPythonPlatformServices.DestroyInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TPythonPlatformServices.AddService(const AType: TGUID;
  const AService: IInterface);
begin
  RemoveService(AType);
  FServices.Add(AType, AService);
end;

procedure TPythonPlatformServices.RemoveService(const AType: TGUID);
begin
  if FServices.ContainsKey(AType) then
    FServices.Remove(AType);
end;

function TPythonPlatformServices.SupportsService(const AType: TGUID;
  out AService): boolean;
begin
  CheckLoaded();
  Result := FServices.ContainsKey(AType);
  if Result then
    Result := Supports(FServices.Items[AType], AType, AService)
  else
    Pointer(AService) := nil;
end;

procedure TPythonPlatformServices.CheckLoaded;
begin
  if not FLoaded then
    raise EPythonPlatformServicesNotLoaded.Create('Python platform services not loaded. Drop a component [TPythonPlatform] into your project');
end;

initialization
  TPythonPlatformServices.CreateInstance();

finalization
  TPythonPlatformServices.DestroyInstance();

end.
