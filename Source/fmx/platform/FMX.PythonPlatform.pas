unit FMX.PythonPlatform;

interface

uses
  System.Classes, PythonPlatforms;

type
  [ComponentPlatformsAttribute(pidSupportedPlatforms)]
  TPythonPlatform = class(TComponent)
  public
    class procedure RegisterServices(); static;
    class procedure UnRegisterServices(); static;
  end;

implementation

uses
  PythonPlatformServices, FMX.PythonLoaderService;

{ TPythonPlatform }

class procedure TPythonPlatform.RegisterServices;
begin
  if TPythonPlatformServices.Instance.Loaded then
    Exit;
  TPythonPlatformServices.Instance.Loaded := true;
  TPythonPlatformServices.Instance.AddService(IPythonLoaderService, TPythonLoaderService.Create());
end;

class procedure TPythonPlatform.UnRegisterServices;
begin
  TPythonPlatformServices.Instance.RemoveService(IPythonLoaderService);
  TPythonPlatformServices.Instance.Loaded := false;
end;

initialization
  TPythonPlatform.RegisterServices();

finalization
  TPythonPlatform.UnRegisterServices();

end.
