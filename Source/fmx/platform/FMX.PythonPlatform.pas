unit FMX.PythonPlatform;

interface

uses
  System.Classes;

type
  TPythonPlatform = class(TComponent)
  public
    class procedure RegisterServices(); static;
    class procedure UnRegisterServices(); static;
  end;

implementation

uses
  PythonPlatformServices;

{ TPythonPlatformServices }

class procedure TPythonPlatform.RegisterServices;
begin

end;

class procedure TPythonPlatform.UnRegisterServices;
begin

end;

initialization
  TPythonPlatform.RegisterServices();

finalization
  TPythonPlatform.UnRegisterServices();

end.
