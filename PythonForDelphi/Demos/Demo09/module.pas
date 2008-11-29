unit module;

interface
uses PythonEngine;

procedure initdemodll; cdecl;

var
  gEngine : TPythonEngine;
  gModule : TPythonModule;

implementation

function Add( Self, Args : PPyObject ) : PPyObject; far; cdecl;
var
  a, b : Integer;
begin
  with GetPythonEngine do
    begin
      if PyArg_ParseTuple( args, 'ii:Add', [@a, @b] ) <> 0 then
        begin
          Result := PyInt_FromLong( a + b );
        end
      else
        Result := nil;
    end;
end;

procedure initdemodll;
begin
  try
    gEngine := TPythonEngine.Create(nil);
    gEngine.AutoFinalize := False;
    gEngine.LoadDll;
    gModule := TPythonModule.Create(nil);
    gModule.Engine := gEngine;
    gModule.ModuleName := 'demodll';
    gModule.AddMethod( 'add', @Add, 'add(a,b) -> a+b' );
    gModule.Initialize;
  except
  end;
end;

initialization
finalization
  gEngine.Free;
  gModule.Free;
end.


