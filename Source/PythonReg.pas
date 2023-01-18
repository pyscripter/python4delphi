unit PythonReg;

interface

procedure Register();

implementation

uses
  Classes, PythonEngine, WrapDelphi;

procedure Register();
begin
  RegisterComponents('Python', [TPythonEngine, TPythonType, TPythonModule,
                                TPythonDelphiVar, TPythonInputOutput,
                                TPyDelphiWrapper]);
end;

end.
