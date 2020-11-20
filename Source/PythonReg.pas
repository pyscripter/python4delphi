unit PythonReg;

interface

procedure Register();

implementation

uses
  Classes, PythonEngine, WrapDelphi, PythonGUIInputOutput;

procedure Register();
begin
  RegisterComponents('Python', [TPythonEngine, TPythonType, TPythonModule,
                                TPythonDelphiVar, TPythonInputOutput,
                                TPyDelphiWrapper, TPythonGUIInputOutput]);
end;

end.
