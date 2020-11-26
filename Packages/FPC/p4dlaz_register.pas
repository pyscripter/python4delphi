unit P4DLaz_register;

interface

uses SysUtils, Classes;

procedure Register;

implementation
{$R '..\..\Source\PythonEngine.dcr'}

uses 
  PythonEngine, 
  Lcl.PythonGUIInputOutput, 
  WrapDelphi;

procedure Register;
begin
  RegisterComponents('Python', [TPythonEngine, TPythonInputOutput,
                                TPythonType, TPythonModule, TPythonDelphiVar]);
  RegisterComponents('Python', [TPythonGUIInputOutput]);
  RegisterComponents('Python', [TPyDelphiWrapper]);
end;

end.
