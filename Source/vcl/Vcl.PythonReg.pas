unit Vcl.PythonReg;

interface

procedure Register();

implementation

uses
  Classes, Controls, Vcl.PythonGUIInputOutput;

procedure Register();
begin
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  RegisterComponents('Python', [TPythonGUIInputOutput]);
end;
end.
