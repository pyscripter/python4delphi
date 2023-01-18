unit FMX.PythonReg;

interface

procedure Register;

implementation

uses
  System.Classes, FMX.Controls, FMX.PythonGUIInputOutput;

procedure Register;
begin
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  RegisterComponents('Python',[TPythonGUIInputOutput]);
end;

end.
