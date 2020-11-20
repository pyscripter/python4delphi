unit FMX.PythonRegistration;

interface

procedure Register;

implementation

uses
  System.Classes, FMX.Controls, FMX.PythonGUIInputOutput, FMX.PythonPlatform;

procedure Register;
begin
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  GroupDescendentsWith(TPythonPlatform, TControl);
  RegisterComponents('Python',[TPythonGUIInputOutput, TPythonPlatform]);
end;

end.
