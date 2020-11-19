unit Vcl.PythonRegistration;

interface

procedure Register();

implementation

uses
  Classes, Controls, Vcl.PythonGUIInputOutput, Vcl.PythonPlatform;

procedure Register();
begin
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  GroupDescendentsWith(TPythonPlatform, TControl);
  RegisterComponents('Python', [TPythonGUIInputOutput, TPythonPlatform]);
end;
end.
