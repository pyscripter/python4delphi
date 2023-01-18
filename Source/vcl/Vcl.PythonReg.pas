{$I ..\Definition.Inc}
unit Vcl.PythonReg;

interface

procedure Register();

implementation

uses
  Classes, Controls, Vcl.PythonGUIInputOutput;

procedure Register();
begin
  {$IFDEF DELPHIXE2_OR_HIGHER}
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  {$ENDIF}
  RegisterComponents('Python', [TPythonGUIInputOutput]);
end;
end.
