unit PythonReg;

interface

uses
  System.Classes, DesignEditors, DesignIntf;

type
  TPythonGUIInputOutputSelectEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register();

implementation

uses
  PythonEngine;

procedure Register;
begin
  RegisterComponents('Python', [TPythonEngine, TPythonType, TPythonModule, TPythonDelphiVar]);
end;

{ TPythonGUIInputOutputSelectEditor }

procedure TPythonGUIInputOutputSelectEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('PythonLoader');
  Proc('PythonInterpreter');
  Proc('PythonEngine');
end;

end.
