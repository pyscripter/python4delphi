unit Vcl.PythonReg;

interface

uses
  System.Classes, DesignEditors, DesignIntf;

type
  TPythonEngineSelectEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TPythonGUIInputOutputSelectEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Vcl.Controls,
  Vcl.PythonComp;

procedure Register;
begin
  GroupDescendentsWith(TPythonEngine, TControl);
  GroupDescendentsWith(TPythonType, TControl);
  GroupDescendentsWith(TPythonModule, TControl);
  GroupDescendentsWith(TPythonDelphiVar, TControl);
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);

  RegisterComponents('Python',[TPythonEngine, TPythonType, TPythonModule, TPythonDelphiVar, TPythonGUIInputOutput]);

  RegisterSelectionEditor(TPythonEngine, TPythonEngineSelectEditor);
  RegisterSelectionEditor(TPythonGUIInputOutput, TPythonGUIInputOutputSelectEditor);
end;

{ TPythonEngineSelectEditor }

procedure TPythonEngineSelectEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('PythonLoader');
  Proc('PythonInterpreter');
  Proc('PythonEngine');
  Proc('VCL.PythonComp');
end;

{ TPythonGUIInputOutputSelectEditor }

procedure TPythonGUIInputOutputSelectEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('PythonInputOutput');
  Proc('VCL.PythonComp');
end;

end.
