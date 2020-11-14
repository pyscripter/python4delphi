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
  Vcl.PythonServicesProvider,
  Vcl.PythonGUIInputOutput;

procedure Register;
begin
  GroupDescendentsWith(TPythonServicesProvider, TControl);
  GroupDescendentsWith(TPythonGUIInputOutput, TControl);
  RegisterComponents('Python', [TPythonServicesProvider, TPythonGUIInputOutput]);
  RegisterSelectionEditor(TPythonGUIInputOutput, TPythonGUIInputOutputSelectEditor);
end;

{ TPythonEngineSelectEditor }

procedure TPythonEngineSelectEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('PythonLoader');
  Proc('PythonInterpreter');
  Proc('PythonEngine');
end;

{ TPythonGUIInputOutputSelectEditor }

procedure TPythonGUIInputOutputSelectEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('PythonInputOutput');
  Proc('VCL.PythonGUIInputOutput');
end;

end.
