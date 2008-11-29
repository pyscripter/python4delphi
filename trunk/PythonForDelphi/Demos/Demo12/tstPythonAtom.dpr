program tstPythonAtom;

uses
  Forms,
  F_Main in 'F_Main.pas' {FRM_Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFRM_Main, FRM_Main);
  Application.Run;
end.
