program ThrdDemo;

uses
  Forms,
  SortThds in 'SortThds.pas',
  ThSort in 'ThSort.pas' {ThreadSortForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TThreadSortForm, ThreadSortForm);
  Application.Run;
end.
