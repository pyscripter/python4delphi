program ThrdDemo;

{$I Definition.Inc}

uses
  Forms,
  ThSort in 'ThSort.pas' {ThreadSortForm},
  SortThds in 'SortThds.pas';

{$R *.res}

begin
  Application.CreateForm(TThreadSortForm, ThreadSortForm);
  Application.Run;
end.
