// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
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
