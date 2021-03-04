// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
library demodll;



uses
  SysUtils,
  Classes,
  module in 'module.pas';

exports
  initdemodll,
  PyInit_demodll;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$IFDEF LINUX}
{$SONAME 'demodll'}

{$ENDIF}

begin
end.
