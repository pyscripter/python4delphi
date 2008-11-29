library demodll;

{$I Definition.Inc}

uses
  SysUtils,
  Classes,
  module in 'module.pas';

exports
  initdemodll;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$IFDEF LINUX}
{$SONAME 'demodll'}

{$ENDIF}

begin
end.
