library DelphiVCL;

uses
  SysUtils,
  Classes,
  uMain in 'uMain.pas';

{$I ..\..\Source\Definition.Inc}

exports
  // This must match the pattern "PyInit_[ProjectName]"
  // So if the project is named DelphiVCL then
  //   the export must be PyInit_DelphiVCL
  PyInit_DelphiVCL;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$IFDEF LINUX}
{$SONAME 'DelphiVCL'}

{$ENDIF}

begin
end.

