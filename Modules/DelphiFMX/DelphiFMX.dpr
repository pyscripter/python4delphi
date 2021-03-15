library DelphiFMX;

uses
  SysUtils,
  Classes,
  uMain in 'uMain.pas';

{$I ..\..\Source\Definition.Inc}

exports
  // This must match the pattern "PyInit_[ProjectName]"
	// So if the project is named DelphiFMX then
	//   the export must be PyInit_DelphiFMX
	PyInit_DelphiFMX;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$IFDEF LINUX}
{$SONAME 'DelphiFMX'}

{$ENDIF}

begin
end.

