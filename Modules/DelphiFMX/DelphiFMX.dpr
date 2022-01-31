library DelphiFMX;

uses
  System.StartUpCopy,
  SysUtils,
  Classes,
  uMain in 'uMain.pas',
  ModuleSpecs in 'ModuleSpecs.pas';

{$I ..\..\Source\Definition.Inc}

exports
  // This must match the pattern "PyInit_[ProjectName]"
	// So if the project is named DelphiFMX then
	//   the export must be PyInit_DelphiFMX
	PyInit_DelphiFMX;
{$IFDEF MSWINDOWS}
{$E pyd}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF LINUX}
{$SONAME 'DelphiFMX'}
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}

begin
end.

