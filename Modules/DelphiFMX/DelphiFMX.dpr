library DelphiFMX;

uses
  System.StartUpCopy,
  SysUtils,
  Classes,
  FMX.Forms,
  FMX.Types,
  {$IFDEF OSX}
  FMX.Context.Metal,
  {$ENDIF OSX}
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
{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF LINUX}
{$SONAME 'DelphiFMX'}
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}

begin
  {$IFDEF OSX}
  GlobalUseMetal := TCustomContextMetal.IsMetalSupported();
  {$ENDIF OSX}
end.

