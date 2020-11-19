{$I ..\Definition.Inc}
unit PythonPlatforms;

interface

uses
  Classes;

{$IF not Defined(FPC) and (CompilerVersion >= 23)}
const
  {$IF CompilerVersion = 33}
    pidSupportedPlatforms = pidWin32 or pidWin64
                         or pidOSX32 or pidOSX64
                         or pidLinux64;
  {$ELSEIF CompilerVersion >= 34}
    pidSupportedPlatforms = pidWin32 or pidWin64
                         or pidOSX32 or pidOSX64
                         or pidLinux64
                         or pidAndroid32Arm or pidAndroid64Arm;
  {$ELSE}
    pidSupportedPlatforms = pidWin32 or pidWin64 or pidOSX32;
  {$IFEND}
{$IFEND}

implementation

end.
