@echo off
call rsvars.bat
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Win32 DelphiVCL/DelphiVCL.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Win64 DelphiVCL/DelphiVCL.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Win32 DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Win64 DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Android DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Android64 DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=Linux64 DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=OSX32 DelphiFMX/DelphiFMX.dproj
msbuild /t:Clean /t:Build /p:config=Release /p:platform=OSX64 DelphiFMX/DelphiFMX.dproj
