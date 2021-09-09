@echo off
call rsvars.bat
for /L %%G in (4,1,7) do (
    echo %%G > DelphiFMX\PythonVersionIndex.inc
    echo %%G > DelphiVCL\PythonVersionIndex.inc
    rmdir /s /q DelphiFMX\pyd%%G >nul
    rmdir /s /q DelphiVCL\pyd%%G >nul
    Echo VCL Win32 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Win32 DelphiVCL/DelphiVCL.dproj
    Echo VCL Win64 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Win64 DelphiVCL/DelphiVCL.dproj
    echo FMX Win32 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Win32 DelphiFMX/DelphiFMX.dproj
    Echo FMX Win64 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Win64 DelphiFMX/DelphiFMX.dproj
    if %%G==7 (
        echo FMX Android 32-bit Index %%G
        msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Android DelphiFMX/DelphiFMX.dproj
        echo FMX Android 64-bit Index %%G
        msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Android64 DelphiFMX/DelphiFMX.dproj
    )
    echo FMX Linux64 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=Linux64 DelphiFMX/DelphiFMX.dproj
rem echo FMX OSX32 Index %%G
rem msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=OSX32 DelphiFMX/DelphiFMX.dproj
    echo FMX OSX64 Index %%G
    msbuild /nologo /v:m /t:Clean /t:Build /p:config=Release /p:platform=OSX64 DelphiFMX/DelphiFMX.dproj

    ren DelphiFMX\pyd pyd%%G
    ren DelphiVCL\pyd pyd%%G
)