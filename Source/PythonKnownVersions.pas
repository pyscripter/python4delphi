{$I Definition.Inc}
unit PythonKnownVersions;

interface

uses
  PythonTypes;

const
{$IFDEF MSWINDOWS}
  PYTHON_KNOWN_VERSIONS: array[1..7] of TPythonVersionProp =
    (
    (DllName: 'python33.dll'; RegVersion: '3.3'; APIVersion: 1013),
    (DllName: 'python34.dll'; RegVersion: '3.4'; APIVersion: 1013),
    (DllName: 'python35.dll'; RegVersion: '3.5'; APIVersion: 1013),
    (DllName: 'python36.dll'; RegVersion: '3.6'; APIVersion: 1013),
    (DllName: 'python37.dll'; RegVersion: '3.7'; APIVersion: 1013),
    (DllName: 'python38.dll'; RegVersion: '3.8'; APIVersion: 1013),
    (DllName: 'python39.dll'; RegVersion: '3.9'; APIVersion: 1013)
    );
{$ENDIF}
{$IFDEF _so_files}
  PYTHON_KNOWN_VERSIONS: array[1..7] of TPythonVersionProp =
    (
    (DllName: 'libpython3.3m.so'; RegVersion: '3.3'; APIVersion: 1013),
    (DllName: 'libpython3.4m.so'; RegVersion: '3.4'; APIVersion: 1013),
    (DllName: 'libpython3.5m.so'; RegVersion: '3.5'; APIVersion: 1013),
    (DllName: 'libpython3.6m.so'; RegVersion: '3.6'; APIVersion: 1013),
    (DllName: 'libpython3.7m.so'; RegVersion: '3.7'; APIVersion: 1013),
    (DllName: 'libpython3.8m.so'; RegVersion: '3.8'; APIVersion: 1013),
    (DllName: 'libpython3.9m.so'; RegVersion: '3.9'; APIVersion: 1013)
    );
{$ENDIF}
{$IFDEF DARWIN}
  PYTHON_KNOWN_VERSIONS: array[1..7] of TPythonVersionProp =
    (
    (DllName: 'libpython3.3.dylib'; RegVersion: '3.3'; APIVersion: 1013),
    (DllName: 'libpython3.4.dylib'; RegVersion: '3.4'; APIVersion: 1013),
    (DllName: 'libpython3.5.dylib'; RegVersion: '3.5'; APIVersion: 1013),
    (DllName: 'libpython3.6.dylib'; RegVersion: '3.6'; APIVersion: 1013),
    (DllName: 'libpython3.7.dylib'; RegVersion: '3.7'; APIVersion: 1013),
    (DllName: 'libpython3.8.dylib'; RegVersion: '3.8'; APIVersion: 1013),
    (DllName: 'libpython3.9.dylib'; RegVersion: '3.9'; APIVersion: 1013)
    );
{$endif}
{$IFDEF ANDROID}
  PYTHON_KNOWN_VERSIONS: array[1..1] of TPythonVersionProp =
    (
    (DllName: 'python3.8.so'; RegVersion: '3.8'; APIVersion: 1013)
    );
{$ENDIF}

  COMPILED_FOR_PYTHON_VERSION_INDEX = High(PYTHON_KNOWN_VERSIONS);

implementation

end.
