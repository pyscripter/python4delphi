unit PythonLoaderService;

interface

type
  IPythonLoaderService = interface
    ['{C223FB58-E5FC-407F-A061-8E6DABEF1DB7}']
    function IsHandleValid(const AHandle: THandle): boolean;
    function LoadDll(const ADllPath: string; out AHandle: THandle): boolean;
    procedure UnloadDll(var AHandle: THandle);
    function Import(const AHandle: THandle; const AFuncName: AnsiString; ACanFail: boolean = true): pointer;

    procedure FatalMsgDlg(const AMsg: string);
    procedure FatalAbort(const AMsg: string);
  end;

implementation

end.
