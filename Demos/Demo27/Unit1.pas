unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PythonEngine, Vcl.PythonGUIInputOutput, StdCtrls;

type
  TMySeq = class(TPyObject)
  public
    // Mapping services
    function  MpLength : NativeInt; override;
    function  MpSubscript( obj : PPyObject) : PPyObject; override;
    //function  MpAssSubscript( obj1, obj2 : PPyObject) : Integer; override;
  end;

  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonModule1: TPythonModule;
    PythonType1: TPythonType;
    procedure Button1Click(Sender: TObject);
    procedure PythonType1Initialization(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TMySeq }

function TMySeq.MpLength: NativeInt;
begin
  Result := 10;
end;

function TMySeq.MpSubscript(obj: PPyObject): PPyObject;
begin
  Result := obj;
  GetPythonEngine.Py_XINCREF(obj);
end;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings(Memo2.Lines);
end;

procedure TForm1.PythonType1Initialization(Sender: TObject);
begin
  with Sender as TPythonType do
    PyObjectClass := TMySeq;
end;

end.
