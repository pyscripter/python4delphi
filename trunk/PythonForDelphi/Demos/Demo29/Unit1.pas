unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls, PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    Memo1: TMemo;
    Memo2: TMemo;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    chkUseDC: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  VarPyth, jpeg;

{$R *.dfm}

// Old code not compatible with Unicode
//
//function ImageToString(AGraphic : TGraphic) : String;
//var
//  _stream : TStringStream;
//begin
//  _stream := TStringStream.Create('');
//  try
//    AGraphic.SaveToStream(_stream);
//    Result := _stream.DataString;
//  finally
//    _stream.Free;
//  end;
//end;
//
//function BinStrToPyStr(const AString : String) : Variant;
//var
//  _str : PPyObject;
//begin
//  _str := GetPythonEngine.PyString_FromStringAndSize(PAnsiChar(AString), Length(AString)*SizeOf(Char));
//  Result := VarPythonCreate(_str);
//  GetPythonEngine.Py_DECREF(_str);
//end;

function ImageToPyStr(AGraphic : TGraphic) : Variant;
var
  _stream : TMemoryStream;
  _str : PPyObject;
begin
  _stream := TMemoryStream.Create();
  try
     AGraphic.SaveToStream(_stream);
    _str := GetPythonEngine.PyString_FromStringAndSize(_stream.Memory, _stream.Size);
    Result := VarPythonCreate(_str);
    GetPythonEngine.Py_DECREF(_str);
  finally
    _stream.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  _im : Variant;
  _stream : TMemoryStream;
  _hdc : Variant;
  _dib : Variant;
  S : AnsiString;
begin
  if (Image1.Picture.Graphic = nil) or Image1.Picture.Graphic.Empty then
    raise Exception.Create('You must first select an image');
  PythonEngine1.ExecStrings(Memo1.Lines);
  _im := MainModule.ProcessImage(ImageToPyStr(Image1.Picture.Graphic));
  if not chkUseDC.Checked then
  begin
   {
     TODO :
        Needs fixing.  Probably has to do with images containing null bytes which
        cannot be converted to ansistrings.
   }
    ShowMessage('This does not work and needs fixing');
    _stream := TMemoryStream.Create();
    try
      S := MainModule.ImageToString(_im);
      _stream.Write(PAnsiChar(S)^, Length(S));
      _stream.Position := 0;
      Image1.Picture.Graphic.LoadFromStream(_stream);
    finally
      _stream.Free;
    end;
  end
  else
  begin
    Image1.Picture.Bitmap.Width := Image1.Width;
    Image1.Picture.Bitmap.Height := Image1.Height;
    _hdc := Import('ImageWin').HDC(Image1.Picture.Bitmap.Canvas.Handle); 
    _dib := Import('ImageWin').Dib(_im);
    _dib.expose(_hdc);
  end;
end;

end.
