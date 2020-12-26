unit Unit1;
{
    Demonstrates the manipulation of images on the Delphi side using python.
    Requires the Pillow python module.
    Istall using pip (pip install Pillow).
    Currently the code is setup for Python 3.x, but Python 2 can be supported
    with minor changes. (io.Bytes should be replaced with StringIO.StringIO)
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtDlgs, StdCtrls, ExtCtrls, PythonEngine,
  PythonGUIInputOutput;

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
  VarPyth,
  Math,
  jpeg;

{$R *.dfm}

function ImageToPyBytes(AGraphic : TGraphic) : Variant;
var
  _stream : TMemoryStream;
  _bytes : PPyObject;
begin
  _stream := TMemoryStream.Create();
  try
     AGraphic.SaveToStream(_stream);
    _bytes := GetPythonEngine.PyBytes_FromStringAndSize(_stream.Memory, _stream.Size);
    Result := VarPythonCreate(_bytes);
    GetPythonEngine.Py_DECREF(_bytes);
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
  _dib : Variant;
  pargs: PPyObject;
  presult :PPyObject;
  P : PAnsiChar;
  Len : NativeInt;
begin
  if (Image1.Picture.Graphic = nil) or Image1.Picture.Graphic.Empty then
    raise Exception.Create('You must first select an image');
  PythonEngine1.ExecStrings(Memo1.Lines);
  _im := MainModule.ProcessImage(ImageToPyBytes(Image1.Picture.Graphic));
  if not chkUseDC.Checked then
  begin
    // We have to call PyString_AsStringAndSize because the image may contain zeros
    with GetPythonEngine do begin
      pargs := MakePyTuple([ExtractPythonObjectFrom(_im)]);
      try
        presult := PyEval_CallObjectWithKeywords(
            ExtractPythonObjectFrom(MainModule.ImageToBytes), pargs, nil);
        try
          if PyBytes_AsStringAndSize(presult, P, Len) < 0 then begin
            ShowMessage('This does not work and needs fixing');
            Abort;
          end;

          _stream := TMemoryStream.Create();
          try
            _stream.Write(P^, Len);
            _stream.Position := 0;
            Image1.Picture.Graphic.LoadFromStream(_stream);
          finally
            _stream.Free;
          end;
        finally
          Py_XDECREF(pResult);
        end;
      finally
        Py_DECREF(pargs);
      end;
    end;
  end
  else
  begin
    Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
    _dib := Import('PIL.ImageWin').Dib(_im);
    Image1.Picture.Bitmap.SetSize(Image1.Height, Image1.Width);
    _dib.expose(NativeInt(Image1.Picture.Bitmap.Canvas.Handle));
  end;
end;

end.
