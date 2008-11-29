unit Unit1;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QDialogs, QStdCtrls, QControls, QExtCtrls,
{$ENDIF}
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonInputOutput1: TPythonInputOutput;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PythonInputOutput1SendData(Sender: TObject;
      const Data: String);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;



var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        Memo1.Lines.LoadFromFile( FileName );
    end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with SaveDialog1 do
    begin
      if Execute then
        Memo1.Lines.SaveToFile( FileName );
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  PythonEngine1.Finalize;
end;

procedure TForm1.PythonInputOutput1SendData(Sender: TObject;
  const Data: String);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString( PChar(Data) );
{$ENDIF}
{$IFDEF LINUX}
  WriteLn( ErrOutput, Data );
{$ENDIF}
end;

end.
