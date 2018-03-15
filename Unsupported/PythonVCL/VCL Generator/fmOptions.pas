unit fmOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TOptions = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure Init;
    procedure Load;
    procedure Save;
  end;

const
  kOptions = 'options.dat';

var
  Options: TOptions;

implementation

uses unMisc;
{$R *.DFM}

procedure TOptions.Init;
begin
end;

procedure TOptions.Load;
var
  f : String;
  S : TFileStream;
begin
  Init;
  f := ExtractFilePath( Application.ExeName ) + kOptions;
  if not FileExists( f ) then
    Exit;
  S := TFileStream.Create( f, fmOpenRead );
  try
//    edPythonFiles.Text := ReadString( S );
//    edDelphiFiles.Text := ReadString( S );
  finally
    S.Free;
  end;
end;

procedure TOptions.Save;
var
  f : String;
  S : TFileStream;
begin
  f := ExtractFilePath( Application.ExeName ) + kOptions;
  S := TFileStream.Create( f, fmCreate );
  try
//    WriteString( S, edPythonFiles.Text );
//    WriteString( S, edDelphiFiles.Text );
  finally
    S.Free;
  end;
end;


procedure TOptions.FormCreate(Sender: TObject);
begin
  Load;
end;

procedure TOptions.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOk then
    Save
  else
    Load;
end;

end.
