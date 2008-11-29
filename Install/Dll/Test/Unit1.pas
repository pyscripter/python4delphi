unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses fmDelphiInstall;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
  DelphiInstall.ShowModal;
end;

end.
