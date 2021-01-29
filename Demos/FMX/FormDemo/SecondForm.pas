unit SecondForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TDelphiSecondForm = class(TForm)
    Label1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DelphiSecondForm: TDelphiSecondForm;

implementation

{$R *.fmx}

initialization
  RegisterClass(TDelphiSecondForm);

end.
