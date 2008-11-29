unit Unit2;

interface

uses
  Windows, Messages, SysUtils,
{$IFDEF DELPHI6_OR_HIGHER}
  Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TTestForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    btnClose: TButton;
    Edit1: TEdit;
    ListBox1: TListBox;
    btnAdd: TButton;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TestForm: TTestForm;

implementation

{$R *.dfm}

procedure TTestForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  RegisterClass(TTestForm);
end.

