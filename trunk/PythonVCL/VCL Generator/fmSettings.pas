unit fmSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls;

type
  TSettings = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    edPythonFiles: TEdit;
    edDelphiFiles: TEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    meExclusion: TMemo;
    TabSheet4: TTabSheet;
    meGlobals: TMemo;
    edComponentName: TEdit;
    edUnitName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Déclarations privées }
    FComponentName : String;
    FUnitName : String;
    FPythonFiles : String;
    FDelphiFiles : String;
    FExclusion : String;
    FGlobals : String;
  public
    { Déclarations publiques }
    procedure LoadFromStream( S : TStream );
    procedure SaveToStream( S : TStream );
  end;

var
  Settings: TSettings;

implementation
uses unMisc, fmMain;
{$R *.DFM}

procedure TSettings.LoadFromStream( S : TStream );
begin
  edComponentName.Text := ReadString( S );
  edUnitName.Text := ReadString( S );
  edPythonFiles.Text := ReadString( S );
  edDelphiFiles.Text := ReadString( S );
  meExclusion.Lines.Text := ReadString( S );
  meGlobals.Lines.Text := ReadString( S );
end;

procedure TSettings.SaveToStream( S : TStream );
begin
  WriteString( S, edComponentName.Text );
  WriteString( S, edUnitName.Text );
  WriteString( S, edPythonFiles.Text );
  WriteString( S, edDelphiFiles.Text );
  WriteString( S, meExclusion.Text );
  WriteString( S, meGlobals.Text );
end;


procedure TSettings.FormShow(Sender: TObject);
begin
  FComponentName := edComponentName.Text;
  FUnitName := edUnitName.Text;
  FPythonFiles := edPythonFiles.Text ;
  FDelphiFiles := edDelphiFiles.Text;
  FExclusion := meExclusion.Text;
  FGlobals := meGlobals.Text;
end;

procedure TSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult = mrOk then
    MainForm.FChanged := True
  else
    begin
      edComponentName.Text := FComponentName;
      edUnitName.Text := FUnitName;
      edPythonFiles.Text := FPythonFiles;
      edDelphiFiles.Text := FDelphiFiles;
      meExclusion.Text := FExclusion;
      meGlobals.Text := FGlobals;
    end;
end;

end.
