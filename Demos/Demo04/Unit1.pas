unit Unit1;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Memo1: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonDelphiVar1: TPythonDelphiVar;
    Button4: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonDelphiVar2: TPythonDelphiVar;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
    procedure PythonDelphiVar1GetData(Sender: TObject; var Data: Variant);
    procedure PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
    procedure PythonDelphiVar2ExtGetData(Sender: TObject;
      var Data: PPyObject);
    procedure PythonDelphiVar2ExtSetData(Sender: TObject; Data: PPyObject);
  private
    { Déclarations privées }
    FMyPythonObject : PPyObject;
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

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowMessage( 'Value = ' + PythonDelphiVar1.ValueAsString );
end;

procedure TForm1.PythonDelphiVar1Change(Sender: TObject);
begin
  with Sender as TPythonDelphiVar do
    ShowMessage( 'Var test changed: ' + ValueAsString );
end;

procedure TForm1.PythonDelphiVar1GetData(Sender: TObject;
  var Data: Variant);
begin
  Data := Edit1.Text;
end;

procedure TForm1.PythonDelphiVar1SetData(Sender: TObject; Data: Variant);
begin
  Edit1.Text := Data;
end;

procedure TForm1.PythonDelphiVar2ExtGetData(Sender: TObject;
  var Data: PPyObject);
begin
  with GetPythonEngine do
    begin
      Data := FMyPythonObject;
      Py_XIncRef(Data); // This is very important
    end;
end;

procedure TForm1.PythonDelphiVar2ExtSetData(Sender: TObject;
  Data: PPyObject);
begin
  with GetPythonEngine do
    begin
      Py_XDecRef(FMyPythonObject); // This is very important
      FMyPythonObject := Data;
      Py_XIncRef(FMyPythonObject); // This is very important
    end;
end;

end.
