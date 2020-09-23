unit Unit1;

{$I Definition.Inc}

interface

uses
  Classes, SysUtils,
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
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  public
  end;



var
  Form1: TForm1;

implementation

uses
  VarPyth;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  P : Variant;
begin
  PythonEngine1.ExecStrings( Memo1.Lines );
  P := MainModule.Person('John', 'Doe');
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(VarIsNone(P.weight));
  Assert(VarIsNone(P.height));
  Assert(VarIsNone(P.age));
  P := MainModule.Person('John', 'Doe', weight := 70);
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(P.weight = 70);
  Assert(VarIsNone(P.height));
  Assert(VarIsNone(P.age));
  P := MainModule.Person('John', 'Doe', weight := 70, height := 172);
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(P.weight = 70);
  Assert(P.height = 172);
  Assert(VarIsNone(P.age));
  P := MainModule.Person('John', 'Doe', weight := 70, height := 172, age := 35);
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(P.weight = 70);
  Assert(P.height = 172);
  Assert(P.age = 35);
  P := MainModule.Person(last_name := 'Doe', first_name := 'John', weight := 70, height := 172, age := 35);
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(P.weight = 70);
  Assert(P.height = 172);
  Assert(P.age = 35);
  P := MainModule.Person('John', 'Doe', 35, 172, 70);
  Assert(P.first_name = 'John');
  Assert(P.last_name = 'Doe');
  Assert(P.weight = 70);
  Assert(P.height = 172);
  Assert(P.age = 35);
  Memo2.Lines.Add('Success')
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

end.
