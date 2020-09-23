unit Unit1;

interface

{$I Definition.Inc}

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls,
  Variants, PythonEngine, PythonGUIInputOutput;

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
    PythonModule1: TPythonModule;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  public
  end;



var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);

  procedure DisplayArrayVar( V : Variant );
  var
    i, j : Integer;
    item : Variant;
    s : String;
  begin
    Memo2.Lines.Add('Displaying a variant array generated from a Python sequence:');
    for i := VarArrayLowBound( V, 1 ) to VarArrayHighBound( V, 1 ) do
      begin
        item := V[i];
        for j := VarArrayLowBound( item, 1 ) to VarArrayHighBound( item, 1 ) do
          begin
            s := item[j];
            Memo2.Lines.Add(Format('(%d, %d) = %s',[i, j, s]));
          end;
      end;
  end;

var
  ComArray : Variant;
begin
  ComArray := VarArrayCreate([0, 3, 0, 2], varVariant);
  ComArray[0, 0] := 1;
  ComArray[0, 1] := 1.1;
  ComArray[0, 2] := 'a';
  ComArray[1, 0] := 2;
  ComArray[1, 1] := 2.2;
  ComArray[1, 2] := 'b';
  ComArray[2, 0] := 3;
  ComArray[2, 1] := 3.3;
  ComArray[2, 2] := 'c';
  ComArray[3, 0] := 4;
  ComArray[3, 1] := 4.4;
  ComArray[3, 2] := 'd';
  PythonModule1.SetVarFromVariant( 'L', ComArray );
  PythonEngine1.ExecStrings(Memo1.Lines);
  DisplayArrayVar( PythonModule1.GetVarAsVariant( 'L' ) );
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
