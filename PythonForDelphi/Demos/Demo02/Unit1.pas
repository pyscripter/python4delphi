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
    Memo2: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  Result : PPyObject;
begin
  with PythonEngine1 do
    begin
      Result := EvalStrings( Memo1.Lines );
      if Assigned(Result) then
        begin
          ShowMessage(Format('Eval: %s',[PyObjectAsString(Result)]));
          Py_DECREF(Result);
        end
      else
        ShowMessage('Could not evaluate the script');
      // Or you could simply use:
      // ShowMessage('Eval: ' + EvalStringsAsStr( Memo1.Lines ) );
    end;
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
