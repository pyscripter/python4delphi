unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PythonEngine, StdCtrls, ComCtrls, ExtCtrls, PythonGUIInputOutput,
  AtomPythonEngine;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PythonModule1: TPythonModule;
    Panel1: TPanel;
    Button1: TButton;
    Splitter1: TSplitter;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    AtomPythonEngine1: TAtomPythonEngine;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  function  spam_foo( self, args : PPyObject ) : PPyObject; cdecl;

var
  Form1: TForm1;

implementation
{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  AtomPythonEngine1.ExecStrings( Memo1.Lines );
end;

// Here's an example of functions defined for the module spam

function spam_foo( self, args : PPyObject ) : PPyObject; cdecl;
var
  varArgs, first, second, varNewObject: Variant;
begin
  //Use the modified PyObjectAsVariant to get an PytonAtom if the argument
  //can't convert to standard Variant type. (string, integer, array etc.)
  //This way you don't have to think about conversion or reference counting,
  //it is handled automaticly by PythonAtom and AtomPythonEngine
  varArgs := GetPythonEngine.PyObjectAsVariant(args);

  first  := varArgs[0];
  second := varArgs[1];

  ShowMessage( 'first argument: ' + first );
  ShowMessage( 'Name of second argument: ' + second.name );

  varNewObject:=second.getObject(first);
  ShowMessage( 'Name of new object: ' + varNewObject.name );

  //Use VariantAsPyObject to return the object to python
  Result := GetPythonEngine.VariantAsPyObject(varNewObject);
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  // In a module initialization, we just need to add our
  // new methods
  with Sender as TPythonModule do
    begin
      AddMethod( 'foo', spam_foo, 'foo' );
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
