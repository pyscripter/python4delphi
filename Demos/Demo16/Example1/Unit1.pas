unit Unit1;

{$I Definition.Inc}

interface

uses
  SysUtils, Classes,
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls,
  PythonEngine, PythonGUIInputOutput;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    PythonDelphiVar1: TPythonDelphiVar;
    edName: TEdit;
    cbInformatician: TCheckBox;
    rgSex: TRadioGroup;
    cbPythonUser: TCheckBox;
    cbTitle: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    edAge: TEdit;
    Label3: TLabel;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PythonDelphiVar1ExtGetData(Sender: TObject;
      var Data: PPyObject);
    procedure PythonDelphiVar1ExtSetData(Sender: TObject; Data: PPyObject);
    procedure PythonDelphiVar1Change(Sender: TObject);
  private
    FProperties : PPyObject;

    function CreateProperties : PPyObject;
    procedure UpdateProperties( props : PPyObject );
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with GetPythonEngine do
    begin
      // Detach from previous object
      Py_XDecRef(FProperties);
      // Create new object
      FProperties := CreateProperties;
      // Execute script
      ExecStrings( Memo1.Lines );
    end;
end;

procedure TForm1.PythonDelphiVar1ExtGetData(Sender: TObject;
  var Data: PPyObject);
begin
  with GetPythonEngine do
    begin
      // Return our object
      Data := FProperties;
      // Don't forget to increment it, otherwise we would loose property !
      Py_XIncRef(Data);
    end;
end;

procedure TForm1.PythonDelphiVar1ExtSetData(Sender: TObject;
  Data: PPyObject);
begin
  with GetPythonEngine do
    begin
      // Check if the transmitted object is a dictionary
      if not PyDict_Check(Data) then
        Exit;
      // Acquire property to the transmitted object
      Py_XIncRef(Data);
      // Release property of our previous object
      Py_XDecRef(FProperties);
      // Assisgn transmitted object
      FProperties := Data;
    end;
end;

function TForm1.CreateProperties : PPyObject;
begin
  with GetPythonEngine do
    begin
      // Create new dictionary
      Result := PyDict_New();
      // Assign key/values
      PyDict_SetItemString( Result, 'Title', VariantAsPyObject(cbTitle.Text) );
      PyDict_SetItemString( Result, 'Name', VariantAsPyObject(edName.Text) );
      PyDict_SetItemString( Result, 'Informatician', VariantAsPyObject(cbInformatician.Checked) );
      PyDict_SetItemString( Result, 'PythonUser', VariantAsPyObject(cbPythonUser.Checked) );
      PyDict_SetItemString( Result, 'Age', VariantAsPyObject(StrToIntDef(edAge.Text, 0)) );
      PyDict_SetItemString( Result, 'Sex', VariantAsPyObject(rgSex.ItemIndex) );
    end;
end;

procedure TForm1.UpdateProperties( props : PPyObject );
begin
  with GetPythonEngine do
    begin
      // Check if the transmitted object is a dictionary
      if not PyDict_Check(props) then
        Exit;
      // Extract our key/values
      cbTitle.Text            := PyObjectAsVariant( PyDict_GetItemString(props, 'Title') );
      edName.Text             := PyObjectAsVariant( PyDict_GetItemString(props, 'Name') );
      cbInformatician.Checked := PyObjectAsVariant( PyDict_GetItemString(props, 'Informatician') );
      cbPythonUser.Checked    := PyObjectAsVariant( PyDict_GetItemString(props, 'PythonUser') );
      edAge.Text              := PyObjectAsVariant( PyDict_GetItemString(props, 'Age') );
      rgSex.ItemIndex         := PyObjectAsVariant( PyDict_GetItemString(props, 'Sex') );
    end;
end;

procedure TForm1.PythonDelphiVar1Change(Sender: TObject);
begin
  UpdateProperties( FProperties );
end;

end.
