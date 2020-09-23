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
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Splitter2: TSplitter;
    Memo1: TMemo;
    GroupBox1: TGroupBox;
    Button1: TButton;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    edName: TEdit;
    cbInformatician: TCheckBox;
    rgSex: TRadioGroup;
    cbPythonUser: TCheckBox;
    cbTitle: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    edAge: TEdit;
    Label3: TLabel;
    PythonModule1: TPythonModule;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
  private
    function GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
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
      // Execute script
      ExecStrings( Memo1.Lines );
    end;
end;

procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'GetProperty', GetProperty, 'GetProperty(PropName) -> PropValue' );
      AddDelphiMethod( 'SetProperty', SetProperty, 'SetProperty(PropName, PropValue) -> None' );
      AddDelphiMethod( 'GetPropertyList', GetPropertyList, 'GetPropertyList() -> List of property names' );
    end;
end;

function TForm1.GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:GetProperty',@key ) <> 0 then
      begin
        if key = 'Title' then
          Result := VariantAsPyObject(cbTitle.Text)
        else if key = 'Name' then
          Result := VariantAsPyObject(edName.Text)
        else if key = 'Informatician' then
          Result := VariantAsPyObject(cbInformatician.Checked)
        else if key = 'PythonUser' then
          Result := VariantAsPyObject(cbPythonUser.Checked)
        else if key = 'Age' then
          Result := VariantAsPyObject(edAge.Text)
        else if key = 'Sex' then
          Result := VariantAsPyObject(rgSex.ItemIndex)
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TForm1.SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
  value : PPyObject;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:SetProperty',@key, @value ) <> 0 then
      begin
        if key = 'Title' then
          begin
            cbTitle.Text := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'Name' then
          begin
            edName.Text := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'Informatician' then
          begin
            cbInformatician.Checked := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'PythonUser' then
          begin
            cbPythonUser.Checked := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'Age' then
          begin
            edAge.Text := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'Sex' then
          begin
            rgSex.ItemIndex := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TForm1.GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(6);
      PyList_SetItem(Result, 0, PyString_FromString('Title'));
      PyList_SetItem(Result, 1, PyString_FromString('Name'));
      PyList_SetItem(Result, 2, PyString_FromString('Informatician'));
      PyList_SetItem(Result, 3, PyString_FromString('PythonUser'));
      PyList_SetItem(Result, 4, PyString_FromString('Age'));
      PyList_SetItem(Result, 5, PyString_FromString('Sex'));
    end;
end;

end.
