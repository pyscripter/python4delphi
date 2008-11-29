unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, PythonEngine, PythonGUIInputOutput, Db,
  DBTables, Grids, DBGrids, PythonDatabase;

type
  TMain = class(TForm)
    RichEdit1: TRichEdit;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    PythonEngine1: TPythonEngine;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Memo1: TMemo;
    Button1: TButton;
    Table1: TTable;
    Memo2: TMemo;
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    Button2: TButton;
    DataSource1: TDataSource;
    TabSheet3: TTabSheet;
    Table1CustNo: TFloatField;
    Table1Company: TStringField;
    Table1Addr1: TStringField;
    Table1Addr2: TStringField;
    Table1City: TStringField;
    Table1State: TStringField;
    Table1Zip: TStringField;
    Table1Country: TStringField;
    Table1Phone: TStringField;
    Table1FAX: TStringField;
    Table1TaxRate: TFloatField;
    Table1Contact: TStringField;
    Table1LastInvoiceDate: TDateTimeField;
    Table1Demo: TStringField;
    Label1: TLabel;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Memo4: TMemo;
    Splitter2: TSplitter;
    Panel2: TPanel;
    Button3: TButton;
    Label2: TLabel;
    Splitter3: TSplitter;
    Memo3: TMemo;
    Splitter4: TSplitter;
    Panel3: TPanel;
    Button4: TButton;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    TreeView1: TTreeView;
    TabSheet6: TTabSheet;
    Label3: TLabel;
    Memo5: TMemo;
    Panel4: TPanel;
    Button5: TButton;
    Splitter5: TSplitter;
    PythonDatabase1: TPythonDatabase;
    TabSheet7: TTabSheet;
    Memo6: TMemo;
    Splitter6: TSplitter;
    Panel5: TPanel;
    Button6: TButton;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Table1CalcFields(DataSet: TDataSet);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation
uses pyDB, pyDBTables;
{$R *.DFM}

procedure TMain.Button1Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo1.Lines );
end;

procedure TMain.Button2Click(Sender: TObject);
var
  tbl : TPyTable;
  obj : PPyObject;
begin
  // Instantiate a new Python object TPyTable
  obj := gTableType.CreateInstance;
  tbl := PythonToDelphi(obj) as TPyTable;
  // Attach our Delphi table to the Python object
  tbl.SetDataset( Table1, False );
  with GetPythonEngine do
    begin
      // Define a new variable "T" in the DB module
      gDBTablesModule.SetVar( 'T', obj );
      Py_XDecRef(obj);
      // Excecute the script
      ExecStrings( memo2.Lines );
    end;
end;

procedure TMain.Table1CalcFields(DataSet: TDataSet);
begin
  Dataset.FieldByName('Demo').AsString := Dataset.FieldByName('State').AsString + ' - ' +
                                          Dataset.FieldByName('Zip').AsString;
end;

procedure TMain.Button3Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo4.Lines );
end;

procedure TMain.Button4Click(Sender: TObject);
var
  tbl : TPyTable;
  obj : PPyObject;
begin
  // Instantiate a new Python object TPyTable
  obj := gTableType.CreateInstance;
  tbl := PythonToDelphi(obj) as TPyTable;
  // connect the Datasource2 to the Python TTable
  Datasource2.Dataset := tbl.Table;
  with GetPythonEngine do
    begin
      // Define a new variable "T" in the DB module
      gDBTablesModule.SetVar( 'T', obj );
      Py_XDecRef(obj);
      // Excecute the script
      ExecStrings( memo3.Lines );
    end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Memo1.Lines.LoadFromFile( 'Example1.py' );
  Memo2.Lines.LoadFromFile( 'Example2.py' );
  Memo3.Lines.LoadFromFile( 'Example3.py' );
  Memo4.Lines.LoadFromFile( 'Example4.py' );
  Memo5.Lines.LoadFromFile( 'Example5.py' );
  Memo6.Lines.LoadFromFile( 'Example6.py' );
  PageControl1.ActivePage := TabSheet1;
end;

procedure TMain.Button5Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo5.Lines );
end;

procedure TMain.Button6Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo6.Lines );
end;

end.

