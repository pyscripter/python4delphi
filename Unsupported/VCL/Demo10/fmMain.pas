unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, PythonEngine, PythonGUIInputOutput, Db,
  DBTables, Grids, DBGrids;

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
    typeTable: TPythonType;
    modDB: TPythonModule;
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
    typeQuery: TPythonType;
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
    typeField: TPythonType;
    TreeView1: TTreeView;
    TabSheet6: TTabSheet;
    Label3: TLabel;
    Memo5: TMemo;
    Panel4: TPanel;
    Button5: TButton;
    Splitter5: TSplitter;
    typeVarArg: TPythonType;
    TabSheet7: TTabSheet;
    Label4: TLabel;
    Memo6: TMemo;
    Splitter6: TSplitter;
    Panel5: TPanel;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure typeTableInitialization(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Table1CalcFields(DataSet: TDataSet);
    procedure typeQueryInitialization(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure typeFieldInitialization(Sender: TObject);
    procedure modDBAfterInitialization(Sender: TObject);
    procedure modDBInitialization(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure typeVarArgInitialization(Sender: TObject);
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

procedure TMain.typeTableInitialization(Sender: TObject);
begin
  with Sender as TPythonType do
    begin
      PyObjectClass := TPyTable;
    end;
end;

procedure TMain.Button2Click(Sender: TObject);
var
  tbl : TPyTable;
  obj : PPyObject;
begin
  // Instantiate a new Python object TPyTable
  obj := typeTable.CreateInstance;
  tbl := PythonToDelphi(obj) as TPyTable;
  // Attach our Delphi table to the Python object
  tbl.SetDataset( Table1, False );
  with GetPythonEngine do
    begin
      // Define a new variable "T" in the DB module
      modDB.SetVar( 'T', obj );
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

procedure TMain.typeQueryInitialization(Sender: TObject);
begin
  with Sender as TPythonType do
    begin
      PyObjectClass := TPyQuery;
    end;
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
  obj := typeTable.CreateInstance;
  tbl := PythonToDelphi(obj) as TPyTable;
  // connect the Datasource2 to the Python TTable
  Datasource2.Dataset := tbl.Table;
  with GetPythonEngine do
    begin
      // Define a new variable "T" in the DB module
      modDB.SetVar( 'T', obj );
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

procedure TMain.typeFieldInitialization(Sender: TObject);
begin
  with Sender as TPythonType do
    begin
      PyObjectClass := TPyField;
    end;
  gFieldType := Sender as TPythonType;
end;

procedure TMain.modDBAfterInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      // Values for type TTableType
      SetVarFromVariant( 'ttDefault', 0 );
      SetVarFromVariant( 'ttParadox', 1 );
      SetVarFromVariant( 'ttDBase', 2 );
      SetVarFromVariant( 'ttASCII', 3 );
      // Values for type TDatasetState
      SetVarFromVariant( 'dsInactive', 0 );
      SetVarFromVariant( 'dsBrowse', 1 );
      SetVarFromVariant( 'dsEdit', 2 );
      SetVarFromVariant( 'dsInsert', 3 );
      SetVarFromVariant( 'dsSetKey', 4 );
      SetVarFromVariant( 'dsCalcFields', 5 );
      SetVarFromVariant( 'dsFilter', 6 );
      SetVarFromVariant( 'dsNewValue', 7 );
      SetVarFromVariant( 'dsOldValue', 8 );
      SetVarFromVariant( 'dsCurValue', 9 );
      // Values for type TFieldType
      SetVarFromVariant( 'ftUnknown', 0 );
      SetVarFromVariant( 'ftString', 1 );
      SetVarFromVariant( 'ftSmallint', 2 );
      SetVarFromVariant( 'ftInteger', 3 );
      SetVarFromVariant( 'ftWord', 4 );
      SetVarFromVariant( 'ftBoolean', 5 );
      SetVarFromVariant( 'ftFloat', 6 );
      SetVarFromVariant( 'ftCurrency', 7 );
      SetVarFromVariant( 'ftBCD', 8 );
      SetVarFromVariant( 'ftDate', 9 );
      SetVarFromVariant( 'ftTime', 10 );
      SetVarFromVariant( 'ftDateTime', 11 );
      SetVarFromVariant( 'ftBytes', 12 );
      SetVarFromVariant( 'ftVarBytes', 13 );
      SetVarFromVariant( 'ftAutoInc', 14 );
      SetVarFromVariant( 'ftBlob', 15 );
      SetVarFromVariant( 'ftMemo', 16 );
      SetVarFromVariant( 'ftGraphic', 17 );
      SetVarFromVariant( 'ftFmtMemo', 18 );
      SetVarFromVariant( 'ftParadoxOle', 19 );
      SetVarFromVariant( 'ftDBaseOle', 20 );
      SetVarFromVariant( 'ftTypedBinary', 21 );
      SetVarFromVariant( 'ftCursor', 22 );
      // Values for type TFieldKind
      SetVarFromVariant( 'fkData', 0 );
      SetVarFromVariant( 'fkCalculated', 1 );
      SetVarFromVariant( 'fkLookup', 2 );
      SetVarFromVariant( 'fkInternalCalc', 3 );
      // Values for type TLocateOption
      SetVarFromVariant( 'loCaseInsensitive', 0 );
      SetVarFromVariant( 'loPartialKey', 1 );
      // Values for type TLockType
      SetVarFromVariant( 'ltReadLock', 0 );
      SetVarFromVariant( 'ltWriteLock', 1 );
      // Values for type TIndexOptions
      SetVarFromVariant( 'ixPrimary', 0 );
      SetVarFromVariant( 'ixUnique', 1 );
      SetVarFromVariant( 'ixDescending', 2 );
      SetVarFromVariant( 'ixCaseInsensitive', 3 );
      SetVarFromVariant( 'ixExpression', 4 );
      // Values for type TDataAction
      SetVarFromVariant( 'daFail', 0 );
      SetVarFromVariant( 'daAbort', 1 );
      SetVarFromVariant( 'daRetry', 2 );
      // Values for type TUpdateKind
      SetVarFromVariant( 'ukModify', 0 );
      SetVarFromVariant( 'ukInsert', 1 );
      SetVarFromVariant( 'ukDelete', 2 );
      // Values for type TUpdateAction
      SetVarFromVariant( 'uaFail', 0 );
      SetVarFromVariant( 'uaAbort', 1 );
      SetVarFromVariant( 'uaSkip', 2 );
      SetVarFromVariant( 'uaRetry', 3 );
      SetVarFromVariant( 'uaApplied', 4 );
    end;
end;

procedure TMain.modDBInitialization(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      with DocString do
        begin
          Add( 'This module contains several Object Types that' );
          Add( 'will let you work with the Borland BDE and access' );
          Add( 'a database.' );
          Add( '' );
          Add( 'CreateTTable() -> creates a TTable instance' );
          Add( 'CreateTQuery() -> creates a TQuery instance' );
        end;
    end;
end;

procedure TMain.Button5Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo5.Lines );
end;

procedure TMain.typeVarArgInitialization(Sender: TObject);
begin
  with Sender as TPythonType do
    begin
      PyObjectClass := TVarArg;
    end;
  gVarArgType := Sender as TPythonType;
end;

procedure TMain.Button6Click(Sender: TObject);
begin
  with PythonEngine1 do
    ExecStrings( Memo6.Lines );
end;

end.

