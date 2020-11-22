unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  System.Types, System.UITypes,
  ComCtrls, ExtCtrls, StdCtrls, PythonEngine, Vcl.PythonGUIInputOutput, Db,
  Grids, DBGrids, Datasnap.DBClient, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Comp.UI,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  WrapDelphi, WrapFireDac, SynEditHighlighter, SynHighlighterPython, SynEdit,
  SynEditCodeFolding, sqlTimSt, FireDAC.VCLUI.Wait,
  FireDAC.Phys.SQLiteWrapper.Stat;

type
  TMain = class(TForm)
    RichEdit1: TRichEdit;
    Splitter1: TSplitter;
    PythonGUIInputOutput: TPythonGUIInputOutput;
    modDBFireDac: TPythonModule;
    dsrcCustomer: TDataSource;
    DataSource2: TDataSource;
    Panel6: TPanel;
    btnSQLTest: TButton;
    cobxConnSQLServer: TComboBox;
    Label5: TLabel;
    Connection: TFDConnection;
    mqSrcTables: TFDMetaInfoQuery;
    SynPythonSyn: TSynPythonSyn;
    PythonEngine: TPythonEngine;
    tblCustomer: TFDTable;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    Splitter7: TSplitter;
    SynEditScript1: TSynEdit;
    Panel7: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    btnExecuteExample1: TButton;
    TabSheet2: TTabSheet;
    Splitter3: TSplitter;
    Panel1: TPanel;
    btnExecuteExample2: TButton;
    DBGrid1: TDBGrid;
    SynEditScript2: TSynEdit;
    TabSheet4: TTabSheet;
    Splitter4: TSplitter;
    Panel3: TPanel;
    btnExecuteExample3: TButton;
    DBGrid2: TDBGrid;
    SynEditScript3: TSynEdit;
    TabSheet5: TTabSheet;
    Splitter2: TSplitter;
    Panel2: TPanel;
    btnExecuteExample4: TButton;
    SynEditScript4: TSynEdit;
    PyDelphiWrapper: TPyDelphiWrapper;
    procedure btnExecuteExample1Click(Sender: TObject);
    procedure btnExecuteExample2Click(Sender: TObject);
    procedure Table1CalcFields(DataSet: TDataSet);
    procedure btnExecuteExample4Click(Sender: TObject);
    procedure btnExecuteExample3Click(Sender: TObject);
    procedure modDBFireDacInitialization(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSQLTestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    function DBConnectionClosedCheck(aConnectionDefName: String): Boolean;
  public
  end;

var
  Main: TMain;


implementation

{$R *.DFM}


procedure TMain.FormShow(Sender: TObject);
var
 i: Integer;
 l_sStr: String;
begin
 SynEditScript1.Lines.LoadFromFile( 'Example1.py' );
 SynEditScript2.Lines.LoadFromFile( 'Example2.py' );
 SynEditScript3.Lines.LoadFromFile( 'Example3.py' );
 SynEditScript4.Lines.LoadFromFile( 'Example4.py' );
 //
 PageControl.ActivePage := TabSheet1;
 //
 RichEdit1.Lines.Clear;
 cobxConnSQLServer.ItemIndex := -1;
 if FileExists('FDDrivers.ini') then begin
   FDManager.DriverDefFileName := 'FDDrivers.ini';
   FDManager.DriverDefFileAutoLoad := True;
 end
 else begin
   l_sStr := 'Die Ini-Datei "FDDrivers.ini" existiert nicht!';
   RichEdit1.Lines.Add(l_sStr);
   MessageDlg(l_sStr, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
   Close;
 end;
 if FileExists('Demo.ini') then begin
   FDManager.ConnectionDefFileName := 'Demo.ini';
   try
     FDManager.LoadConnectionDefFile;
     FDManager.GetConnectionNames(cobxConnSQLServer.Items);
     for i := 0 to cobxConnSQLServer.Items.Count-1 do begin
       l_sStr := UpperCase(cobxConnSQLServer.Items[i]);
       if (Pos('SQLITE',l_sStr) > 0) and (cobxConnSQLServer.ItemIndex < 0) then begin
         cobxConnSQLServer.ItemIndex := i;
         break;
       end;
     end;
   except
     on E: Exception do begin
       l_sStr := 'Exception: ' + E.Message;
       RichEdit1.Lines.Add(l_sStr);
       MessageDlg(l_sStr, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
     end;
   end;
 end
 else begin
   l_sStr := 'Die Ini-Datei "Demo.ini" existiert nicht!';
   RichEdit1.Lines.Add(l_sStr);
   MessageDlg(l_sStr, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
   Close;
 end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
 if Connection.Connected then
   Connection.Close;
end;

procedure TMain.btnSQLTestClick(Sender: TObject);
var
 l_sStr, l_sConnName: String;
begin
 RichEdit1.Lines.Clear;
 l_sConnName := cobxConnSQLServer.Items[cobxConnSQLServer.ItemIndex];
 l_sStr := UpperCase(l_sConnName);
 if Pos('ORA',l_sStr) > 0 then begin
   RichEdit1.Lines.Add('Oracle Parameter:');
 end
 else if Pos('MSSQL',l_sStr) > 0 then begin
   RichEdit1.Lines.Add('MS SQL Server Parameter:');
 end
 else if Pos('SQLITE',l_sStr) > 0 then begin
   RichEdit1.Lines.Add('SQLite Parameter:');
 end
 else begin
   RichEdit1.Lines.Add('Unkown database type!');
   exit;
 end;
 if self.DBConnectionClosedCheck(l_sConnName) then
   RichEdit1.Lines.Add('Database was connected!')
 else
   RichEdit1.Lines.Add('Database was not connected!');
end;

function TMain.DBConnectionClosedCheck(aConnectionDefName: String): Boolean;
var
 l_sStr: String;
 l_oDef: IFDStanConnectionDef;
begin
 Result := True;
 if Connection.Connected then
   Connection.Close;
 if Connection.ConnectionDefName <> aConnectionDefName then begin
   l_oDef := FDManager.ConnectionDefs.ConnectionDefByName(aConnectionDefName);
   Connection.Params.Clear;
   Connection.Params.AddStrings(l_oDef.Params);
   Connection.ConnectionDefName := aConnectionDefName;
   //
   try
     Connection.Connected := True;
     if Connection.Connected then begin
       Result := True;
       Connection.Close;
     end;
   except
     on E: Exception do begin
       Result := False;
       l_sStr := 'Exception: ' + E.Message;
       RichEdit1.Lines.Add(l_sStr);
     end;
   end;
 end;
end;

procedure TMain.Table1CalcFields(DataSet: TDataSet);
begin
 Dataset.FieldByName('Demo').AsString := Dataset.FieldByName('State').AsString + ' - ' +
                                         Dataset.FieldByName('Zip').AsString;
end;

procedure TMain.modDBFireDacInitialization(Sender: TObject);
begin
// pyDBFireDac.g_oDBModule := Sender as TPythonModule;
 with Sender as TPythonModule do begin
   with DocString do begin
     Add( 'This module contains several Object Types that' );
     Add( 'will let you work with FireDAC and access' );
     Add( 'a database.' );
     Add( '' );
     Add( 'CreateDBTable() -> creates a TFDTable instance' );
     Add( 'CreateDBQuery() -> creates a TFDQuery instance' );
   end;
   with Errors.Add do begin
     Name := 'DBError';
     ErrorType := etClass;     // <- !!! Must ...
   end;
 end;
end;

procedure TMain.PageControlChange(Sender: TObject);
begin
 if PageControl.TabIndex = 1 then begin
   // Example2
   if not tblCustomer.Active then
     tblCustomer.Active := True;
 end
 else begin
   if tblCustomer.Active then
     tblCustomer.Active := False;
 end;
end;

procedure TMain.btnExecuteExample1Click(Sender: TObject);
var
 l_sConnName: String;
begin
 l_sConnName := cobxConnSQLServer.Items[cobxConnSQLServer.ItemIndex];
 if self.DBConnectionClosedCheck(l_sConnName) then begin
   with GetPythonEngine do begin
     ExecStrings( SynEditScript1.Lines );
   end;
 end;
end;

procedure TMain.btnExecuteExample2Click(Sender: TObject);
var
 pyObj : PPyObject;
begin
  //  Instantiate a new Python object TPyTable
  pyObj := PyDelphiWrapper.Wrap(tblCustomer);
  with GetPythonEngine do begin
    // Define a new variable "T" in the DB module
    modDBFireDac.SetVar( 'T', pyObj );
    Py_XDecRef(pyObj);
    // Excecute the script
    ExecStrings( SynEditScript2.Lines );
  end;
end;

procedure TMain.btnExecuteExample3Click(Sender: TObject);
var
 pyObj : PPyObject;
 l_oTable: TFDTable;
begin
  // connect the Datasource2 to the Python Table
  l_oTable := TFDTable.Create(Self);
  l_oTable.TableName := 'Customer';
  l_oTable.Connection := Connection;
  Datasource2.Dataset := l_oTable;
  l_oTable.Open();

  pyObj := PyDelphiWrapper.Wrap(l_oTable, soOwned);
  with GetPythonEngine do begin
     // Define a new variable "T" in the DB module
    modDBFireDac.SetVar( 'T', pyObj );
    Py_XDecRef(pyObj);
    // Excecute the script
    ExecStrings( SynEditScript3.Lines );
  end;
end;

procedure TMain.btnExecuteExample4Click(Sender: TObject);
var
 l_sConnName: String;
begin
 l_sConnName := cobxConnSQLServer.Items[cobxConnSQLServer.ItemIndex];
 if self.DBConnectionClosedCheck(l_sConnName) then begin
   with PythonEngine do
     ExecStrings( SynEditScript4.Lines );
 end;
end;

initialization
  ReportMemoryLeaksOnShutdown := True;
finalization
  CheckSynchronize;
end.

