unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PythonEngine, StdCtrls, ComCtrls, ExtCtrls, PythonGUIInputOutput, Db,
  DBTables, Grids, DBGrids;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Panel2: TPanel;
    Panel3: TPanel;
    RichEdit1: TRichEdit;
    Splitter1: TSplitter;
    Memo1: TMemo;
    DBGrid1: TDBGrid;
    Table1: TTable;
    DataSource1: TDataSource;
    PythonModule1: TPythonModule;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure PythonModule1AfterInitialization(Sender: TObject);
    procedure PythonModule1Initialization(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  function CreateTable( const DBName, TblName : String ) : TDataset;
  function DoCreateTable( Self, Args : PPyObject ) : PPyObject; cdecl; // Don't forget the cdecl keyword !!!


var
  Form1: TForm1;

implementation
uses pyDB;
{$R *.DFM}

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

procedure TForm1.PythonModule1AfterInitialization(Sender: TObject);
var
  ds : TDataset;
  obj : PPyObject;
begin
  // The table is created while Python initializes our modules.
  // But we could do it just before the ExecStrings method.
  with Sender as TPythonModule do
    begin
      // Create our TDataset subclass
      ds := CreateTable( 'DBDemos', 'Animals.dbf' );
      // Use it in the Datasource of the DBGrid
      Datasource1.Dataset := ds;
      // Create a Python object that will let your script access your table
      obj := NewDataset(ds, True);
      // Put the Python object in a module var called 'Dataset'
      // To access this var in your script, import the module and prefix the var name with
      // the module name:
      //   import MyModule
      //   print MyModule.Dataset.Active
      PythonModule1.SetVar( 'Dataset', obj );
      // Don't forget to decrement the object, because SetVar has incremented it,
      // in order to acquire property.
      GetPythonEngine.Py_XDecRef(obj);
    end;
end;



procedure TForm1.PythonModule1Initialization(Sender: TObject);
begin
  // Here we register our new module functions
  with Sender as TPythonModule do
    AddMethod( 'CreateTable', DoCreateTable, 'CreateTable( DBName, TbleName ) -> Dataset' );
end;

function CreateTable( const DBName, TblName : String ) : TDataset;
var
  ds : TTable;
begin
  // Note that here we use a simple TTable, but replace the TTable with anything that
  // inherits from a TDataset.
  ds := TTable.Create(nil);
  ds.DatabaseName := DBName;
  ds.TableName := TblName;
  ds.Active := True;
  Result := ds;
end;

{
  This function will let your Python script create its own instances of your
  TDataset subclass !
}
function DoCreateTable( Self, Args : PPyObject ) : PPyObject; cdecl;
var
  DBName, TblName : PAnsiChar;
  ds : TDataset;
begin
  with GetPythonEngine do
    begin
      // Extract our arguments
      if PyArg_ParseTuple( args, 'ss:CreateTable',@DBName, @TblName ) <> 0 then
        begin
          try
            // Try to create our dataset
            ds := CreateTable( DBName, TblName );
            // Create our Python wrapper
            Result := NewDataset( ds, True );
          except
            on E : Exception do
              begin
                if Assigned(gDBModule) then
                  gDBModule.RaiseError( 'DBError', E.Message );
                Result := nil;
              end;
          end;
        end
      else // the arguments were not right
        Result := nil;
    end;
end;

end.
