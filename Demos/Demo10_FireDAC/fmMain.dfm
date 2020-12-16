object Main: TMain
  Left = 142
  Top = 147
  Caption = 'Demo 10 : DB with FireDac API'
  ClientHeight = 660
  ClientWidth = 904
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 496
    Width = 904
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 289
    ExplicitWidth = 688
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 503
    Width = 904
    Height = 157
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    Zoom = 100
  end
  object Panel6: TPanel
    Left = 0
    Top = 0
    Width = 904
    Height = 41
    Align = alTop
    TabOrder = 1
    object Label5: TLabel
      Left = 40
      Top = 16
      Width = 46
      Height = 13
      Caption = 'Database'
    end
    object btnSQLTest: TButton
      Left = 584
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Test'
      TabOrder = 0
      OnClick = btnSQLTestClick
    end
    object cobxConnSQLServer: TComboBox
      Left = 111
      Top = 12
      Width = 443
      Height = 21
      TabOrder = 1
    end
  end
  object PageControl: TPageControl
    Left = 0
    Top = 41
    Width = 904
    Height = 455
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 2
    OnChange = PageControlChange
    object TabSheet1: TTabSheet
      Caption = 'Example 1'
      object Splitter7: TSplitter
        Left = 497
        Top = 0
        Height = 427
        ExplicitLeft = 1
        ExplicitTop = 1
        ExplicitHeight = 261
      end
      object SynEditScript1: TSynEdit
        Left = 0
        Top = 0
        Width = 497
        Height = 427
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 0
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.RightOffset = 15
        Gutter.ShowLineNumbers = True
        Highlighter = SynPythonSyn
        Lines.Strings = (
          '')
      end
      object Panel7: TPanel
        Left = 500
        Top = 0
        Width = 396
        Height = 427
        Align = alClient
        TabOrder = 1
        object Label1: TLabel
          Left = 10
          Top = 34
          Width = 379
          Height = 65
          Caption = 
            'Example 1 shows you how to create your own TFDTable object and w' +
            'ork with it.'#13#10#13#10'Example 2 shows you how to create a TFDTable obj' +
            'ect connected to an'#13#10'     already created Delphi TFDTable (which' +
            ' may include calculated fields'#13#10'     and the like).'
        end
        object Label2: TLabel
          Left = 10
          Top = 126
          Width = 372
          Height = 52
          Caption = 
            'Example 3 shows you how to connect a Python Table to a Delphi Da' +
            'tasource.'#13#10#13#10'Example 4 show you how to create your own TFDQuery ' +
            'object and work with it'#13#10
        end
        object btnExecuteExample1: TButton
          Left = 272
          Top = 3
          Width = 75
          Height = 25
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnExecuteExample1Click
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Example 2'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Splitter3: TSplitter
        Left = 497
        Top = 0
        Height = 427
        ExplicitLeft = 257
        ExplicitHeight = 261
      end
      object Panel1: TPanel
        Left = 500
        Top = 0
        Width = 90
        Height = 427
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object btnExecuteExample2: TButton
          Left = 6
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnExecuteExample2Click
        end
      end
      object DBGrid1: TDBGrid
        Left = 590
        Top = 0
        Width = 306
        Height = 427
        Align = alClient
        DataSource = dsrcCustomer
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object SynEditScript2: TSynEdit
        Left = 0
        Top = 0
        Width = 497
        Height = 427
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 2
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynPythonSyn
        Lines.Strings = (
          '')
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Example 3'
      object Splitter4: TSplitter
        Left = 497
        Top = 0
        Height = 427
        ExplicitLeft = 257
        ExplicitHeight = 261
      end
      object Panel3: TPanel
        Left = 500
        Top = 0
        Width = 90
        Height = 427
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object btnExecuteExample3: TButton
          Left = 6
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnExecuteExample3Click
        end
      end
      object DBGrid2: TDBGrid
        Left = 590
        Top = 0
        Width = 306
        Height = 427
        Align = alClient
        DataSource = DataSource2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object SynEditScript3: TSynEdit
        Left = 0
        Top = 0
        Width = 497
        Height = 427
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 2
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynPythonSyn
        Lines.Strings = (
          '')
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Example 4'
      object Splitter2: TSplitter
        Left = 497
        Top = 0
        Height = 427
        ExplicitLeft = 257
        ExplicitHeight = 261
      end
      object Panel2: TPanel
        Left = 500
        Top = 0
        Width = 396
        Height = 427
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object btnExecuteExample4: TButton
          Left = 16
          Top = 8
          Width = 75
          Height = 25
          Caption = 'Execute'
          TabOrder = 0
          OnClick = btnExecuteExample4Click
        end
      end
      object SynEditScript4: TSynEdit
        Left = 0
        Top = 0
        Width = 497
        Height = 427
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Courier New'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 1
        UseCodeFolding = False
        Gutter.Font.Charset = DEFAULT_CHARSET
        Gutter.Font.Color = clWindowText
        Gutter.Font.Height = -11
        Gutter.Font.Name = 'Courier New'
        Gutter.Font.Style = []
        Gutter.ShowLineNumbers = True
        Highlighter = SynPythonSyn
        Lines.Strings = (
          '')
        Options = [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
      end
    end
  end
  object PythonGUIInputOutput: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = RichEdit1
    Left = 88
    Top = 240
  end
  object modDBFireDac: TPythonModule
    Engine = PythonEngine
    OnInitialization = modDBFireDacInitialization
    ModuleName = 'DBFireDac'
    Errors = <>
    Left = 192
    Top = 240
  end
  object dsrcCustomer: TDataSource
    DataSet = tblCustomer
    Left = 432
    Top = 120
  end
  object DataSource2: TDataSource
    Left = 432
    Top = 72
  end
  object Connection: TFDConnection
    Params.Strings = (
      'DriverID=SQLite'
      'Database=Demo.s3db'
      'CharacterSet=UTF8')
    LoginPrompt = False
    Left = 432
    Top = 168
  end
  object mqSrcTables: TFDMetaInfoQuery
    Connection = Connection
    TableKinds = [tkTable]
    ObjectScopes = [osMy, osOther]
    Left = 352
    Top = 72
  end
  object SynPythonSyn: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 76
    Top = 73
  end
  object PythonEngine: TPythonEngine
    IO = PythonGUIInputOutput
    Left = 84
    Top = 289
  end
  object tblCustomer: TFDTable
    IndexFieldNames = 'CUSTNO'
    Connection = Connection
    UpdateOptions.UpdateTableName = 'CUSTOMER'
    TableName = 'CUSTOMER'
    Left = 356
    Top = 121
  end
  object PyDelphiWrapper: TPyDelphiWrapper
    Engine = PythonEngine
    Module = modDBFireDac
    Left = 192
    Top = 296
  end
end
