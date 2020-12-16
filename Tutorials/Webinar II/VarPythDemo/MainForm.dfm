object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple P4D Demo'
  ClientHeight = 599
  ClientWidth = 1049
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 345
    Width = 1049
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ResizeStyle = rsUpdate
    ExplicitTop = 201
    ExplicitWidth = 383
  end
  object sePythonCode: TSynEdit
    Left = 0
    Top = 17
    Width = 1049
    Height = 328
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Highlighter = SynPythonSyn
    Lines.Strings = (
      'from delphi_module import np_array'
      'print("type(np_array) = ", type(np_array))'
      'print("len(np_array) = ", len(np_array))'
      'print("np_array = ", np_array)'
      ''
      'res_array = np_array.copy()'
      'for i in range(len(np_array)):'
      '    res_array[i] *= np_array[i]'
      'print("res_array = ", res_array)'
      ' ')
    ExplicitWidth = 957
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 0
    Width = 1049
    Height = 17
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Sections = <
      item
        Alignment = taCenter
        AutoSize = True
        ImageIndex = -1
        Text = 'Python Source code'
        Width = 1049
      end>
    ParentFont = False
    ExplicitWidth = 957
  end
  object Panel1: TPanel
    Left = 0
    Top = 558
    Width = 1049
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitLeft = 1
    ExplicitTop = 563
    object btnRun: TButton
      Left = 24
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
    object Button1: TButton
      Left = 120
      Top = 5
      Width = 89
      Height = 25
      Caption = 'Python Version'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 348
    Width = 1049
    Height = 210
    Align = alClient
    TabOrder = 3
    ExplicitTop = 347
    object Splitter2: TSplitter
      Left = 833
      Top = 18
      Height = 191
      ExplicitLeft = 520
      ExplicitTop = 56
      ExplicitHeight = 100
    end
    object HeaderControl2: THeaderControl
      Left = 1
      Top = 1
      Width = 1047
      Height = 17
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      Sections = <
        item
          Alignment = taCenter
          AutoSize = True
          ImageIndex = -1
          Text = 'Python Output'
          Width = 1047
        end>
      ParentFont = False
      ExplicitWidth = 955
    end
    object mePythonOutput: TMemo
      Left = 1
      Top = 18
      Width = 832
      Height = 191
      Align = alLeft
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object ListBox: TListBox
      Left = 836
      Top = 18
      Width = 212
      Height = 191
      Align = alClient
      ItemHeight = 13
      TabOrder = 2
      ExplicitLeft = 752
      ExplicitWidth = 296
    end
  end
  object SynPythonSyn: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 760
    Top = 32
  end
  object SynEditPythonBehaviour: TSynEditPythonBehaviour
    Editor = sePythonCode
    Left = 760
    Top = 80
  end
  object PythonEngine: TPythonEngine
    IO = PythonGUIInputOutput
    Left = 880
    Top = 32
  end
  object PythonGUIInputOutput: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = mePythonOutput
    Left = 880
    Top = 80
  end
  object PythonModule: TPythonModule
    Engine = PythonEngine
    ModuleName = 'delphi_module'
    Errors = <>
    Left = 888
    Top = 144
  end
end
