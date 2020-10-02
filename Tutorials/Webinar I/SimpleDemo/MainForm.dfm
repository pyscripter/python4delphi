object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Simple P4D Demo'
  ClientHeight = 599
  ClientWidth = 957
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 345
    Width = 957
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
    Width = 957
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
    ExplicitWidth = 983
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 0
    Width = 957
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
        Width = 957
      end>
    ParentFont = False
    ExplicitWidth = 983
  end
  object Panel1: TPanel
    Left = 0
    Top = 558
    Width = 957
    Height = 41
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 614
    ExplicitWidth = 983
    object btnRun: TButton
      Left = 24
      Top = 5
      Width = 75
      Height = 25
      Caption = 'Run'
      TabOrder = 0
      OnClick = btnRunClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 348
    Width = 957
    Height = 210
    Align = alClient
    TabOrder = 3
    ExplicitTop = 404
    ExplicitWidth = 983
    object HeaderControl2: THeaderControl
      Left = 1
      Top = 1
      Width = 955
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
          Width = 955
        end>
      ParentFont = False
      ExplicitWidth = 981
    end
    object mePythonOutput: TMemo
      Left = 1
      Top = 18
      Width = 955
      Height = 191
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      ExplicitWidth = 981
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
end
