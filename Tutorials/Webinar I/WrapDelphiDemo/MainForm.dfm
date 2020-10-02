object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'PyDelphiWrapper Demo'
  ClientHeight = 655
  ClientWidth = 983
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
    Top = 401
    Width = 983
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
    Width = 983
    Height = 384
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
      'from delphi_module import delphi_functions'
      'from timeit import Timer'
      'import math'
      ''
      'def test():'
      '    max_n = 1000000'
      
        '    print(f'#39'Number of primes between 0 and {max_n} = {delphi_fun' +
        'ctions.count_primes(max_n)}'#39')'
      ''
      'def main():'
      '    print(f'#39'Elapsed time: {Timer(stmt=test).timeit(1)} secs'#39')'
      ''
      'if __name__ == '#39'__main__'#39':'
      '    main()')
  end
  object HeaderControl1: THeaderControl
    Left = 0
    Top = 0
    Width = 983
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
        Width = 983
      end>
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 614
    Width = 983
    Height = 41
    Align = alBottom
    TabOrder = 2
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
    Top = 404
    Width = 983
    Height = 210
    Align = alClient
    TabOrder = 3
    object HeaderControl2: THeaderControl
      Left = 1
      Top = 1
      Width = 981
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
          Width = 981
        end>
      ParentFont = False
    end
    object mePythonOutput: TMemo
      Left = 1
      Top = 18
      Width = 981
      Height = 191
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
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
  object PyDelphiWrapper: TPyDelphiWrapper
    Engine = PythonEngine
    Module = PythonModule
    Left = 880
    Top = 192
  end
  object PythonModule: TPythonModule
    Engine = PythonEngine
    ModuleName = 'delphi_module'
    Errors = <>
    Left = 880
    Top = 136
  end
end
