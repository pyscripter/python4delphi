object Form1: TForm1
  Left = 204
  Top = 124
  Width = 544
  Height = 375
  VertScrollBar.Range = 170
  ActiveControl = Button1
  Caption = 'Form1'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 129
    Width = 528
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 536
  end
  object Memo1: TMemo
    Left = 0
    Top = 132
    Width = 528
    Height = 163
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      'import sys'
      'print (sys.version_info)'
      'import demodll'
      'print (dir(demodll))'
      'print (demodll.add( 2, 2 ))')
    ParentFont = False
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 528
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ParentBackground = False
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 0
    Width = 528
    Height = 129
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      '')
    ParentFont = False
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    DllName = 'python37.dll'
    APIVersion = 1013
    RegVersion = '3.7'
    UseLastKnownVersion = False
    IO = PythonGUIInputOutput1
    Left = 16
    Top = 16
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 56
    Top = 16
  end
end
