object Form1: TForm1
  Left = 192
  Top = 114
  Caption = 'Form1'
  ClientHeight = 601
  ClientWidth = 854
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 56
    Top = 24
    Width = 745
    Height = 281
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 56
    Top = 320
    Width = 745
    Height = 217
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'import test'
      'S=test.CreateMySeq()'
      'print (S)'
      'print (len(S))'
      'print (S[1])'
      'print (S[1,2])'
      'print (S[1:2])'
      'print (S[1:20:2])'
      'print (S[...])'
      'print (S[1,4,5:8, 10:20:2, ...])')
    ParentFont = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 56
    Top = 560
    Width = 201
    Height = 25
    Caption = 'Execute'
    TabOrder = 2
    OnClick = Button1Click
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 40
    Top = 144
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo1
    Left = 40
    Top = 176
  end
  object PythonModule1: TPythonModule
    Engine = PythonEngine1
    ModuleName = 'test'
    Errors = <>
    Left = 40
    Top = 216
  end
  object PythonType1: TPythonType
    Engine = PythonEngine1
    OnInitialization = PythonType1Initialization
    TypeName = 'MySeq'
    Prefix = 'Create'
    Module = PythonModule1
    Services.Basic = [bsGetAttrO, bsSetAttrO, bsRepr, bsStr]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = [msLength, msSubscript]
    Left = 40
    Top = 248
  end
end
