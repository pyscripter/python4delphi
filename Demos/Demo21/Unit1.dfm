object Form1: TForm1
  Left = 237
  Top = 135
  Caption = 'Demo of Python'
  ClientHeight = 337
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 140
    Width = 528
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Memo1: TMemo
    Left = 0
    Top = 143
    Width = 528
    Height = 150
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'import spam'
      'print (spam.foo())'
      'print (spam.foo.__doc__)'
      ''
      'P = spam.CreatePoint(2,3)'
      'print (P)'
      'P.OffsetBy(2, 2)'
      'print (P)')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 293
    Width = 528
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 6
      Top = 8
      Width = 115
      Height = 25
      Caption = 'Execute script'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 168
      Top = 8
      Width = 91
      Height = 25
      Caption = 'Load script...'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 264
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Save script...'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 0
    Width = 528
    Height = 140
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 32
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.py'
    Filter = 'Python files|*.py|Text files|*.txt|All files|*.*'
    Left = 176
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.py'
    Filter = 'Python files|*.py|Text files|*.txt|All files|*.*'
    Left = 208
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 64
  end
  object PythonModule1: TPythonModule
    Engine = PythonEngine1
    Events = <
      item
        Name = 'foo'
        OnExecute = PythonModule1Events0Execute
        DocString.Strings = (
          'Hello !'
          'This is a doc string'
          'of several lines...')
      end>
    ModuleName = 'spam'
    Errors = <>
    Left = 112
    Top = 64
  end
  object PythonType1: TPythonType
    Engine = PythonEngine1
    OnInitialization = PythonType1Initialization
    Events = <
      item
        Name = 'OffsetBy'
        OnExecute = PythonType1Events0Execute
      end>
    TypeName = 'Point'
    Prefix = 'Create'
    Module = PythonModule1
    Services.Basic = [bsRepr, bsStr, bsGetAttrO, bsSetAttrO]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = []
    Left = 192
    Top = 64
  end
end
