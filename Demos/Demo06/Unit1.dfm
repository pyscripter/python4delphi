object Form1: TForm1
  Left = 233
  Top = 175
  Caption = 'Demo of Python'
  ClientHeight = 337
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 145
    Width = 528
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 536
  end
  object Memo1: TMemo
    Left = 0
    Top = 148
    Width = 528
    Height = 121
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'import sys'
      'import spam'
      'print (spam.foo('#39'hello world'#39', 1))'
      'p = spam.CreatePoint( 10, 25 )'
      'print ("Point:", p)'
      'p.x = 58'
      'print (p.x, p)'
      'p.OffsetBy( 5, 5 )'
      'print (p)'
      'print ("Current value of var test is: ", test)'
      'test.Value = "New value set by Python"'
      'print ("getdouble: ", spam.getdouble())')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 269
    Width = 528
    Height = 68
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
    object Button4: TButton
      Left = 368
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Show var test'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Edit1: TEdit
      Left = 368
      Top = 32
      Width = 161
      Height = 21
      TabOrder = 4
      Text = 'Edit1'
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 0
    Width = 528
    Height = 145
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Memo2')
    ParentFont = False
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    InitScript.Strings = (
      'import sys'
      'print ("Python Dll: ", sys.version)'
      'print (sys.copyright)'
      'print')
    IO = PythonGUIInputOutput1
    Left = 8
  end
  object PythonType1: TPythonType
    Engine = PythonEngine1
    OnInitialization = PythonType1Initialization
    TypeName = 'Point'
    Prefix = 'Create'
    Services.Basic = [bsGetAttr, bsSetAttr, bsRepr, bsStr]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = []
    Left = 72
  end
  object PythonModule1: TPythonModule
    Engine = PythonEngine1
    OnInitialization = PythonModule1Initialization
    ModuleName = 'spam'
    Errors = <>
    Left = 104
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
  object PythonDelphiVar1: TPythonDelphiVar
    Engine = PythonEngine1
    Module = '__main__'
    VarName = 'test'
    OnGetData = PythonDelphiVar1GetData
    OnSetData = PythonDelphiVar1SetData
    OnChange = PythonDelphiVar1Change
    Left = 136
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 40
  end
end
