object Main: TMain
  Left = 153
  Top = 148
  Width = 769
  Height = 561
  VertScrollBar.Range = 356
  ActiveControl = Memo1
  Caption = 'VarPyth unit tests'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 249
    Width = 753
    Height = 5
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 761
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 753
    Height = 249
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 420
    Width = 753
    Height = 102
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 616
      Top = 59
      Width = 24
      Height = 13
      Caption = 'times'
    end
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 86
      Width = 753
      Height = 16
      Align = alBottom
      TabOrder = 15
      Visible = False
    end
    object btnExecuteScript: TButton
      Left = 8
      Top = 8
      Width = 89
      Height = 25
      Caption = 'E&xecute Script'
      TabOrder = 0
      OnClick = btnExecuteScriptClick
    end
    object btnTestIntegers: TButton
      Left = 128
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test Integers'
      TabOrder = 1
      OnClick = btnTestIntegersClick
    end
    object btnTestFloats: TButton
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test Floats'
      TabOrder = 2
      OnClick = btnTestFloatsClick
    end
    object btnTestSequences: TButton
      Left = 392
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Test Sequences'
      TabOrder = 3
      OnClick = btnTestSequencesClick
    end
    object btnTestMappings: TButton
      Left = 488
      Top = 8
      Width = 89
      Height = 25
      Caption = 'Test Mappings'
      TabOrder = 4
      OnClick = btnTestMappingsClick
    end
    object btnTestObjects: TButton
      Left = 664
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test Objects'
      TabOrder = 5
      OnClick = btnTestObjectsClick
    end
    object btnTestStrings: TButton
      Left = 304
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test Strings'
      TabOrder = 6
      OnClick = btnTestStringsClick
    end
    object cbIntegers: TCheckBox
      Left = 128
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
    object cbFloats: TCheckBox
      Left = 216
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object cbStrings: TCheckBox
      Left = 304
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 9
    end
    object cbSequences: TCheckBox
      Left = 392
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 10
    end
    object cbMappings: TCheckBox
      Left = 488
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object cbObjects: TCheckBox
      Left = 664
      Top = 33
      Width = 73
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 12
    end
    object btnRunSelectedTests: TButton
      Left = 128
      Top = 56
      Width = 121
      Height = 25
      Caption = 'Run selected tests once'
      TabOrder = 13
      OnClick = btnRunSelectedTestsClick
    end
    object edtTestCount: TEdit
      Left = 568
      Top = 56
      Width = 41
      Height = 21
      TabOrder = 14
      Text = '1000'
    end
    object btnRunNTimes: TButton
      Left = 400
      Top = 56
      Width = 163
      Height = 25
      Caption = 'Run selected tests n times'
      TabOrder = 16
      OnClick = btnRunNTimesClick
    end
    object btnTestDates: TButton
      Left = 584
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Test Dates'
      TabOrder = 18
      OnClick = btnTestDatesClick
    end
    object cbDates: TCheckBox
      Left = 584
      Top = 33
      Width = 77
      Height = 17
      Caption = 'Included'
      Checked = True
      State = cbChecked
      TabOrder = 17
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 254
    Width = 753
    Height = 166
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      'class XYZ(object):'
      '  pass'
      ''
      'class Foo:'
      '  def __init__(Self, Value=0):'
      '    Self.Value = Value'
      '  def __del__(Self):'
      '    print ("delete", Self)'
      '  def __add__(self, other):'
      '    return Foo(self.Value + other.Value)'
      '  def Inc(Self, AValue = 1):'
      '    Self.Value = Self.Value + AValue'
      '  def GetSelf(Self):'
      '    return Self'
      '  def GetValue(Self):'
      '    return Self.Value'
      '  def SetABC(Self, A, B, C):'
      '    Self.A = A'
      '    Self.B = B'
      '    Self.C = C'
      '  def Add(Self, AFooInst):'
      '    Self.Value = Self.Value + AFooInst.Value'
      'class Bar(Foo):'
      '  def Inc(Self, AValue = 1):'
      '    Self.Value = Self.Value - AValue'
      'def Add(a, b):'
      '  return a + b'
      'def MakeList(a, b, c, d):'
      '  return [a, b, c, d]'
      ''
      'f = Foo()'
      'print ("Created", f)'
      'f.Inc()'
      'f.Inc(2)'
      'b = Bar()'
      'b.Inc()'
      'b.Inc(2)')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 192
    Top = 48
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = False
    RawOutput = False
    Output = Memo1
    Left = 232
    Top = 48
  end
end
