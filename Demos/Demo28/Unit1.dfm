object Form1: TForm1
  Left = 241
  Top = 155
  Width = 597
  Height = 557
  VertScrollBar.Range = 200
  ActiveControl = Button1
  Caption = 'Demo of Python'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 209
    Width = 581
    Height = 3
    Cursor = crVSplit
    Align = alTop
    Color = clBtnFace
    ParentColor = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 212
    Width = 581
    Height = 262
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      'import p4d'
      'L = p4d.CreateTStringList(1, 2, 3, 4)'
      'print (L)'
      'for i in L:'
      '  print (i, type(i))'
      ''
      'i = iter(L)'
      'print (i)'
      'try:'
      '  while True:'
      '    print (i.next())'
      'except StopIteration:'
      '  print ("Done")'
      ''
      'print (L[2])'
      'L[2] = int(L[2]) * 2'
      'print (L[2], type(L[2]))'
      'print (L[ L.add(10) ])')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 474
    Width = 581
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
    Width = 581
    Height = 209
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
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
    Title = 'Open'
    Left = 176
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.py'
    Filter = 'Python files|*.py|Text files|*.txt|All files|*.*'
    Title = 'Save As'
    Left = 208
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 64
  end
  object ptStringList: TPythonType
    Engine = PythonEngine1
    OnCreate = ptStringListCreate
    TypeName = 'TStringList'
    Prefix = 'Create'
    Module = pmP4D
    Services.Basic = [bsRepr, bsStr, bsGetAttrO, bsSetAttrO, bsIter]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = [ssLength, ssItem, ssAssItem]
    Services.Mapping = []
    Left = 64
    Top = 48
  end
  object pmP4D: TPythonModule
    Engine = PythonEngine1
    ModuleName = 'p4d'
    Errors = <>
    Left = 32
    Top = 48
  end
  object ptStringListIterator: TPythonType
    Engine = PythonEngine1
    OnCreate = ptStringListIteratorCreate
    TypeName = 'TStringListIterator'
    Prefix = 'Create'
    Module = pmP4D
    Services.Basic = [bsRepr, bsStr, bsGetAttrO, bsSetAttrO, bsIter, bsIterNext]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = []
    Left = 64
    Top = 80
  end
end
