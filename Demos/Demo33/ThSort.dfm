object ThreadSortForm: TThreadSortForm
  Left = 171
  Top = 19
  BorderStyle = bsDialog
  Caption = 'Thread Sorting Demo'
  ClientHeight = 564
  ClientWidth = 566
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 24
    Width = 177
    Height = 355
  end
  object Bevel3: TBevel
    Left = 376
    Top = 24
    Width = 177
    Height = 355
  end
  object Bevel2: TBevel
    Left = 192
    Top = 24
    Width = 177
    Height = 355
  end
  object BubbleSortBox: TPaintBox
    Left = 8
    Top = 24
    Width = 177
    Height = 355
    OnPaint = BubbleSortBoxPaint
  end
  object SelectionSortBox: TPaintBox
    Left = 192
    Top = 24
    Width = 177
    Height = 355
    OnPaint = SelectionSortBoxPaint
  end
  object QuickSortBox: TPaintBox
    Left = 376
    Top = 24
    Width = 177
    Height = 355
    OnPaint = QuickSortBoxPaint
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 49
    Height = 13
    Caption = 'SortFunc1'
  end
  object Label2: TLabel
    Left = 192
    Top = 8
    Width = 49
    Height = 13
    Caption = 'SortFunc2'
  end
  object Label3: TLabel
    Left = 376
    Top = 8
    Width = 49
    Height = 13
    Caption = 'SortFunc3'
  end
  object Start3Btn: TButton
    Left = 334
    Top = 385
    Width = 108
    Height = 25
    Caption = 'three interpreters'
    TabOrder = 0
    OnClick = Start3BtnClick
  end
  object PythonMemo: TMemo
    Left = 0
    Top = 419
    Width = 566
    Height = 145
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'from SortModule import getvalue,swap'
      ' '
      'def SortFunc1(handle, low, high):'
      '  for i in range(low, high):'
      '    for j in range(low+1,high):'
      '      if getvalue(handle,j-1) > getvalue(handle,j):'
      '        swap(handle,j-1,j)'
      ''
      'def SortFunc2(handle,low,high):'
      '  for i in range(low,high-1):'
      '    for j in range(i+1,high):'
      '      if getvalue(handle,i) > getvalue(handle,j):'
      '        swap(handle,i,j)'
      ''
      'def SortFunc3(handle,low,high):'
      '  Lo = low'
      '  Hi = high-1'
      '  Mid = getvalue(handle,(Lo+Hi) // 2)'
      '  while 1:'
      '    while getvalue(handle,Lo) < Mid: '
      '      Lo = Lo + 1'
      '    while getvalue(handle,Hi) > Mid:'
      '      Hi = Hi - 1'
      '    if Lo <= Hi:'
      '      swap(handle,Lo,Hi)'
      '      Lo = Lo + 1'
      '      Hi = Hi - 1'
      '    if Lo > Hi:'
      '      break'
      '  if Hi > low:'
      '    SortFunc3(handle,low,Hi+1)'
      '  if Lo < high-1:'
      '    SortFunc3(handle,Lo,high)')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Start1Btn: TButton
    Left = 224
    Top = 385
    Width = 104
    Height = 25
    Caption = 'one interpreter'
    TabOrder = 2
    OnClick = Start1BtnClick
  end
  object LoadBtn: TButton
    Left = 8
    Top = 384
    Width = 97
    Height = 25
    Caption = 'Load Script'
    TabOrder = 3
    OnClick = LoadBtnClick
  end
  object SaveBtn: TButton
    Left = 111
    Top = 385
    Width = 107
    Height = 25
    Caption = 'Save Script'
    TabOrder = 4
    OnClick = SaveBtnClick
  end
  object StopBtn: TButton
    Left = 448
    Top = 385
    Width = 105
    Height = 25
    Caption = 'stop interpreters'
    TabOrder = 5
    OnClick = StopBtnClick
  end
  object PythonEngine1: TPythonEngine
    InitThreads = True
    PyFlags = [pfDebug, pfInteractive, pfVerbose]
    RedirectIO = False
    Left = 16
    Top = 88
  end
  object SortModule: TPythonModule
    Engine = PythonEngine1
    OnInitialization = SortModuleInitialization
    ModuleName = 'SortModule'
    Errors = <>
    Left = 64
    Top = 88
  end
  object PythonDialog: TOpenDialog
    DefaultExt = 'py'
    Filter = 'Python scripts|*.py|All files|*.*'
    Left = 64
    Top = 40
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'py'
    Filter = 'Python scripts|*.py|All files|*.*'
    Left = 16
    Top = 40
  end
end
