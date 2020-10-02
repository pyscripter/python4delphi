object ThreadSortForm: TThreadSortForm
  Left = 171
  Top = 19
  Caption = 'Thread Sorting Demo'
  ClientHeight = 510
  ClientWidth = 559
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
  DesignSize = (
    559
    510)
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
  object StartBtn: TButton
    Left = 456
    Top = 384
    Width = 97
    Height = 25
    Caption = 'three interpreters'
    TabOrder = 0
    OnClick = StartBtnClick
  end
  object PythonMemo: TMemo
    Left = 8
    Top = 407
    Width = 545
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
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
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Start2Btn: TButton
    Left = 344
    Top = 384
    Width = 97
    Height = 25
    Caption = 'one interpreter'
    TabOrder = 2
    OnClick = Start2BtnClick
  end
  object LoadBtn: TButton
    Left = 120
    Top = 384
    Width = 97
    Height = 25
    Caption = 'Load Script'
    TabOrder = 3
    OnClick = LoadBtnClick
  end
  object SaveBtn: TButton
    Left = 232
    Top = 384
    Width = 97
    Height = 25
    Caption = 'Save Script'
    TabOrder = 4
    OnClick = SaveBtnClick
  end
  object PythonEngine1: TPythonEngine
    InitThreads = True
    RedirectIO = False
    Left = 16
    Top = 32
  end
  object PythonDialog: TOpenDialog
    DefaultExt = 'py'
    Filter = 'Python scripts|*.py|All files|*.*'
    Left = 72
    Top = 80
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'py'
    Filter = 'Python scripts|*.py|All files|*.*'
    Left = 16
    Top = 80
  end
  object SortModule: TPythonModule
    Engine = PythonEngine1
    OnInitialization = SortModuleInitialization
    ModuleName = 'SortModule'
    Errors = <>
    Left = 64
    Top = 32
  end
end
